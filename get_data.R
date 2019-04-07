############################
##   GET Wikipedia Data   ##
############################

# Functions in this blick: 

# construct_API_call
#  -  Input:  name of person to look up
#  -  Output:  web address, that can be called to grab the person's data

# call_API 
#  - Input: web address of person to look up
#  - Output: wikitext of person

# get_infobox
#  - Input: wikitext of person from call_API
#  - Output: raw infobox segment from that wikitext

# parse_infobox_into_list 
#  - input: raw infobox from get_infobox
#  - output: infobox arranged as a lsit, but with no cleaning or anything

# add_person_to_db
#  - Input: output of parse_infobox_into_list
#  - Output: none.
#  - Side effectL input list is appended to raw_person_db

# GET_wiki_data 
# - input: person's name
#  - Output: none.
#  - Side effect: adds person's parsed infobox to the person_db
#  - NOTE: calls the above functions, in order
#  - ON FAILURE: return raw name, wiki address attemted in the call, wikitext if any



# construct_API_call ------------------------------------------------------

# constructs the call to wikipedia API to grab the text of a given person's wikipedia page
#  -  Input:  name of person to look up
#  -  Output:  web address, that can be called to grab the person's data

construct_API_call <- function(name) {
  #replace any spaces in the name. if so, replace with '_'
  name <- str_replace_all(name, " ", "_")
  paste0("https://en.wikipedia.org/w/index.php?action=raw&title=",
         name)
}


# call_API ----------------------------------------------------------------

# calls the wikipedia api to get the text data of the person
#  - Input: web address of person to look up
#  - Output: wikitext of person

call_API <- function(API_address) {
  read_lines(API_address)
  # TODO add in error messages
  # nececary error message will become evident as project continues, and will be added on
}


# get_infobox -------------------------------------------------------------

# pulls the infobox out of the raw wikitext.
# throws an error if there is no infobox
#   NOTE: might need to edit the error to outptu something else, without erroring, so the overall function covers it instead

get_infobox <- function(wiki_text) {
  collapsed_text <- paste(wiki_text, collapse = "") #collapse from a vector to one string
  if (!str_detect(collapsed_text, "Infobox")) { return("'Infobox' not located in wiki_text input") } 

  #loop from the end of '{{Infobox' to the closing '}}', skipping over '}}' that do not close it
  open_bracket_layer <- 2 # the initial '{{'
  #loc. of the start of the string
  start_infobox_index <- str_locate(collapsed_text, "\\{\\{Infobox ")
  
  text_vector <- unlist(str_split(collapsed_text, ""))
  
  for (i in start_infobox_index[2]:length(text_vector)) { #check over the length of the wiki text}
    symbol_at_index_i <- text_vector[i]
    
    if (symbol_at_index_i == "{") {
      open_bracket_layer <- open_bracket_layer + 1
    } else if (symbol_at_index_i == "}") {
      open_bracket_layer <- open_bracket_layer - 1
      if (open_bracket_layer == 0) {
        end_infobox_index <- i
        break 
      }
    }
  }
  return(substr(collapsed_text, start_infobox_index[1], end_infobox_index))  
}



# parse_infobox_into_list -------------------------------------------------

#very basic parsing, jsut making a list without modifying the conctents of the list elements
#  - input: raw infobox from get_infobox
#  - output: infobox arranged as a lsit, but with no cleaning or anything
#  assunes '{{Infobox' exists in the text, since get_infobox checked for it first

parse_infobox_into_list <- function(infobox_str) {
  #Trim the opening '{{' and closing'}}' marks
  infobox_str <- substr(infobox_str, 3, nchar(infobox_str) - 2)
  
  #break up the string by the '|' delimiters
  infobox_vector <- NULL
  layer_opener <- c("[", "{", "<")
  layer_closer <- c("]", "}", ">")
  start_str <- 1
  
  layer <- 0 # how many '[', '{', or'<' in currently
  for (i in 1:nchar(infobox_str)) {
    symbol_at_index_i <- substr(infobox_str, i, i) 
    
    #figure out how many layers in it is currently
    if (symbol_at_index_i %in% layer_opener) {
      layer = layer + 1
    } else if (symbol_at_index_i %in% layer_closer) {
      layer = layer - 1
    }
    
    #if outside any layer, check if "|"
    #then append that section to infobox_vector
    if (layer == 0 & symbol_at_index_i == "|") {
      infobox_vector <- append(infobox_vector, substr(infobox_str, start_str, i - 1))
      #set new start of current data string to after the current '|' character
      start_str <- i + 1
    }
  }
  #add on the final data point, which would not end in a '|'
  infobox_vector <- 
    infobox_vector %>%
    append(substr(infobox_str, start_str, nchar(infobox_str))) %>%
    str_trim()
  #str_trim here?
  
  #part 2:convert the vector of strings into a list
  infobox_list <- list()
  
  # infobox_vector[1] is slightly different than the other elements. it is 'Infobox ___', and does not have the '=' symbol that all other lines should have
  infobox_list$infobox <- str_remove(infobox_vector[1], "Infobox ")
  
  # now go over all other lines and coerce to a list
  for (i in 2:length(infobox_vector)) {
    #split each line according to the '=' symbol
    current_ib_data <- 
      infobox_vector[i] %>%
      str_split("=", n = 2) %>% 
      unlist() %>% 
      str_trim()
    
    #add to the infobox list, using LHS as name and RHS as data content
    infobox_list[current_ib_data[1]] <- current_ib_data[2]
  }
  return(infobox_list)

}


# add_info_into_db ------------------------------------------------------

#adds the minimally parsed person data into a list of person data
#  - Input: output of parse_infobox_into_list
#  - Output: none.
#  - Side effectL input list is appended to raw_person_db
#TODO somewhere in the project there will need to be a statement like 'person_db <- list()'
raw_person_db <- list()
failed_person_db <- list() 
add_info_into_db <- function(infobox_list, name) {
  db_addition <- list(x = infobox_list)
  names(db_addition) <- name
  # '<<-' for that global assignment
  raw_person_db <<- append(person_db, db_addition)
}


# GET_wiki_data -----------------------------------------------------------

# basically the poinf of this file
# do all the functions above
# - input: person's name
#  - Output: none.
#  - Side effect: adds person's parsed infobox to the person_db
#  - NOTE: calls the above functions, in order
#  - ON FAILURE: return raw name, wiki address attemted in the call, wikitext if any

GET_wiki_data <- function(name) {
  API_call <- construct_API_call(name)
  raw_text <- call_API(API_call)
  infobox <- get_infobox(raw_text)
  if (infobox == "'Infobox' not located in wiki_text input") {
    add_info_into_db(list(API_call = API_call,
                          raw_text = raw_text,
                          collected_p = F),
                     name)
    warning("'Infobox' not located in wiki_text input")
  } else {
    parsed_infobox <- parse_infobox_into_list(infobox)
    parsed_infobox <- c(parsed_infobox, collected_p = T)
    add_info_into_db(parsed_infobox, name)
    return(parsed_infobox) # might delete later, if unnececary/annoying output
  }
}


# TEST SOME ---------------------------------------------------------------

GET_wiki_data("Egon Pearson")
GET_wiki_data("Isaac Newton")
GET_wiki_data("Bhaskar Kumar Ghosh")

glimpse(person_db)