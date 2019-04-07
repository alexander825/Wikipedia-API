#########################
##   Partial Parsing   ##
#########################


# This part will do parsing of only a couple properties of people, 
# just enough to parse newnames of people to collect, and verify that they should be collected from

# GOALS
# parse the following properties:
#    influences        doctoral_advisor  academic_advisors  influenced 
#    notable_students  name              known_for         field 
#    fields            infobox

#Parsing mostly involves identifying '[[___]]' boxes
# exept infobox and names, whcih are basically already cleaned


# Function List -----------------------------------------------------------

# GET_data_to_parse
#  pulls the data from raw_person_db
# and picks out the properties we plan to analyse, lsited up top of this file
#  - Input: name
#  - Output: person's list from raw_person_db, containing ony the properties we care about


# get_bracket_data
# takes the list element, and picks out all '[[___]]'
# and additionally trimms off the '[[' and ']]'
# input: a single prpoerty form a person, like raw_person_db$person$property
# output: a vector of all '___' located

# handle_vertical_bars
#  some brackets ahve the form '[[___|___]]'
#  the left is the displayed text, the right is the linked page name, and we want the part to teh right
#  - Input: a vector of all the '___' that were between the brackets, from get_bracket_data
#  - Output: a vector of all the '___' from teh input, with the '|' removed


# parse_and_save_raw_person_db 
#  might just be changed to 'parse_data
#  calls GET_data_to_parse
#  grabs 'infobox' and'name' and sticks them in a list obj
# then loops over the other properties, to call pull_bracketed_data over each other property,
#    saving them into the same list onj
# Finally, saves as a side effect, that list obj to person_db, which will be the db for cleaned raw_person_db
#  - Input: a name
#  - Output: none, or a T/F about success
#  - Side Effect: the cleaned person's data is added to persons_db



# GET_data_to_parse --------------------------------------------------------

#  pulls the data from raw_person_db
# and picks out the properties we plan to analyse, lsited up top of this file
#  - Input: name
#  - Output: person's list from raw_person_db, containing ony the properties we care about

GET_data_to_parse <- function(name) {
  person_data <- raw_person_db[[name]]
  
  #select the relevant bits of acceptable properties
  relevant_person_data <- 
    person_data[c("infobox",  "name",  "API_call",
                  "influences",        "doctoral_advisor", 
                  "academic_advisors", "influenced", 
                  "notable_students",  "doctoral_students",
                  "known_for",         "field",     
                  "fields")]
  #grab the list elements that are not NULL, which were created when we did the subsetting of person_data above
  #also removes empty "" strings
  return(relevant_person_data[!unlist(map(relevant_person_data, is.null)) & 
                              relevant_person_data != ""])
}
#that barely needed to be a function. 
#  I'm gonna keep it in. i expect once I have much more data and I have to store the data in another file and retrieve from it, 
#  grabbing the data will be much less simple than simply using '[[' and will benefit from the clarity of its own function. 
#  Essentially, im expecting an increase in complexity


# get_bracket_data ------------------------------------------------------------
# takes the list element, and picks out all '[[___]]'
# input: a single prpoerty form a person, like raw_person_db$person$property
# output: a vector of all '[[___]]' located


get_bracket_data <- function(property) {

  # handle a case where there is an empty string for data
  if(property == "") { 
    bracket_vector <- NA 
  } else {
    #splits up the property vector into each character, for easier use in the next for loop
    atomized_property_vector <- unlist(str_split(property, ""))

    bracket_layer <- 0
    bracket_vector <- NULL
  
    for (i in 1:(nchar(property) - 1)) {
      # -1 because the final index would cause an automatic failure every time,
      # but would be meaningless to address since it will never open or close a vector anyway

      if (atomized_property_vector[i] == "[" & atomized_property_vector[i + 1] == "[") {
        #just note the start location, for when we find the end location
        start_index <- i + 2
      } 
      else if (atomized_property_vector[i] == "]" & atomized_property_vector[i + 1] == "]") {
        #when you find a close bracket, just take the contents and stick them in the collection vector
        end_index <- i - 1
        bracket_vector <- c(bracket_vector, 
                          substr(property, start_index, end_index))
        # probably not necessary, but it would be a decent error catch if, ie, we get to the end of a bracket without having a start
        rm(start_index, end_index)
      }
    }
  }
  return(bracket_vector)
}
  

# handle_vertical_bars ----------------------------------------------------

#  some brackets ahve the form '[[___|___]]'
#  the left is the displayed text, the right is the linked page name, and we want the part to teh right
#  - Input: a vector of all the '___' that were between the brackets, from get_bracket_data
#  - Output: a vector of all the '___' from the input, with the '|' removed


handle_vertical_bars <- function(bracket_data) {
  for (i in seq_along(bracket_data)) {
    # cases where we have '[[___|___|___]]' are possible, but I'm not as confident in taking the last ___ segment.
    # it will return that, but the warning will point out the potential source of errors
    if (any(str_count(bracket_data, "\\|") > 1)) {
      warning("WARNING: handle_vertical_bars does not currently handleitems with multiple vertical bars. Output is suspect.")
    }
    
    if (str_detect(bracket_data[i], "\\|")) {
      bracket_data[i] <- 
        str_remove(bracket_data[i], "^.*\\|")
    }
  }
  return(bracket_data)
}
  


# parse_and_save_raw_person_db -----------------------------------------------------

#  calls GET_data_to_parse
#  grabs 'infobox' and'name' and sticks them in a list obj
# then loops over the other properties, to call pull_bracketed_data over each other property,
#    saving them into the same list onj
# Finally, saves as a side effect, that list obj to person_db, which will be the db for cleaned raw_person_db
#  - Input: a name
#  - Output: none, or a T/F about success
#  - Side Effect: the cleaned person's data is added to persons_db

person_db <- list()
parse_and_save_raw_person_db <- function(name) {
  
  #grab that data
  unparsed_person_data <- GET_data_to_parse(name)
  
  #pick out the 'infobox' and 'name' data, stick them in a list
  parsed_person_data <- list(infobox = unparsed_person_data$infobox,
                             name = unparsed_person_data$name,
                             API_call = unparsed_person_data$API_call)
  #then just remove them from the original list
  unparsed_person_data$infobox <- NULL
  unparsed_person_data$name <- NULL
  unparsed_person_data$API_call <- NULL
  
  # for each list element, grab and parse it, and add that to the list
  for (i in seq_along(unparsed_person_data)) {
    property_name <- names(unparsed_person_data[i])
    parsed_person_data[[property_name]] <-
      unparsed_person_data[[i]] %>%
      get_bracket_data() %>%
      handle_vertical_bars()
  }
  
  #here we're gonna define clearly who this eprson was influenced by, and who they influenced in turn
  parsed_person_data$was_influenced_by <- 
    c(parsed_person_data$influences,
      parsed_person_data$doctoral_advisor,
      parsed_person_data$academic_advisors)
    
  parsed_person_data$had_influenced <-
    c(parsed_person_data$influenced,
      parsed_person_data$doctoral_students,
      parsed_person_data$notable_students,
      parsed_person_data$known_for)
    
  person_db[[name]] <<- parsed_person_data
}

parse_and_save_raw_person_db("Richard Feynman")
str(person_db)
