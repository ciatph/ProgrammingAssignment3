## Programming Assignment 3: Ranking hospitals in all states
## For JHU Data Science - R Programming course
## madbarua 20180606

## Generates a data.frame of hospitals with the specified rank per state
## isValid: check if a list contains given value
## readCSV: read and format the csv file
## rankall: lists hospitals with specified rank per state


# Check if a list contains a given value
# Returns TRUE|FALSE if value is found in the list
isValid <- function(list, value){
  # Assume at first value is not found in the list
  isPresent = FALSE
  
  # Search through list for value
  for(i in 1:length(list)){
    if(value == list[i]){
      # Mark as TRUE and stop searching if value is found
      isPresent = TRUE
      break
    }
  }
  
  # Return the final "found" flag
  isPresent
}



# Initialize data: Read csv file
# Returns the read csv file
readCSV <- function(toInteger = c(11,17,23), columnNames = c("heart attack", "heart failure", "pneumonia")){
  # Read outcome data
  csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  head(csv)
  
  for(i in 1:length(toInteger)){
    # Replace column names
    colnames(csv)[toInteger[i]] <- columnNames[i]
    
    # Coerce to numeric values
    csv[,toInteger[i]] <- as.numeric(csv[,toInteger[i]])    
  }
  csv
}



# Returns a data.frame containing hospital name and state codes of
# the hospital per state with the specified ranking for mortality rates 
# for a given outcome in a 30-day period
# In case of ties in the lowest mortality rates, hospitals are arranged alphabetically
# @param outcome: disease to check ("heart attack", "heart failure", "pneumonia")
# @param num: - numeric ranking number or 
#             - "best" = lowest rate, or 
#             - "worst" = highest mortality rate
rankall <- function(outcome, num = "best"){
  # Read and clean the csv file
  data <- readCSV();
  
  # Encode the hardcoded outcomes
  outcome_list <- c("heart attack", "heart failure", "pneumonia");
  
  # Check if valid stateCode
  if(!isValid(outcome_list, outcome))
    stop("invalid outcome")  
  
  # Get the numeric index of outcome
  # 11 - heart attack
  # 17 - heart failure
  # 23 - pneumonia
  index = 11
  
  # Get the outcome numeric index
  if(outcome == outcome_list[2])
    index = 17
  else if(outcome == outcome_list[3])
    index = 23  
  
  # Clean the outcome data of NA's
  clean_data <- data[!is.na(data[outcome]), ][, c(2,7,index)]
  
  # Extract unique states list
  states_list <- unique(data$State)
  
  # Initialize the data.frame container
  final <- data.frame(hospital="", state="")
  
  maxLen <- length(states_list);
  for(i in 1:maxLen){
    # Filter data select only stateCode
    cursor <- clean_data[clean_data$State == states_list[i],]  
    
    # Get the index of the list's row no. if num ranking is "best" or "worst"
    if(num == "best")  
      num = 1
    else if(num == "worst")
      num = nrow(cursor)
  
    
    # Order results first by outcome-ranking then alphabetically by Hospital.Name
    # Select the Hospital.Name and state with the given rank in the ordered list
    pair <- cursor[order(cursor[outcome], cursor$Hospital.Name),][num, c(1,2)]
    
    
    if(is.na(pair)){
      # Format values if NA
      final <- rbind(final, data.frame(hospital=NA, state=states_list[i]))
    }
    else{
      # Keep track of normal results
      final <- rbind(final, data.frame(hospital=pair$Hospital.Name, state=pair$State))
    }
  }
  final
}