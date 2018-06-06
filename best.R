## Programming Assignment 3: Finding the best hospital in a state
## For JHU Data Science - R Programming course
## madbarua 20180606

## Finds the best hospital in a state for a given outcome 
## isValid: check if a list contains given value
## readCSV: read and format the csv file
## best: get the original matrix



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
# Returns the csv file with numeric values on specified indices
# @param toInteger: Vector of column indices to make numeric
# @param columnNames: Character vector of names for specified toInteger indices
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


# Retrieve valid "columnNo" data from master list
# Append Hospital.Name and State columns
getCleanData <- function(list, columnNo){
  list[!is.na(list[columnNo]), ][, c(2,7,columnNo)]
}


# Finds the "best" hospital in a state for a given outcome
# Best hospitals have the lowest mortality rate for a given outcome in a 30-day period
# In case of ties in the lowest mortality rates, hospitals are arranged alphabetically
# @param stateCode: 2-character code for a state
# @param outcome: disease to check ("heart attack", "heart failure", "pneumonia")
best <- function(stateCode, outcome){
  # Read and clean the csv file
  data <- readCSV();
  
  # Extract unique states list
  states_list <- unique(data$State)
  
  # Encode the hardcoded outcomes
  outcome_list <- c("heart attack", "heart failure", "pneumonia");
  
  # Check if valid stateCode
  if(!isValid(states_list, stateCode))
    stop("invalid state")
  
  # Check if valid stateCode
  if(!isValid(outcome_list, outcome))
    stop("invalid outcome")  
  
  # Initialize the log message container
  best <- "NONE";
  
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
  
  # Filter data select only stateCode
  clean_data <- clean_data[clean_data$State == stateCode,]
  
  # Find the smallest mortality rate among all State=stateCode
  smallest <- min(clean_data[clean_data$State == stateCode,][,3])
  
  # Find matching Hospital.Names with "smallest" value
  clean_data <- clean_data[clean_data[outcome] == smallest,]
  
  # Arrange alphabetically and get the 1st value (Hospital.Name)
  best <- clean_data[order(clean_data$Hospital.Name),][1,1]
  print(best)
}