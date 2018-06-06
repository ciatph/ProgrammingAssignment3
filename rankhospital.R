## Programming Assignment 3: Ranking hospitals by outcome in a state
## For JHU Data Science - R Programming course
## madbarua 20180606

## Creates a special "matrix" object that can cache its inverse
## The matrix supplied is always invertible
## isValid: check if a list contains given value
## readCSV: read and format the csv file
## rankhospital: get the original matrix


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


# Returns the hospital in the state with the specified ranking for 
# low mortality rates for a given outcome in a 30-day period
# Ranks state hospitals according to low mortality rates in a 30-day period
# In case of ties in the lowest mortality rates, hospitals are arranged alphabetically
# @param stateCode: 2-character code for a state
# @param outcome: disease to check ("heart attack", "heart failure", "pneumonia")
# @param num: - numeric ranking number or 
#             - "best" = lowest rate, or 
#             - "worst" = highest mortality rate
rankhospital <- function(stateCode, outcome, num = "best"){
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
  
  # Get the index of the list's row no. if num ranking is "best" or "worst"
  if(num == "best")  
    num = 1
  else if(num == "worst")
    num = nrow(clean_data)
  
  # Order results first by outcome-ranking then alphabetically by Hospital.Name
  # Return the hospital name in the state with the given rank 
  # 30-day death rate
  clean_data[order(clean_data[outcome], clean_data$Hospital.Name),][num,1]
}