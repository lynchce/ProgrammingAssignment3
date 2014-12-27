## Programming Assignment 3 - Hospital Data

best <- function(state, outcome) {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  valid.states <- outcomes[,7]
  valid.states <- unique(valid.states)
  valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
  valid <- 0
  for (st in valid.states) {
    if(st == state)
    {
      valid <- 1
    }
  }
  if (valid == 0)
  {
    stop("invalid state")
  }
  valid <- 0
  for (illness in valid.outcomes)
  {
    if(illness == outcome)
    {
      valid <- 1
    }
  }
  if(valid == 0)
  {
    stop("invalid outcome")
  }
  
  ## Return hospital name in the state with lowest 30-day death rate
}

outcomes <- read.csv("outcome-of-care-measures.csv", 
                     colClasses = "character", 
                     na.strings = "Not Available")
head(outcomes)

print("How many columns are present?")
print(ncol(outcomes))

## Create a 30-day histogram death rates from heart attack (col 11)
outcomes[,11] <- as.numeric(outcomes[,11])
hist(outcomes[,11])
