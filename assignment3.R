## Programming Assignment 3 - Hospital Data

best <- function(state, outcome) {
  ## Read outcome data
  
  ## Check that state and outcome are valid
  valid.states <- outcomes[,7]
  valid.states <- unique(valid.states)
  valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
  heart.attack = 11
  heart.failure = 17
  pneumonia = 23
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
      ## Now set the column index
      if(outcome == "heart attack")
      {
        num.outcome <- heart.attack
      }
      else if(outcome == "heart failure")
      {
        num.outcome <- heart.failure
      }
      else if(outcome == "pneumonia")
      {
        num.outcome <- pneumonia
      }
    }
  }
  if(valid == 0)
  {
    stop("invalid outcome")
  }
  
  ## Get a logical vector of hospitals in a state
  hospitals.in.state <- outcomes[,7] == state
  outcomes.in.state <- outcomes[hospitals.in.state,]
  ## Now we are only interested in the hospital name and data in question
  data <- data.frame(outcomes.in.state[,2], outcomes.in.state[,num.outcome])
  colnames(data) <- c("Hospital","Data")
  data <- data[complete.cases(data),]
  sorted.data <- data[order(Data),]
  
  ## Return hospital name in the state with lowest 30-day death rate
  #attributes(outcomes.in.state)
  print("Done")
  sorted.data
}

outcomes <- read.csv("outcome-of-care-measures.csv",  
                     na.strings = "Not Available",
                     stringsAsFactors=FALSE)
head(outcomes)

print("How many columns are present?")
print(ncol(outcomes))

## Create a 30-day histogram death rates from heart attack (col 11)
outcomes[,11] <- as.numeric(outcomes[,11])
hist(outcomes[,11])
