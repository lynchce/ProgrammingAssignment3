## best
# This function reads a csv file containing healthcare outcomes
# from different hospitals in different states.  The function 
# accepts two arguments, the state we wish to study, and the 
# disease outcome we wish to study.
# Diseases: heart attack, heart failure, pneumonia
# 
# This function will return the name of the hospital with the
# best 30-day mortality for a given disease in a given state.

best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv",  
                       na.strings = "Not Available",
                       stringsAsFactors=FALSE)
  
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
  vars.to.keep <- c(2, 7, num.outcome)
  data.to.sort <- outcomes[vars.to.keep]
  data.to.sort <- data.to.sort[complete.cases(data.to.sort),]
  hospitals.in.state <- data.to.sort[data.to.sort$State == state, ]
  sorted.hospitals <- hospitals.in.state[order(hospitals.in.state[,3]),]
  
  ## Return hospital name in the state with lowest 30-day death rate
  best.hospital.name <- sorted.hospitals[1,1]
}