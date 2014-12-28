## rankhospital
# 3 arguments - state, outcome, num
# state: 2 letter abbreviation
# outcome: heart attack, heart failure, pneumonia
# num: the ranking of a hospital in that state for that outcome

rankhospital <- function(state, outcome, num="best") {
  if(num != "best")
  {
    # Read the data
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
    if(num == "worst")
    {
      sorted.hospitals <- hospitals.in.state[order(-hospitals.in.state[,3]),]
    }
    else
    {
      sorted.hospitals <- hospitals.in.state[order(hospitals.in.state[,3]),]
    }
    
    ## Return the hospital ranked at num
    if(num == "worst")
    {
      hospital.at.rank <- sorted.hospitals[1,1]
    }
    else
    {
      hospital.at.rank <- sorted.hospitals[num,1]
    }
  }
  else
  {
    source("best.R")
    hospital.at.rank <- best(state, outcome)
  }
}