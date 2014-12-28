## rankall


rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv("outcome-of-care-measures.csv",  
                       na.strings = "Not Available",
                       stringsAsFactors=FALSE)
  
  ## Check that the outcome is valid
  valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
  heart.attack = 11
  heart.failure = 17
  pneumonia = 23
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
  
  ## For each state, find the hospital of the given rank
  states <- outcomes$State
  states <- unique(states)
  
  hospitals.at.rank <- data.frame()
  
  for(st in states)
  {
    vars.to.keep <- c(2, 7, num.outcome)
    data.to.sort <- outcomes[vars.to.keep]
    data.to.sort <- data.to.sort[complete.cases(data.to.sort),]
    hospitals.in.state <- data.to.sort[data.to.sort$State == st, ]
    if(num == "worst")
    {
      sorted.hospitals <- hospitals.in.state[order(-hospitals.in.state[,3],hospitals.in.state[,1]),]
      hospitals.at.rank <- rbind(hospitals.at.rank,sorted.hospitals[1,])
    }
    else if(num == "best")
    {
      sorted.hospitals <- hospitals.in.state[order(hospitals.in.state[,3],hospitals.in.state[,1]),]
      hospitals.at.rank <- rbind(hospitals.at.rank,sorted.hospitals[1,])
    }
    else
    {
      sorted.hospitals <- hospitals.in.state[order(hospitals.in.state[,3],hospitals.in.state[,1]),]
      if(is.na(sorted.hospitals[num,1]))
      {
        sorted.hospitals[num,2] = st
      }
      hospitals.at.rank <- rbind(hospitals.at.rank,sorted.hospitals[num,])
    }
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  vars.to.keep <- c(1,2)
  hospitals.at.rank <- hospitals.at.rank[vars.to.keep]
  colnames(hospitals.at.rank) <- c("hospital","state")
  hospitals.at.rank <- hospitals.at.rank[order(hospitals.at.rank$state),]
}