## Programming Assignment 3 - Hospital Data

outcome <- read.csv("outcome-of-care-measures.csv", 
                    colClasses = "character", 
                    na.strings = "Not Available")
head(outcome)

print("How many columns are present?")
print(ncol(outcome))

## Create a 30-day histogram death rates from heart attack (col 11)
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])
