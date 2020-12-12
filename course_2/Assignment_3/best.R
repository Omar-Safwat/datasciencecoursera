best <- function(state, out_come)
{
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data <- outcome$State == state
  
  if(sum(data, na.rm = TRUE) == 0)
  {
    stop("invalid state")
  }
  
  ##Equate the entered out_come to the exact col name
  if(out_come == "heart attack")
    out_come <- colnames(outcome[11])
  else if(out_come == "heart failure")
    out_come <- colnames(outcome[17])
  else if(out_come == "pneumonia")
    out_come <- colnames(outcome[23])
  else
    stop("invalid outcome")
    
  ##Store the row indices of the specified state
  data <- which(data)
  data <- outcome[data, c(colnames(outcome[2]), out_come)]
  ##Removing rows with "Not Available" values using grepl()
  data <- data[!(grepl("Not Available", data[, out_come])), ]
  sorted <- order(as.numeric(data[, out_come]), data[, colnames(outcome[2])])
  best <- data[head(sorted, n = 1),1]
}