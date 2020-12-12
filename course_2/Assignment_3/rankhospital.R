
rankhospital <- function(state, out_come, num = "best")
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
  
  data <- which(data)
  data <- outcome[data, c(colnames(outcome[2]), out_come)]
  ##Removing rows with "Not Available" values using grepl()
  data <- data[!(grepl("Not Available", data[, out_come])), ]
  sorted <- order(as.numeric(data[, out_come]), data[, colnames(outcome[2])])
  
  if(num == "best")
  {
    return(data[sorted[1],1])
  }
  else if(num == "worst")
  {
    return(data[tail(sorted, n = 1), 1])
  }
  if(length(sorted) < num)
  {
    return(NA)
  }
  data[sorted[num], 1]
}