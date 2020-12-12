outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

rankall <- function(out_come, num = "best")
{
  ##Equate the entered out_come to the exact col name
  if(out_come == "heart attack")
    out_come <- colnames(outcome[11])
  else if(out_come == "heart failure")
    out_come <- colnames(outcome[17])
  else if(out_come == "pneumonia")
    out_come <- colnames(outcome[23])
  else
    stop("invalid outcome")
  
  ##Split the rows by States
  data <- split(outcome[, c("Hospital.Name", "State", out_come)], outcome$State)
  ##Remove all "Not Available" values
  data <- lapply(data, function(x){x[!(grepl("Not Available", x[, out_come])),]})
  ## Sort all data.frames of all states in "data"
  sorted <- lapply(data, function(x){order(as.numeric(x[,out_come]), x[,"Hospital.Name"])})
  
  ##Function to return index of hospitals from the list "sorted"
  getindex <- function(x)
  {
    if(num == "best")
    {
      return(x[1])
    }
    else if(num == "worst")
    {
      return(x[length(x)])
    }
    else if(length(x) < num)
    {
      return(NA)
    }
    else
      x[num]
  }
  
  indices <- lapply(sorted, getindex)
  
  ##Function to match Lists in "indices" with lists in "h_names"
  ##that store data of the same state!
  ##example: indices[1] and h_names[1] both hold data of state "AK"
  match_State <- function(x)
  {
    n_index <- x[1,"State"]
    if(is.na(getElement(indices, n_index)))
      return(data.frame("Hospital.Name" = NA, "State" = x[1,"State"]))
    else
      x[getElement(indices, n_index), c("Hospital.Name", "State")]
  }
  h_names <- lapply(data, match_State)
  
  ##Recursively merge the data.frames in list "h_names"
  result <- rbind.data.frame(h_names[[1]],h_names[[2]])
  i <- 3
  while(i <= length(h_names))
  {
    result <- rbind.data.frame(result, h_names[[i]])
    i = i + 1
  }
  result
}