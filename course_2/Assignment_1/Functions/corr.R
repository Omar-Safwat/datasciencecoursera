corr <- function(directory = "Data/specdata", threshold = 0)
{
  ##find the indices of files having median above threshold from my_obs
  indices <- my_obs[which(my_obs[, 2] >= threshold), 1]
  correlations <- c()
  for(i in seq_along(indices))
  {
    ##Append zeros infornt of th index because of how the csv files are named
    if(indices[i] < 10)
    {
      fpath <- paste(directory, "/00", indices[i], ".csv", sep = "")
    }
    else if(indices[i] > 9 & indices[i] < 100)
    {
      fpath <- paste(directory, "/0", indices[i], ".csv", sep = "")
    }
    else
    {
      fpath <- paste(directory, "/", indices[i], ".csv", sep = "")
    }
    current <- read.csv(fpath, colClasses = c("Date", "numeric", "numeric", "integer"), comment.char =  "")
    current <- na.omit(current)
    correlations <- c(correlations, cor(x = current[ , 2], y = current[ , 3]))
  }
  return(correlations)
}