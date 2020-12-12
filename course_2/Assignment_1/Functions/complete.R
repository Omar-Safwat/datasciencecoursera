complete <- function(directory = "Data/specdata", id = 1:322)
{
  my_dir <- list.files(directory, full.names = TRUE)
  my_obs <- data.frame(id)
  my_row <- 1
  for(file in my_dir[id])
  {
    current <- read.csv(file, colClasses = c("Date", "numeric", "numeric", "integer"), comment.char =  "")
    current <- na.omit(current)
    my_obs[my_row, 2] <- nrow(current)
    my_row <- my_row + 1
  }
  names(my_obs) <- c("id", "nobs")
  return(my_obs)
}