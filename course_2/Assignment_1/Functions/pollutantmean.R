pollutantmean <- function(directory = "Data/specdata", pollutant = "sulfate", id = 1:332)
{
  my_dir <- list.files(directory, full.names = TRUE)
  data <- c()
  for(file in my_dir[id])
  {
    current <- read.csv(file, colClasses = c("Date", "numeric", "numeric", "integer"), comment.char =  "")
    current <- na.omit(current)
    current2 <- current[ ,pollutant]
    data <- c(data, current2)
  }
   my_mean <- mean(data, na.rm = TRUE)
}