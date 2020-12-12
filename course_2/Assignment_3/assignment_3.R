source("rankall.R")
r <- rankall("heart attack", 4)
as.character(subset(r, State == "HI")$hospital)
