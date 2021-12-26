library(here)

ref_folder <- here("TidyTuesday_Explained/088-Advent_of_Code_Day_7")
input_file <- file.path(ref_folder,"day-7-whales.txt")
x <- scan(input_file, sep = ",")

min_x <- min(x)
max_x <- max(x)

min_x
max_x

## Part 1

x_seq <- seq(from =min_x, to = max_x)

y <- rep(NA, length(x_seq))

for(i in 1:length(x_seq)){
  
  y[i] <- sum(abs(x - x_seq[i]))
}

head(y)
length(y)
plot(y)

min(y)
max(y)

which(y == min(y))
(min_loc <- which.min(y))

sum(abs(x - x_seq[min_loc]))

## Part 2

y2 <- rep(NA, length(x_seq))

gas_used_vector <- cumsum(x_seq + 1)

for(i in 1:length(x_seq)){
  y2[i] <- sum(gas_used_vector[abs(x - x_seq[i])])
}

head(y2)
length(y2)
plot(y2)

min(y2)
max(y2)

which(y2 == min(y2))
(min_loc <- which.min(y2))

sum(gas_used_vector[abs(x - x_seq[min_loc])])
