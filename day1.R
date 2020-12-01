# Day 1 Challenge 1

# find the two numbers in the input file that sum to 2020, return their product

library(readr)

input <- as.numeric(read_lines(here::here("data/input_1_1.txt"))) 

two_numbers <- function(numbers){
  prod(intersect(numbers, 2020 - numbers))
}

two_numbers(input)

# Challenge 2

# find the three numbers that sum to 2020 this time

three_numbers <- function(numbers){
  temp <- 2020 - numbers
  for(i in 1:length(numbers)){
    if(length(intersect(numbers, temp - numbers[i])) > 0){
      return(prod(c(intersect(numbers, temp - numbers[i]), numbers[i])))
    }
  }
}

three_numbers(input)