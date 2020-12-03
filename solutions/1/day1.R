# Day 1 Challenge 1

# find the two numbers in the input file that sum to 2020, return their product

library(readr)

input <- as.numeric(read_lines(here::here("data/input_1_1.txt"))) 

two_numbers <- function(numbers, target){
  prod(intersect(numbers, target - numbers))
}

two_numbers(input, 2020)

# Challenge 2

# find the three numbers that sum to 2020 this time

three_numbers_ugly <- function(numbers, target){
  temp <- target - numbers
  for(i in 1:length(numbers)){
    if(length(intersect(numbers, temp - numbers[i])) > 0){
      return(prod(c(intersect(numbers, temp - numbers[i]), numbers[i])))
    }
  }
}

three_numbers_cleaner <- function(numbers, target){
  targets <- target - numbers
  for(i in 1:length(targets)){
    if(two_numbers(numbers, targets[i]) > 1){
      return(two_numbers(numbers, targets[i]) * numbers[i])
    }
  }
}

library(purrr)
library(dplyr)

three_numbers <- function(numbers, target){
  map_dbl(target - numbers, ~two_numbers(numbers, .)) %>% 
    prod() %>% 
    sqrt()
}

library(rbenchmark)

benchmark("ugly" = {three_numbers_ugly(input, 2020)},
          "cleaner" = {three_numbers_cleaner(input, 2020)},
          "map" = {three_numbers(input, 2020)})



