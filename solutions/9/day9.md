## Challenge 1

We can bring back our code from the very first day to help with this
one.

    library(readr)
    library(purrr)
    library(dplyr)

    numbers <- as_tibble(as.numeric(read_lines(here::here("data/input_9.txt")))) 

    two_numbers <- function(numbers, target){
      intersect(numbers, target - numbers)
    }

We need to run this code on the numbers from 26 and up, using the 25
numbers before them.

    map_int(26:nrow(numbers), 
        ~two_numbers(slice(numbers, (. - 25):(. - 1)), 
                     pull(slice(numbers, .))) %>% 
          nrow()
        ) %>% 
      which.min() %>% 
      {slice(numbers, (. + 25))} %>% 
      pull() -> part1

    part1

    ## [1] 133015568

I’m trying really hard not to write loops, but the loop solution is so
clean:

    from <- 1
    to <- 2

    while(TRUE){
      if(sum(numbers$value[from:to]) < part1){to <- to + 1}
      else if(sum(numbers$value[from:to]) > part1){from <- from + 1}
      else{break}
    }

    min(numbers$value[from:to]) + max(numbers$value[from:to])

    ## [1] 16107959
