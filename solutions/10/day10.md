## Challenge 1

Weirdly easy after the last few:

    library(readr)
    library(tibble)
    library(dplyr)
    library(purrr)

    options(scipen = 999)

    tibble(jolt = read_lines(here::here("data/input_10.txt")) %>% as.numeric()) %>% 
      add_row(jolt = 0) %>% 
      arrange(jolt) %>% 
      transmute(change = jolt - lag(jolt)) %>% 
      add_row(change = 3) -> jolts

    jolts %>% 
      table() %>% 
      prod()

    ## [1] 1998

Every jump of 3 narrows the possibilities - there is only one legal way
to cross a gap of 3. Every stretch of 1’s of the same length will have
the same number of branching paths. There aren’t any 2’s in the data.

    paths <- function(x){
      if(x < 0){return(0)}
      if(x <= 1){return(1)}
      return(paths(x - 1) + paths(x - 2) + paths(x - 3))
    }

    jolts %>% 
      slice(2:nrow(jolts)) %>% 
      group_by(cumsum(change == 3)) %>% 
      filter(change == 1) %>% 
      summarize(num_ones = n()) %>% 
      pull() %>% 
      map_dbl(., ~paths(.)) %>% 
      prod()

    ## [1] 347250213298688
