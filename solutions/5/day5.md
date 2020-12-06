## Challenges 1 and 2

The numbers are essentially provided in an encoded variation of binary.
We read and convert them to real binary, then calculate the required id
information:

    library(readr)
    library(dplyr)
    library(stringr)

    read_lines(here::here("data/input_5.txt")) %>% 
      str_replace_all("B|R", "1") %>% 
      str_replace_all("F|L", "0") %>% 
      strtoi(base = 2) %>%
      as_tibble() %>% 
      summarize(part1 = max(value),
                part2 = setdiff(min(value):max(value),value))

    ## # A tibble: 1 x 2
    ##   part1 part2
    ##   <int> <int>
    ## 1   994   741
