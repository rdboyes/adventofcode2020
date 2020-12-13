## Challenge 1

    library(readr)
    library(stringr)
    library(tidyr)
    library(dplyr)

    options(scipen = 999)

    read_lines(here::here("data/input_13.txt"))[2] %>% 
      str_split(",") %>% unlist() %>% 
      as.numeric() %>% as.data.frame() %>% 
      magrittr::set_colnames("bus") -> bus

    bus %>% 
      drop_na() %>% 
      mutate(time_after = bus - as.numeric(read_lines(here::here("data/input_13.txt"))[1]) %% bus) %>% 
      slice(which.min(time_after)) %>% 
      prod()

    ## [1] 246

## Challenge 2

    bus %>% 
      mutate(tplus = 0:(n() - 1)) %>% 
      drop_na() -> bus

    num <- inc <- bus$bus[1]

    for(i in 2:nrow(bus)){
      while(TRUE){
        if((num + bus$tplus[i]) %% bus$bus[i] == 0){break}
        num <- num + inc
      }
      inc <- prod(bus$bus[1:i])
    }

    num

    ## [1] 939490236001473
