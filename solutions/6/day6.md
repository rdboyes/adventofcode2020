## Challenge 1

We need to find the total number of questions that had at least one yes
in each group, then add them. We can combine all answers in each group,
then run `unique` |&gt; `length` by group to find the total number of
questions with at least one yes.

    library(tidyr)
    library(readr)
    library(stringr)
    library(purrr)

    read_file(here::here("data/input_6.txt")) %>% 
      str_split("\n\n") %>% unlist() -> customs

    customs %>% 
      str_replace_all("\n", "") %>% 
      str_split("") %>% 
      map_int(~length(unique(.))) %>% 
      sum()

    ## [1] 6809

## Challenge 2

Now we need the number of questions where everyone answered yes. The
code is relatively straightforward except the `setdiff` call, which is
there to remove the blank row at the end of the file.
`Reduce(intersect)` finds the intersect of all of the vectors in the
list.

    customs %>% 
      str_split("\n") %>% 
      map_int(. %>% 
            setdiff("") %>% 
            str_split("") %>% 
            Reduce(intersect, .) %>% 
            length()
          ) %>% 
      sum()

    ## [1] 3394
