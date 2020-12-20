## Challenge 1

1 + (2 \* 3) + (4 \* (5 + 6))

1.  %&gt;% (add((2) %&gt;% mult(3)) %&gt;% add((4) %&gt;% mult((5)
    %&gt;% add(6)))

-   becomes “) %&gt;% add(”
-   becomes “) %&gt;% mult(” parenthesis beginning and end

<!-- -->

    library(dplyr)
    library(stringr)
    library(readr)
    library(tibble)
    library(purrr)

    add <- function(x, y){x + y}
    mult <- function(x, y){x * y}

    eval_string <- function(str){
      return(str %>% str_replace_all("\\+", ") %>% add(") %>% 
        str_replace_all("\\*", ") %>% mult(") %>% 
        paste0("(", ., ")") %>% 
        parse(text = .) %>% 
        eval())
    }

    input <- read_lines(here::here("data/input_18.txt")) 

    input %>% 
      map_dbl(eval_string) %>% 
      sum()

    ## [1] 701339185745

## Challenge 2

We define “special plus” as an infix operator, giving it higher
precedence than multiplication.

    `%add%` <- function(x, y){x + y}

    eval_string_p2 <- function(str){
      return(str %>% str_replace_all("\\+", "%add%") %>% 
        parse(text = .) %>% 
        eval())
    }

    input %>% 
      map_dbl(eval_string_p2) %>% 
      sum()

    ## [1] 4208490449905
