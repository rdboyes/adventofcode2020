## Challenge 1

The numbers are essentially provided in an encoded variation of binary.
We read and convert them to real binary, then use the row and column
numbers to find the seat id numbers.

    library(readr)
    library(dplyr)
    library(stringr)

    input <- read_fwf(here::here("data/input_5.txt"), fwf_widths(c(7, 3)))

    colnames(input) <- c("row", "col")

    decode <- function(str_vector, one = "B", zero = "F"){
      str_vector %>% 
        str_replace_all(one, "1") %>% 
        str_replace_all(zero, "0") %>% 
        strtoi(base = 2)
    }

    seats <- data.frame(row = decode(input$row), col = decode(input$col, one = "R", zero = "L")) %>% 
      mutate(id = row * 8 + col)

Finding the highest id number from here is simple:

    max(seats$id)

    ## [1] 994

## Challenge 2

We can find the expected sum of id’s between the lowest and highest id
if all were present, then compare to the actual sum to find the missing
value.

    0.5 * (max(seats$id) - min(seats$id) + 1) * (max(seats$id) + min(seats$id)) - sum(seats$id)

    ## [1] 741
