## Challenge 1

    library(readr)

    input <- read_lines(here::here("data/input_2.txt"))

This is a string processing problem. Given a string of the form “X-Y C:
ccccc”, determine whether C appears between X and Y times in c. The
first step will be to parse the input strings into individual variables.
`stringr::str_split()` should be useful for this.

    library(stringr)

    str_split("X-Y C: ccccc", " |-|: ")

    ## [[1]]
    ## [1] "X"     "Y"     "C"     "ccccc"

Next, we need a way to count the number of times our third variable
appears in the fourth - `stringr::str_count()`.

    str_count("abcde", "a")

    ## [1] 1

Putting it together:

    library(dplyr)

    password_valid <- function(string_vector){
      str_split(string_vector, " |-|: ") %>% 
        do.call(rbind.data.frame, .) %>% 
        magrittr::set_names(c("low", "high", "char", "str")) %>% 
        mutate(across(low:high, as.numeric)) %>% 
        mutate(valid = between(str_count(str, char), low, high)) %>% 
        summarise(total_valid = sum(valid))
    }

    password_valid(input)

    ##   total_valid
    ## 1         434

But this function doesn’t get the right answer. Let’s do some digging.

    test_case <- c("1-3 a: abcde",
                   "1-3 b: cdefg",
                   "2-9 c: ccccccccc")

    password_valid <- function(string_vector){
      str_split(string_vector, " |-|: ") %>% 
        do.call(rbind.data.frame, .) %>% 
        magrittr::set_names(c("low", "high", "char", "str")) %>% 
        mutate(across(low:high, as.numeric)) %>% 
        mutate(valid = between(str_count(str, char), low, high)) #%>% 
        #summarise(total_valid = sum(valid))
    }

    password_valid(test_case)

    ##   low high char       str valid
    ## 1   1    3    a     abcde  TRUE
    ## 2   1    3    b     cdefg FALSE
    ## 3   2    9    c ccccccccc FALSE

Looks like there’s something wrong with the validity logic. Test case 3
should be TRUE, not false.

    password_valid <- function(string_vector){
      str_split(string_vector, " |-|: ") %>% 
        do.call(rbind.data.frame, .) %>% 
        magrittr::set_names(c("low", "high", "char", "str")) %>% 
        mutate(across(low:high, as.numeric)) %>% 
        mutate(count = str_count(str, char)) %>%
        mutate(valid = between(count, low, high))
        #summarise(total_valid = sum(valid))
    }

    password_valid(test_case)

    ##   low high char       str count valid
    ## 1   1    3    a     abcde     1  TRUE
    ## 2   1    3    b     cdefg     0 FALSE
    ## 3   2    9    c ccccccccc     9 FALSE

Ugh. Floating point comparisons. Easy fix:

    library(dplyr)

    password_valid <- function(string_vector){
      str_split(string_vector, " |-|: ") %>% 
        do.call(rbind.data.frame, .) %>% 
        magrittr::set_names(c("low", "high", "char", "str")) %>% 
        mutate(across(low:high, as.integer)) %>% # as.integer instead of as.numeric
        mutate(valid = between(str_count(str, char), low, high)) #%>% 
        #summarise(total_valid = sum(valid))
    }

    password_valid(test_case)

    ##   low high char       str valid
    ## 1   1    3    a     abcde  TRUE
    ## 2   1    3    b     cdefg FALSE
    ## 3   2    9    c ccccccccc FALSE

WTF?

    password_valid <- function(string_vector){
      str_split(string_vector, " |-|: ") %>% 
        do.call(rbind.data.frame, .) %>% 
        magrittr::set_names(c("low", "high", "char", "str")) %>% 
        mutate(across(low:high, as.integer)) %>% 
        mutate(val_count = str_count(str, char)) %>% 
        mutate(valid = val_count >= low & val_count <= high) %>% 
        summarise(total_valid = sum(valid))
    }

    password_valid(test_case)

    ##   total_valid
    ## 1           2

Seems like the problem is the `dplyr::between()` function. Weird.

    password_valid <- function(string_vector){
      str_split(string_vector, " |-|: ") %>% 
        do.call(rbind.data.frame, .) %>% 
        magrittr::set_names(c("low", "high", "char", "str")) %>% 
        mutate(across(low:high, as.numeric)) %>% 
        mutate(char_count = str_count(str, char)) %>%
        mutate(valid = char_count >= low & char_count <= high) %>% 
        summarise(total_valid = sum(valid))
    }

    password_valid(input)

    ##   total_valid
    ## 1         422

## Challenge 2

The numbers actually describe positions in the password, not counts.
Password is valid if either numbered position contains the character. We
can modify the validity checks using the base `substr()` function to get
characters at the required positions. Since exactly one of the positions
is required to have the character, we need to combine the two
comparisons using `xor()` rather than the typical `|`.

    password_valid_positions <- function(string_vector){
      str_split(string_vector, " |-|: ") %>% 
        do.call(rbind.data.frame, .) %>% 
        magrittr::set_names(c("low", "high", "char", "str")) %>% 
        mutate(across(low:high, as.numeric)) %>% 
        mutate(char_1 = substr(str, low, low), char_2 = substr(str, high, high)) %>%
        mutate(valid = xor(char == char_1, char == char_2)) %>% 
        summarise(total_valid = sum(valid))
    }

    password_valid_positions(input)

    ##   total_valid
    ## 1         451

## Wrap Up

We can clean up these solutions a little to avoid repeating code:

    library(readr)
    library(stringr)
    library(dplyr)

    input <- read_lines(here::here("data/input_2.txt"))

    extract <- function(string_vector){
      str_split(string_vector, " |-|: ") %>% 
        do.call(rbind.data.frame, .) %>% 
        magrittr::set_names(c("low", "high", "char", "str")) %>% 
        mutate(across(low:high, as.numeric))
    }

    valid_between <- function(dataframe){
      dataframe %>% 
        mutate(char_count = str_count(str, char)) %>%
        mutate(valid = char_count >= low & char_count <= high) %>% 
        summarise(total_valid = sum(valid))
    }

    valid_at <- function(dataframe){
      dataframe %>% 
        mutate(char_1 = substr(str, low, low), char_2 = substr(str, high, high)) %>%
        mutate(valid = xor(char == char_1, char == char_2)) %>% 
        summarise(total_valid = sum(valid))
    }

    extract(input) %>% valid_between()

    ##   total_valid
    ## 1         422

    extract(input) %>% valid_at()

    ##   total_valid
    ## 1         451
