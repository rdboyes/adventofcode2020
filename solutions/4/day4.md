# Challenge 1

    library(readr)
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(purrr)
    library(magrittr)

We’ve got a pretty messy file to read in. Never fear - between `stringr`
and `janitor` we can make short work of this. We’re using
`readr::read_file()` to read in the text as is.

    readr::read_file(here::here("data/input_4.txt")) %>% 
      str_split("\n\n") %>% 
      unlist() %>% 
      str_replace_all("\n", " ") %>% 
      str_split(" ") %>% 
      map_df(~str_split(., ":") %>% 
        do.call(cbind.data.frame, .) %>% 
        janitor::row_to_names(1)
      ) %>% 
      janitor::remove_constant(na.rm = TRUE) -> passport_data

    passport_data %>% 
      select(-cid) %>% 
      drop_na() %>% 
      nrow()

    ## [1] 210

## Challenge 2

We need to implement some validation rules. `pointblank` allows for
custom data validation:

    library(pointblank)

    passport_data %>% mutate(hgt_num = as.numeric(str_extract(hgt, "[0-9]+")),
                             hgt_unit = (str_extract(hgt, "[aA-zZ]+"))) %>% 
      mutate(hgt_cm = ifelse(hgt_unit == "in", hgt_num * 2.54, hgt_num)) %>% 
      create_agent() %>% 
      col_vals_between(vars(byr), 1920, 2002) %>% 
      col_vals_between(vars(iyr), 2010, 2020) %>% 
      col_vals_between(vars(eyr), 2020, 2030) %>% 
      col_vals_regex(vars(hcl), "^#[0-9a-f]{6}$") %>% 
      col_vals_in_set(vars(ecl), c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>% 
      col_vals_regex(vars(pid), "^[0-9]{9}$") %>% 
      col_vals_between(vars(hgt_cm), 150, 193) %>% 
      interrogate() -> valid_report

![report](https://github.com/rdboyes/adventofcode2020/blob/master/solutions/4/valid_report.png?raw=true)

    get_sundered_data(valid_report, type = "pass") %>% nrow()

    ## [1] 131
