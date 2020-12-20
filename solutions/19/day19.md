## Challenge 1

We’ll call this approach the “big ass regex” approach.

    library(readr)
    library(tidyr)
    library(tibble)
    library(dplyr)
    library(stringr)

    input <- read_lines(here::here("data/input_19.txt"))

    rules <- tibble(rules = input[1:134]) %>% 
      separate(rules, into = c("rule_num", "rule_txt"), sep = ": ") %>% 
      mutate(across(rule_num, as.numeric)) %>% 
      arrange(rule_num)

    rule_0 <- rules$rule_txt[1]

    rules <- rules %>% slice(2:n())

    rule_to_regex <- function(rule_txt, rules_list = rules){
      rule_txt <- paste0(" ", rule_txt, " ")
      while(str_detect(rule_txt, "[0-9]")){
        replace_rule <- str_extract(rule_txt, "( [0-9]* )")
        rule_txt <- str_replace(rule_txt, replace_rule,
            paste0(" ( ", rules_list$rule_txt[as.numeric(replace_rule)], " ) ")) %>% 
          str_remove_all('"')
      }
      return(str_remove_all(rule_txt, " "))
    }

    str_detect(input[136:514], paste0("^", rule_to_regex(rule_0), "$")) %>% 
      sum()

    ## [1] 107

Translating the rules, 8: 42 | 42 8 means “8 can become one or more
42’s”, and “11: 42 31 | 42 11 31” means 11 can be replaced by (X &gt;=
1) X 42’s followed by X 31’s.

    rules_part2 <- rules

    rule_42 <- rule_to_regex(rules_part2$rule_txt[42], rules_list = rules_part2)
    rule_31 <- rule_to_regex(rules_part2$rule_txt[31], rules_list = rules_part2)

    rules_part2$rule_txt[8] <- paste0("(", rule_42, ")+")
    rules_part2$rule_txt[11] <- paste0("(((", rule_42, ")(", rule_31, "))|",
                                        "((", rule_42, ")(", rule_42, 
                                        ")(", rule_31, ")(", rule_31, "))|",
                                        "((", rule_42, ")(", rule_42, ")(", rule_42, 
                                        ")(", rule_31, ")(", rule_31, ")(", rule_31, "))|",
                                        "((", rule_42, ")(", rule_42, ")(", rule_42, ")(", rule_42, 
                                        ")(", rule_31, ")(", rule_31, ")(", rule_31, ")(", rule_31, ")))")

    str_detect(input[136:514], paste0("^", rule_to_regex(rule_0, rules_part2), "$")) %>% 
      sum()

    ## [1] 321
