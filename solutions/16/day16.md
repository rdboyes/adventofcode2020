## Challenge 1

    library(readr)
    library(tibble)
    library(tidyr)
    library(dplyr)

    input <- read_lines(here::here("data/input_16.txt"))

    tibble(rules = input[1:20]) %>% 
      separate(rules, into = c("label", "low1", "high1", "low2", "high2"), sep = ":| or |-") %>% 
      mutate(across(low1:high2, as.numeric)) -> rules

    tibble(numbers = input[26:263]) %>% 
      separate_rows(numbers, sep = ",") %>% 
      mutate(id = rep(1:(263-25), each = 20)) %>% 
      mutate(across(numbers, as.numeric)) %>% 
      mutate(pass = between(numbers, min(rules$low1), max(rules$high2))) -> tickets 
      
    tickets %>% 
      filter(!pass) %>% 
      pull(numbers) %>% 
      sum()

    ## [1] 20013

## Challenge 2

    tickets %>% 
      group_by(id) %>% 
      summarize(ticket_valid = sum(pass) == 20) %>% 
      left_join(tickets, ., by = "id") %>% 
      filter(ticket_valid) %>% 
      pull(numbers) %>% 
      matrix(nrow = 20) %>% t() -> number_matrix

    rule_followed <- matrix(nrow = 20, ncol = 20) 

    for(i in 1:20){
      for(j in 1:20){
        rule_followed[i,j] <- sum((rules$low1[j] <= number_matrix[,i] & rules$high1[j] >= number_matrix[,i]) | (rules$low2[j] <= number_matrix[,i] & rules$high2[j] >= number_matrix[,i])) == 190
      }
    }

    pivot_longer(as.data.frame(rule_followed) %>% rowid_to_column(), -rowid, names_to = "rule", names_prefix = "V", values_to = "possible_match") %>% 
      filter(possible_match) -> rule_matches

    solution <- list()

    for(i in 1:20){
      rule_matches %>%
        group_by(rowid) %>% 
        summarize(num = n()) %>% 
        slice(., which.min(pull(., num))) %>% 
        pull(rowid) -> q 
        
      solution[[i]] <- filter(rule_matches, rowid == q)
      
      rule_matches <- filter(rule_matches, !rule == as.integer(solution[[i]]$rule[1]))
    }

    do.call(rbind.data.frame, solution) %>% filter(rule %in% 1:6) %>% 
      pull(rowid) %>% 
      slice(separate_rows(tibble(d = input[23]), d, sep = ","), .) %>% 
      pull() %>% 
      as.numeric() %>% 
      prod()

    ## [1] 5977293343129
