## Challenge 1

The problem is “Game of Life”-esque. Input format is similar to the
tobbogan problem.

    library(readr)
    library(tidyr)
    library(stringr)
    library(dplyr)
    library(purrr)
    library(tibble)

    input <- read_lines(here::here("data/input_11.txt")) %>% 
      str_replace_all("L", "0") %>% 
      strsplit("") %>% 
      do.call(rbind.data.frame, .) %>% 
      add_row() %>% add_row(.before = 1) %>% 
      mutate_all(as.numeric) %>% 
      mutate_all(as.logical) %>% 
      mutate(buffer = NA, buffer2 = NA) %>% 
      select(buffer, everything()) %>% 
      as.matrix()

    state_adjacent <- function(row, col, data){
      state <- data[row, col]
      if(is.na(state)){return(NA)}
      
      sum <- sum(data[(row - 1):(row + 1), (col - 1):(col + 1)], na.rm = TRUE)
      if(sum == 0){return(TRUE)}
      if(state == TRUE && sum >= 5){return(FALSE)}
      
      return(state)
    }

    run_sim <- function(state_function){
      state_list <- list()
      state_list[[1]] <- input
      i <- 1

      while(TRUE){
        state_list[[i + 1]] <- pmap_lgl(expand.grid(1:100, 1:100), ~state_function(.x, .y, state_list[[i]])) %>% 
          matrix(nrow = 100, ncol = 100)
        i <- i + 1
        if(isTRUE(all.equal(state_list[[i]], state_list[[i-1]]))){break}
      }

      return(state_list)
    }

    part1 <- run_sim(state_adjacent)
    sum(part1[[length(part1)]], na.rm = T)

    ## [1] 2470

## Challenge

    state_sightline <- function(row, col, data){
      state <- data[row, col]
      if(is.na(state)){return(NA)}
      
      check_direction <- function(data, row, col, h, v){
        while(is.na(data[row + v, col + h]) && between(row + v, 2, 99) && between(col + h, 2, 99)){
          row <- row + v
          col <- col + h
        }
        return(data[row + v, col + h])
      }
      
      pmap_lgl(expand.grid(c(-1,0,1), c(-1,0,1)), ~check_direction(data, row, col, .x, .y)) %>% 
        sum(na.rm = T) -> sum
      
      if(sum == 0){return(TRUE)}
      if(state == TRUE && sum >= 6){return(FALSE)}
      return(state)
    }

    part2 <- run_sim(state_sightline)
    sum(part2[[length(part2)]], na.rm = T)

    ## [1] 2259

## Bonus

    library(ggplot2)
    library(gganimate)

    colnames(part1[[1]]) <- colnames(part1[[2]])
    colnames(part2[[1]]) <- colnames(part2[[2]])

    map(part1, ~as.data.frame(.) %>% 
      rownames_to_column() %>% 
      pivot_longer(starts_with("V"), names_prefix = "V") %>% 
      mutate_all(as.numeric) %>% 
      ggplot() + 
        geom_tile(aes(x = name, y = rowname, fill = value)) +
        theme_void() +
        theme(legend.position = "none")) -> plots1 

    map(part2, ~as.data.frame(.) %>% 
      rownames_to_column() %>% 
      pivot_longer(starts_with("V"), names_prefix = "V") %>% 
      mutate_all(as.numeric) %>% 
      ggplot() + 
        geom_tile(aes(x = name, y = rowname, fill = value)) +
        theme_void() +
        theme(legend.position = "none")) -> plots2 

    animation::saveGIF(expr = {for(i in 1:length(plots1)){plot(plots1[[i]])}}, movie.name = "part1.gif")

    ## [1] TRUE

    animation::saveGIF(expr = {for(i in 1:length(plots2)){plot(plots2[[i]])}}, movie.name = "part2.gif")

    ## [1] TRUE
