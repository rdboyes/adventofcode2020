## Challenge 1

I think we can solve this one with a graph as well.

To build the graph from the input data: 1. Every row is a node 2. From
each acc or nop row, there is an edge connecting to the next row 3. Each
jmp node has an edge connecting to the number indicated

    library(readr)
    library(tidyr)
    library(dplyr)
    library(igraph)

    read_lines(here::here("data/input_8.txt")) %>% 
      as_tibble() %>% 
      separate(value, into = c("command", "num"), sep = " ") %>%
      mutate(num = as.numeric(num)) %>% 
      add_row(command = "end", num = 0) %>% 
      tibble::rowid_to_column("id") -> nodes

    edges <- nodes %>% 
        rename(from = id) %>%
        mutate(to = if_else(command == "jmp", from + num, from + 1)) %>% 
        filter(to <= max(nodes$id)) %>% 
        mutate(weight = 0) %>% 
        select(from, to, weight)

    command_net <- graph_from_data_frame(vertices = nodes, d = edges, directed = TRUE)

    all_simple_paths(command_net, from = 1, mode = "out") %>% 
      last() %>% as.vector() %>% 
      as_tibble() %>% 
      left_join(nodes, by = c("value" = "id")) %>% 
      filter(command == "acc") %>% 
      summarize(total = sum(num)) %>% 
      pull()

    ## [1] 1744

## Challenge 2

Here we build a graph that includes all possible changes, but gives the
changed edges a weight of 1. Finding the shortest path will find the
path with the fewest changes (in this case, 1):

    edges_2 <- nodes %>% 
        rename(from = id) %>%
        mutate(to = if_else(command == "nop", from + num, Inf)) %>% 
        mutate(to = if_else(command == "jmp", from + 1, Inf)) %>% 
        filter(to <= max(nodes$id)) %>% 
        mutate(weight = 1) %>% 
        select(from, to, weight) %>% 
        rbind(edges)

    all_possible <- graph_from_data_frame(d = edges_2, vertices = nodes, directed = TRUE)

    shortest_paths(all_possible, from = 1, to = 613)$vpath %>% 
      unlist() %>% as.vector() %>% as_tibble() %>% 
      left_join(nodes, by = c("value" = "id")) %>% 
      filter(command == "acc") %>% 
      summarize(total = sum(num)) %>% 
      pull()

    ## [1] 1174
