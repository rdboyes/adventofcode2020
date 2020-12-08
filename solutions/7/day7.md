## Challenge 1

This seems like the hardest challenge so far. Lets read in the data.

    library(readr)
    library(dplyr)
    library(stringr)
    library(tidygraph)
    library(purrr)
    library(igraph)
    library(tidyr)

    rules <- read_lines(here::here("data/input_7.txt")) %>% 
      as_tibble() %>% 
      separate(value, into = c("parent", "child"), sep = " bags contain ") %>% 
      separate_rows(child, sep = ", ") %>% 
      mutate(child = str_remove(child, pattern = " bags?.?")) %>% 
      separate(child, into = c("number", "child"), extra = "merge") 

    rules$number[rules$number == "no"] <- "0"

    nodes <- full_join(distinct(select(rules, label = parent)),
                       distinct(select(rules, label = child))) %>% 
      tibble::rowid_to_column("id")

    edges <- rules %>% 
      left_join(nodes, by = c("parent" = "label")) %>% 
      rename(to = id) %>% 
      left_join(nodes, by = c("child" = "label")) %>% 
      rename(from = id) %>% 
      filter(!number == "0") %>% 
      mutate(weights = as.numeric(number)) %>% 
      select(from, to, weights)

I’m thinking about the problem as a graph, so we’ll create the graph
here:

    rules_net <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

    shiny_gold <- nodes$id[nodes$label == "shiny gold"]

Once the data is represented as a graph, finding everything that is
connected to shiny gold is straightforward (subtract one for answer).

    distances(rules_net, v = shiny_gold, mode = "out") %>% 
      t() %>% 
      as_tibble() %>% 
      filter(!is.infinite(V1)) %>% 
      nrow()

    ## [1] 230

## Challenge 2

Now looking at the other direction to find the bags inside the shiny
gold bag. We need all simple paths rather than shortest paths because
there are some overlaps. We need a custom `path_distance()` function to
calculate “distance” as the product of the weights.

    path_distance <- function(vec){
      data.frame(to = as.vector(vec[1:length(vec)-1]), 
                 from = as.vector(vec[2:length(vec)])) %>% 
        left_join(edges) %>% 
        summarize(bag_count = prod(weights)) %>% 
        pull()
    }

    all_simple_paths(rules_net, from = shiny_gold, mode = "in") %>% 
      map_dbl(path_distance) %>% 
      sum()

    ## [1] 6683
