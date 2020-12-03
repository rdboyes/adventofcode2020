## Challenge 1

Find the two numbers in the input file that sum to 2020, return their
product

    library(readr)

    input <- as.numeric(read_lines(here::here("data/input_1_1.txt"))) 

    two_numbers <- function(numbers, target){
      prod(intersect(numbers, target - numbers))
    }

    two_numbers(input, 2020)

    ## [1] 1013211

## Challenge 2

Find the *three* numbers that sum to 2020 this time. My first thought
was to apply the same kind of logic as the two number solution, but in a
loop with varying targets.

    three_numbers_ugly <- function(numbers, target){
      temp <- target - numbers
      for(i in 1:length(numbers)){
        if(length(intersect(numbers, temp - numbers[i])) > 0){
          return(prod(c(intersect(numbers, temp - numbers[i]), numbers[i])))
        }
      }
    }

    three_numbers_ugly(input, 2020)

    ## [1] 13891280

Since this is really the same logic, there’s a much cleaner-looking
version of this code that calls the two number function:

    three_numbers_cleaner <- function(numbers, target){
      targets <- target - numbers
      for(i in 1:length(targets)){
        if(two_numbers(numbers, targets[i]) > 1){
          return(two_numbers(numbers, targets[i]) * numbers[i])
        }
      }
    }

    three_numbers_cleaner(input, 2020)

    ## [1] 13891280

People always say for loops are bad in R, so lets try a version without
a loop using `purrr::map`.

    library(purrr)
    library(dplyr)

    three_numbers <- function(numbers, target){
      map_dbl(target - numbers, ~two_numbers(numbers, .)) %>% 
        prod() %>% 
        sqrt()
    }

    three_numbers(input, 2020)

    ## [1] 13891280

Which is the fastest?

    library(rbenchmark)

    benchmark("ugly" = {three_numbers_ugly(input, 2020)},
              "cleaner" = {three_numbers_cleaner(input, 2020)},
              "map" = {three_numbers(input, 2020)}) %>% 
      knitr::kable()

<table>
<thead>
<tr class="header">
<th style="text-align: left;"></th>
<th style="text-align: left;">test</th>
<th style="text-align: right;">replications</th>
<th style="text-align: right;">elapsed</th>
<th style="text-align: right;">relative</th>
<th style="text-align: right;">user.self</th>
<th style="text-align: right;">sys.self</th>
<th style="text-align: right;">user.child</th>
<th style="text-align: right;">sys.child</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">2</td>
<td style="text-align: left;">cleaner</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">0.06</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.07</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="even">
<td style="text-align: left;">3</td>
<td style="text-align: left;">map</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">0.48</td>
<td style="text-align: right;">8</td>
<td style="text-align: right;">0.49</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
<tr class="odd">
<td style="text-align: left;">1</td>
<td style="text-align: left;">ugly</td>
<td style="text-align: right;">100</td>
<td style="text-align: right;">0.06</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">0.06</td>
<td style="text-align: right;">0</td>
<td style="text-align: right;">NA</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

The function using `purrr::map` is slower by an order of magnitude,
partly because it doesn’t stop early if the solution is found.
