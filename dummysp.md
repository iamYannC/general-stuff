# Dummy example on cracking the stock market


``` r
library(tidyverse)
```

    -- Attaching core tidyverse packages ------------------------ tidyverse 2.0.0 --
    v dplyr     1.1.4     v readr     2.1.5
    v forcats   1.0.0     v stringr   1.5.1
    v ggplot2   3.5.1     v tibble    3.2.1
    v lubridate 1.9.3     v tidyr     1.3.1
    v purrr     1.0.2     
    -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    x dplyr::filter() masks stats::filter()
    x dplyr::lag()    masks stats::lag()
    i Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

So… What if we were to randomly sample *k* companies out of the S&P 500
list? Is it possible that we could beat the average revenue of the whole
market? (**Yes…**) And what if we would pick another subset (be it *k*
as in number of companies or *p* - portion of the market)?

And if we pick a new subset, why dont we just pick MANY subsets? Well,
If we were to pick ALL possible subsets, the average revenue would be
the same as the average revenue of the whole market. This is a simple
soncept in statistics that states that the average of the sampling
distribution of the mean is equal to the population mean (Expected
value).

But stats tend to be borring… Let’s look at a dummy example and keep an
eye on the average as we go through this.

#### First, Let’s pick a few companies to play with!

``` r
company = c('google','microsoft','twitter','facebook','instagram','youtube','apple')
```

#### Now, let’s simulate the stock prices for 8 days (day 0 and 7 more)

``` r
set.seed(12)
sp7 <- 
  tibble(
  day = rep(0:7,length(company)),
  companies = rep(company,each=8),
  start_value = round(rnorm(length(company)*8,100,5),1),
  revenue = round(runif(length(company)*8,-3,3),1)
) |> 
  mutate(
    end_value = start_value + revenue,
    start_value = lag(end_value, default = first(start_value)),
    .by = companies)
```

``` r
sp7
```

    # A tibble: 56 x 5
         day companies start_value revenue end_value
       <int> <chr>           <dbl>   <dbl>     <dbl>
     1     0 google           92.6     1.2      93.8
     2     1 google           93.8    -2.5     105. 
     3     2 google          105.     -2.4      92.8
     4     3 google           92.8     0.5      95.9
     5     4 google           95.9    -0.6      89.4
     6     5 google           89.4     0.9      99.5
     7     6 google           99.5     0.7      99.1
     8     7 google           99.1     2.5      99.4
     9     0 microsoft        99.5     0.9     100. 
    10     1 microsoft       100.      2.3     104. 
    # i 46 more rows

### The average revenue across all companies is 0.318

``` r
round(mean(sp7$revenue),3)
```

    [1] 0.318

### Average revenue for each company

``` r
sp7 |> summarise(revenue = mean(revenue), .by = companies)
```

    # A tibble: 7 x 2
      companies revenue
      <chr>       <dbl>
    1 google     0.0375
    2 microsoft  1.5   
    3 twitter    0.312 
    4 facebook   1.04  
    5 instagram  0.125 
    6 youtube   -0.988 
    7 apple      0.2   

##### obviously, this average is also 0.18

``` r
sp7 |> summarise(revenue = mean(revenue), .by = companies) |> 
  summarise(revenue = mean(revenue))
```

    # A tibble: 1 x 1
      revenue
        <dbl>
    1   0.318

### but what if we would pick two companies and go only with them throught time? could we beat the average?

``` r
random_camponies <- sample(company,2)
```

#### attempt 1

``` r
paste0('Average revenue for ', paste(random_camponies,collapse = ' & '),':') 
```

    [1] "Average revenue for instagram & apple:"

``` r
sp7 |> filter(companies %in% random_camponies) |> 
  summarise(roi = mean(revenue), .by = companies) |> 
  print()|> 
  summarise(revenue = mean(roi))
```

    # A tibble: 2 x 2
      companies   roi
      <chr>     <dbl>
    1 instagram 0.125
    2 apple     0.2  

    # A tibble: 1 x 1
      revenue
        <dbl>
    1   0.163

and attempt 2

``` r
paste0('Average revenue for ', paste(random_camponies,collapse = ' & '),':') 
```

    [1] "Average revenue for instagram & apple:"

``` r
sp7 |> filter(companies %in% random_camponies) |> 
  summarise(roi = mean(revenue), .by = companies) |> 
  print()|> 
  summarise(revenue = mean(roi))
```

    # A tibble: 2 x 2
      companies   roi
      <chr>     <dbl>
    1 instagram 0.125
    2 apple     0.2  

    # A tibble: 1 x 1
      revenue
        <dbl>
    1   0.163

and 3?

``` r
paste0('Average revenue for ', paste(random_camponies,collapse = ' & '),':') 
```

    [1] "Average revenue for instagram & apple:"

``` r
sp7 |> filter(companies %in% random_camponies) |> 
  summarise(roi = mean(revenue), .by = companies) |> 
  print()|> 
  summarise(revenue = mean(roi))
```

    # A tibble: 2 x 2
      companies   roi
      <chr>     <dbl>
    1 instagram 0.125
    2 apple     0.2  

    # A tibble: 1 x 1
      revenue
        <dbl>
    1   0.163

Ok… Let’s try all possible combinations of a subset of 2 companies. KEEP
THINGS SIMPLE!!!

``` r
combinations <- combn(company,2) |> t() |> as_tibble() |> 
  set_names(c('company1','company2'))
```

    Warning: The `x` argument of `as_tibble.matrix()` must have unique column names if
    `.name_repair` is omitted as of tibble 2.0.0.
    i Using compatibility `.name_repair`.

Calculate the average revenue for each combination

``` r
get_revenue <- function(c1,c2){
  sp7 |> filter(companies %in% c(c1,c2)) |> 
    summarise(roi = mean(revenue), .by = companies) |> 
    summarise(revenue = mean(roi)) |> 
    pull()  
}
```

``` r
combinations <- mutate(combinations, revenue = map2_dbl(company1,company2,get_revenue))
x <- combinations |> pivot_longer(-revenue,names_to = 'pairs',values_to = 'company') |> 
  mutate(revenue_company=mean(revenue),.by = company) |> 
  mutate(is_better_than_avg = revenue_company > mean(sp7$revenue)) |> 
  select(company, revenue_company,is_better_than_avg) |> distinct()
x
```

    # A tibble: 7 x 3
      company   revenue_company is_better_than_avg
      <chr>               <dbl> <lgl>             
    1 google              0.201 FALSE             
    2 microsoft           0.810 TRUE              
    3 twitter             0.316 FALSE             
    4 facebook            0.618 TRUE              
    5 instagram           0.238 FALSE             
    6 youtube            -0.226 FALSE             
    7 apple               0.269 FALSE             

And of course, since we picked the entire space of possible
combinations, the mean of all possible combinations is the exact same
population mean.

``` r
 x |> print() |> 
  summarise(revenue = mean(revenue_company))
```

    # A tibble: 7 x 3
      company   revenue_company is_better_than_avg
      <chr>               <dbl> <lgl>             
    1 google              0.201 FALSE             
    2 microsoft           0.810 TRUE              
    3 twitter             0.316 FALSE             
    4 facebook            0.618 TRUE              
    5 instagram           0.238 FALSE             
    6 youtube            -0.226 FALSE             
    7 apple               0.269 FALSE             

    # A tibble: 1 x 1
      revenue
        <dbl>
    1   0.318

### What does it tell us?

I have no idea… but it was fun to play with this dummy example. Choosing
things randomly might yield cool or interesting results, but we should
always remember that coincidence may occur, and it is up to us to
understand the underlying reasons for the results we get. It might be
that someone was lucky. but then the other person might not be as lucky,
and if all people were to just randomly select 10% of the S&P 500
companies, the average revenue would be the same as the average revenue
of the whole market.
