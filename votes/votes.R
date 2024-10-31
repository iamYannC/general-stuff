library(tidyverse)
library(rvest)

library(glue)
library(marquee)
# library(furrr) # library(readxl) # library(writexl)

source('votes/functions.R')

# Data from https://boardsgenerator.cbs.gov.il/pages/WebParts/YishuvimPage.aspx?mode=Yeshuv#
yesh <- readxl::read_xlsx("votes/data/yesh.xlsx",col_names = 
                            c("yeshuv", "name", "district", "district_2", "type", "municipal_status",
                              "natural_region", "pop", "jews_others", "jews", "arabs","authority_cluster")
) %>% .[-c(1:2),]

knesset <- 21:25

('Once knesset_list ran, dont re-run. its heavy. load from rds') |> cli::col_red() |> cat()

# knesset_list <- map(knesset,\(x) map(yesh[[1]], \(y) get_yeshuv(y, x)),
#                     .progress = TRUE)
# write_rds(knesset_list, "votes/knesset_list.rds") ||| 

knesset_list <- read_rds("votes/knesset_list.rds")
# Structure:
  # 5 items (knesset numbers 21 to 25)
    # 1285 items (yeshuvim)
      # 2 items (yeshuv_general, yeshuv_pattern)
        # 1 tibble each
  

# Meaningful/eng names:
names(knesset_list) <- paste0('k',knesset)

for(k in names(knesset_list)){
  names(knesset_list[[k]]) <- paste0('id',yesh[[1]])
  
}

for(k in 1:length(knesset_list)){
  for(y in 1:length(knesset_list[[1]])){
    colnames(knesset_list[[k]][[y]][[1]]) <- c('can_vote','votes','pct_vote','valid_votes','invalid_votes','knesset','yeshuv')
  }
}

for(k in 1:length(knesset_list)){
  for(y in 1:length(knesset_list[[1]])){
    colnames(knesset_list[[k]][[y]][[2]]) <- c('party','id','pct','votes','knesset','yeshuv')
  }
}

# Get national results

national <- map_dfr(knesset, national_func)
colnames(national) <- c('party','id','mandate','pct','votes','knesset')
national <- national |> mutate(pct = parse_number(pct), votes = parse_number(votes))

('This will also take time...') |> cli::col_br_red() |> cat()

# One big table with all yeshuvim, all years. voting patterns
voting_patterns <- furrr::future_map_dfr(yesh[[1]],yeshuv_pattern_years,
                                         .progress =TRUE
)

# One big table with all yeshuvim, all years. general results
voting_general <- furrr::future_map_dfr(yesh[[1]],yeshuv_general_years,
                                         .progress =TRUE
) |> mutate(yeshuv = as.character(yeshuv))

national_txt <- "ארצי"

# Combine pattern data with population data
voting_pattern_with_pop <-
  voting_patterns |> 
  bind_rows(national) |> 
  mutate(yeshuv = ifelse(complete.cases(mandate),"999",yeshuv)) |> 
  left_join(
     (
       voting_consistency("no_filter", n = Inf, pop_threshold = 0, no_filter = T) |> select(-SD_pct)
     )
   ) |>mutate(name = ifelse(complete.cases(mandate), national_txt,name)) |> 
    relocate(name,knesset)
  
# Combine general data with population data
voting_general_with_pop <- 
  voting_general |> left_join(
    (
      voting_consistency('no_filter',n=Inf,pop_threshold = 0,no_filter = T) |>
      select(-party,-id,-pct,-SD_pct,-votes) |> 
      distinct() 
    )
      )


walk2( list(
            voting_general,
            voting_patterns,
            national # for now, dont include population data from cbs
            ),
       c('voting_general','voting_pattern','national'),
       \(df,name)
       writexl::write_xlsx(df,glue('votes/data/{name}.xlsx')
                           )
       )
