library(tidyverse)
library(rvest)

library(glue)
library(marquee)
# library(furrr) # library(readxl) # library(writexl)

source('votes/functions.R')

# Data from https://boardsgenerator.cbs.gov.il/pages/WebParts/YishuvimPage.aspx?mode=Yeshuv#
yesh <- readxl::read_xlsx("votes/data/yesh.xlsx",col_names = 
                            c("id", "name", "district", "district_2", "type", "municipal_status",
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

national <- map_dfr(knesset, national)
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
)

# Combine general data by Yeshuv and Knesset
voting_patterns_with_pop <- voting_consistency('no_filter',n=Inf,pop_threshold = 0,no_filter = T) |> select(-1,-3,-5) |> 
  distinct() |> left_join(voting_general |> mutate(yeshuv = as.character(yeshuv)))


walk2( list(voting_patterns_with_pop,
            voting_patterns,
            voting_general,
            national # for now, dont include population data from cbs
            ),
       c('pattern_full_data','pattern','general','national'),
       \(df,name)
       writexl::write_xlsx(df,glue('votes/data/{name}.xlsx')
                           )
       )
