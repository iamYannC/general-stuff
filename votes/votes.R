library(tidyverse)
library(rvest)

library(glue)
library(marquee)
# library(furrr) # library(readxl) # library(writexl)

source('votes/functions.R')
load_xlsx <- T

# CBS DATA ----------------------------------------------------------------

# Data from https://boardsgenerator.cbs.gov.il/pages/WebParts/YishuvimPage.aspx?mode=Yeshuv#
yesh <- readxl::read_xlsx("votes/data/yesh.xlsx",col_names = 
                            c("yeshuv", "name", "district", "district_2", "type", "municipal_status",
                              "natural_region", "pop", "jews_others", "jews", "arabs","authority_cluster")
) %>% .[-c(1:2),] |> mutate(across(pop:arabs,\(x) parse_number(x, na = '-')),
                            across(jews_others:arabs, \(x) ifelse(is.na(x),0,x)))

knesset <- 21:25

('Once knesset_list ran, dont re-run. its heavy. load from rds\n') |> cli::col_red() |> cat()

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

# VOTES DATA --------------------------------------------------------------

national_txt <- 'All'

if(!load_xlsx){
  # Get national results
  
  national_pattern <- map_dfr(knesset, national_func)
  colnames(national_pattern) <- c('party','id','mandate','pct','votes','knesset')
  national_pattern <- national_pattern |> mutate(pct = parse_number(pct), votes = parse_number(votes))
  
  national_general <- map_dfr(knesset,\(k) national_func(k,F)) |>
    mutate(across(everything(-knesset), parse_number))
  colnames(national_general) <- c('can_vote','votes','pct_vote','valid_votes','invalid_votes','knesset')
  
  
  ('This will also take time...\n') |> cli::col_br_red() |> cat()
  
  # One big table with all yeshuvim, all years. voting patterns
  voting_pattern <- furrr::future_map_dfr(yesh[[1]],yeshuv_pattern_years,
                                          .progress =TRUE
  )
  
  # Combine voting pattern data with population data
  voting_pattern <-
    voting_pattern |> 
    bind_rows(national_pattern) |> 
    mutate(yeshuv = ifelse(complete.cases(mandate),"999",yeshuv)) |> 
    left_join(yesh) |>
    mutate(name = ifelse(complete.cases(mandate), national_txt,name)) |> 
    relocate(name,knesset) |> 
    # Enrich pattern data with yeshuv-party-knesset level SD, which is constant for the same party and yeshuv across Knesset (up to K duplicayes: K = number of Knesset),
    # And with the difference in percentage of votes from the previous Knesset.
    mutate(SD_pct = sd(pct),
           diff_pct = pct - lag(pct,1,default = first(pct)),
           .by = c(yeshuv,id),
           .after = pct) |>  arrange(name,id,knesset)
  
  
  # One big table with all yeshuvim, all years. general results
  voting_general <- furrr::future_map_dfr(yesh[[1]],yeshuv_general_years,
                                          .progress =TRUE
  )
  
  # Combine general data with population data
  voting_general <-
    voting_general |> 
    bind_rows(national_general) |> 
    mutate(yeshuv = ifelse(is.na(yeshuv),"999",yeshuv)) |> 
    left_join(yesh) |>
    mutate(name = ifelse(yeshuv == 999, national_txt,name)) |> 
    relocate(name,knesset)
  
  
  # Transnationals to english
  id_tbl <- tibble(
    id_eng = c(
      "Emet", "Etz", "B", "G", "D", "Daam", "Wadaam", "Um", "Z", "Zi", "Zach", "Zak", "Zan", "Zan", "Zatz", 
      "T", "Tav", "Y", "Yaz", "Yin", "Yin", "Yif", "Yitz", "Yik", "Yir", "Kach", "Kachak", "Kaf", "Kafi", "Kach", 
      "Ken", "Kaf", "L", "Mahal", "Meretz", "N", "Nach", "Nan", "Nun", "Nez", "Nai", "Nach", "Naf", "Natz", "Nak", 
      "Nir", "Am", "P", "Pez", "Piy", "Pach", "Pan", "Patz", "Peh", "Tz", "Tzaz", "Tzai", "Tz", "Tzi", "Tzak", 
      "Tzan", "Tzaf", "Tzak", "K", "Kai", "Kach", "Ken", "Kan", "Kaf", "Katz", "R", "Raz", "Ran", "Raf", "Rak", 
      "Shas", "Tav"
    ),
    id = unique(voting_pattern$id)
  ) 
  voting_pattern <- voting_pattern |> left_join(id_tbl) |> relocate(id_eng, .after = id)
  rm(national_general, national_pattern)
  
} else{
  ('Loading existing, pre-proccessed data\n') |> cli::col_magenta() |> cat()
  
  voting_pattern <- readxl::read_xlsx("votes/data/voting_pattern.xlsx")
  voting_general <- readxl::read_xlsx("votes/data/voting_general.xlsx")
}

# SAVE --------------------------------------------------------------------

# If i didnt load xlsx, probably want to save it
if(!load_xlsx){
  walk2( list(
    voting_general,
    voting_pattern
  ),
  c('voting_general','voting_pattern'),
  \(df,name)
  writexl::write_xlsx(df,glue('votes/data/{name}.xlsx')
  )
  )
  
}
('Done!\n') |> cli::bg_br_green() |> cli::col_black() |>  cat()
