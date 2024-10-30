library(tidyverse)
library(rvest)

library(glue)
library(marquee)
# library(furrr) # library(readxl) # library(writexl)

source('votes/functions.R')
theme_set(theme_classic(base_size = 12))

# Data from https://boardsgenerator.cbs.gov.il/pages/WebParts/YishuvimPage.aspx?mode=Yeshuv#
yesh <- readxl::read_xlsx("votes/yesh.xlsx",col_names = 
                            c("id", "name", "district", "district_2", "type", "municipal_status",
                              "natural_region", "pop", "jews_others", "jews", "arabs","authority_cluster")
) %>% .[-c(1:2),]

knesset <- 21:25

knesset_list <- map(knesset,\(x) map(yesh[[1]], \(y) get_yeshuv(y, x)),
                    .progress = TRUE)
write_rds(knesset_list, "votes/knesset_list.rds")

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

k_df <- map_dfr(knesset, national)
colnames(k_df) <- c('party','id','mandate','pct','votes','knesset')
k_df <- k_df |> mutate(pct = parse_number(pct), votes = parse_number(votes))

# One big table with all yeshuvim, all years. voting patterns
voting_patterns <- furrr::future_map_dfr(yesh[[1]],yeshuv_pattern_years,
                                         .progress =TRUE
)

# One big table with all yeshuvim, all years. general results
voting_general <- furrr::future_map_dfr(yesh[[1]],yeshuv_general_years,
                                         .progress =TRUE
)

# Combine general data by Yeshuv and Knesset
data_yeshuv <- voting_consistency(party_id,n=Inf,pop_threshold = t) |> select(-1,-3,-5) |> 
  distinct() |> mutate(id = party_id) |> left_join(voting_general |> mutate(yeshuv = as.character(yeshuv)))


walk2( list(data_yeshuv,voting_patterns,voting_general), c('general_full','pattern','general'),
       \(df,name)
       writexl::write_xlsx(df,glue('votes/data/{name}.xlsx')
                           )
       )






# toy example

k_df |> filter(id == 'מחל') |> select(-1,-2) |> 
  pivot_longer(-knesset) |> filter(value < 1e4) |> 
  ggplot(aes(x = knesset, y = value, color = name)) + 
  geom_line(aes(group = name))

k_df |> filter(id == 'מחל') |> select(-1,-2) |> 
  pivot_longer(-knesset) |> filter(value > 1e4) |> 
  ggplot(aes(x = knesset, y = value)) + geom_col(position = 'dodge2')


# ChatGPT translation for party names & ID -- 39 in total, based on Election 24
{
  party_name_eng <- c(
    "Likud under the Leadership of Benjamin Netanyahu for Prime Minister",
    "Yesh Atid under the Leadership of Yair Lapid",
    "The Sephardic Torah Guardians Movement of the Late Rabbi Ovadia Yosef",
    "Blue and White under the Leadership of Benny Gantz",
    "Yamina under the Leadership of Naftali Bennett",
    "Labor Party under the Leadership of Merav Michaeli",
    "United Torah Judaism and Shabbat – Agudat Yisrael – Degel HaTorah",
    "Yisrael Beiteinu under the Leadership of Avigdor Lieberman",
    "Religious Zionism under the Leadership of Bezalel Smotrich",
    "The Joint List (Hadash, Ta'al, Balad)",
    "New Hope under the Leadership of Gideon Sa'ar for Prime Minister",
    "Meretz – The Left of Israel",
    "The United Arab List",
    "The New Economy under the Leadership of Professor Yaron Zelekha",
    "Rafa – Just Health under the Leadership of Doctor Aryeh Avni",
    "The Pirates",
    "You and I – The Israeli People’s Party",
    "Hope for Change",
    "The Social Explosion - Pensioners",
    "Justice Reform – For Judicial System Reform",
    "Tzomet – Independents, Farmers, Villages",
    "Whole Nation Party – under the Leadership of Rabbi Haim Amsalem",
    "New Order for Electoral and Governmental Reform in Israel under Adv. Avital Ofek",
    "Kama – Advancement of the Status of the Individual",
    "Noam Kolman, Liron Ofri, and Suli Wolf – The Impossible is Possible",
    "The Jewish Heart under the Leadership of Eli Yosef",
    "Ourselves – Independents and Liberals",
    "Biblical Bloc Party",
    "New World under the Leadership of Yoram Edri",
    "The Partnership Alliance for National Unity led by Captain B. Shlayan",
    "The Israelis",
    "Shama Party under the Leadership of Naftali Goldman",
    "Da'am – Green Economy One State",
    "Social Leadership",
    "Ma'an (Together) for a New Era",
    "Arrow",
    "\"We\" – under the Leadership of Adv. Mosh Hogeg",
    "Human Dignity, under the Leadership of Adv. Arkady Pugach",
    "Democratic – Freedom, Equality, and Mutual Responsibility"
  )
  
  party_id_eng <- c("machal", "peh", "shas", "ken", "b", "emet", "g", "l", "t", "vda'am", 
                    "t", "meretz", "im", "yaz", "r", "pfz", "kach", "ran", "y", "katz", 
                    "tzatz", "raf", "kach", "n", "k", "ch", "tsi", "yak", "ni", "yin", 
                    "z", "ki", "tz", "ir", "tzak", "tzaf", "ner", "yaf", "rak")
  }
