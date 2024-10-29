library(tidyverse)
library(rvest)
theme_set(theme_classic(base_size = 12))

# Read the tables from the election website for each Knesset (k) and village (yeshuv)
i_read_table <- function(url, class, k, yeshuv=NULL){
  read_html(url) |>
    html_nodes(class) |>
    html_table() %>%
    .[[1]] |>
    as_tibble() |>
    mutate(knesset = k, yeshuv = yeshuv)
}

# Get national results by Knesset number (k)
national <- function(k){
  url <- paste0("https://votes",k,".bechirot.gov.il/")
  i_read_table(url, ".TableData", k)
}

# Data from https://boardsgenerator.cbs.gov.il/pages/WebParts/YishuvimPage.aspx?mode=Yeshuv#
yesh <- readxl::read_xlsx("votes/yesh.xlsx",col_names = 
                            c("id", "name", "district", "district_2", "type", "municipal_status",
                              "natural_region", "pop", "jews_others", "jews", "arabs","authority_cluster"),
                          skip = 1
)

get_yeshuv <- function(yeshuv, k) {
  url <- paste0("https://votes", k, ".bechirot.gov.il/cityresults?cityID=", yeshuv)
  return(
    list(
      yeshuv_general = i_read_table(url, ".ResultsSummary", k, yeshuv),
      yeshuv_pattern = i_read_table(url, ".TableDataBox", k, yeshuv)  
    )
  )
}

knesset_list <- map(knesset,\(x) map(yesh[[1]], \(y) get_yeshuv(y, x)),
                    .progress = TRUE)

# Structure:
# 5 items (knesset numbers 21 to 25)
# 1285 items (yeshuvim)
# 2 items (yeshuv_general, yeshuv_pattern)
# 1 tibble each
knesset <- 21:25  

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

# Get yeshuv pattern across the years (can be modified to specific knesset)
yeshuv_pattern_years <-function(yeshuv_id, knesset = 21:25){
  #' Get voting pattern across the years for a specific yeshuv
  #' 
  #' Since the 4th column can be either numeric or character, there is some error handling
  list_k <- vector('list',length(knesset)) |> set_names(paste0('k',knesset))
  for(k in paste0('k',knesset)){
    
    tmp <-  knesset_list[[k]][[paste0('id',yeshuv_id)]][[2]]
    if(nrow(tmp)==0){
      next
    }
    # Needs to be converted to numeric values
    
    if(is.numeric(tmp[[4]])){
      tmp <- tmp |> mutate(across(3, \(x) if (is.character(x))
        parse_number(x) else x ))
    } else{
      tmp <- tmp |> mutate(across(3:4, \(x) if (is.character(x))
        parse_number(x) else x))  
    }
    list_k[[k]] <- tmp
  }
  return(list_k |> list_rbind())
}


# Now lets implement this using furrr for speed. one big table with all yeshuvim, all years.
voting_patterns <- furrr::future_map_dfr(yesh[[1]],yeshuv_pattern_years,
                                         .progress =TRUE
)


# Get consistency in voting pattern across all yeshuvim
party_id <- 'מחל'

sd_voting <- voting_patterns |> filter(id == party_id) |>
  mutate(id = as.character(yeshuv)) |> 
  reframe(SD_pct = sd(pct),
          SD_votes = sd(votes),
          .by = id
  ) |> 
  left_join(yesh)

sd_voting_gg <- sd_voting |> slice_max(SD_pct, n = 10) |> 
  select(name,SD_pct,pop,jews) |> 
  mutate(pop = as.numeric(pop),
         jews = parse_number(jews) > 100,
         jews = ifelse(is.na(jews)|!jews,F,T)
  )

heb_title <- 
  paste0(glue::glue(
    "עשרת היישובים עם דפוס ההצבעה התנודתי ביותר עבור {party_id}."),
    "{.darkred יישובים ערבים} ו{.navyblue יישובים יהודיים}, כאשר גודל הנקודה מייצג את גודל האוכלוסייה."
  )

sd_voting_gg |> ggplot(aes(x = fct_reorder(name,SD_pct),
                           y = SD_pct,
                           color = jews
)) +
  geom_point(aes(size = pop), show.legend = F) +
  geom_text(aes(label = round(SD_pct,2),
                # size = SD_pct
  ),vjust = -1,show.legend = F) +
  geom_segment(
    aes(x = name, xend = name, y = 0, yend = SD_pct), lty = 2, alpha = 0.2
    
  ) +
  labs(
    title = paste0(glue::glue("Least 10 consistent villages in voting patterns to **{party_id}**"),
                   "{.darkred **Arab**} and {.navyblue **Jewish**} villages, where dot size represents population size."),
    x = NULL, y = glue::glue("Standard Deviation in voting patterns to **{party_id}**"),
    caption = '*Data: gov.il, Election to Knesset 21<sup>st</sup> to 25<sup>th</sup>.*'
  ) +
  scale_color_manual(values =  c('darkred','navyblue'))+
  coord_flip(clip = 'on')+
  theme(plot.title = marquee::element_marquee(width = 1,
                                              lineheight = .5),
        plot.caption = marquee::element_marquee(hjust = 0, lineheight = 0.2),
        axis.title.x =  marquee::element_marquee(hjust = .5,lineheight = 0) ,
        legend.position = 'none'
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
