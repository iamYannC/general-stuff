library(tidyverse)

df <- readxl::read_xlsx('data/voting_pattern.xlsx',sheet = 1)
df <- df |> select(1:5,9,11)
rich_data <- readxl::read_xlsx(('data/voting_general.xlsx'),sheet = 1) |> 
  select(name,knesset,can_vote,pop,yeshuv)
rich_data2 <- readxl::read_xlsx(('data/yesh.xlsx'),sheet = 1) |> select(name,size,pop) |> 
  mutate(pop = as.integer(pop)) |> drop_na()

votes_to_mandate <- \(votes){
  votes/sum(votes) * 120
}

true_vals <- df |> filter(name == "All") |>
  select(knesset, id_eng,party, mandate_true = mandate)

df_mandates <- df |>
  filter(name != "All") |>
  left_join(true_vals, by = c("knesset", "id_eng", "party")) |>
  mutate(
    mandate       = votes_to_mandate(votes),
    distance_raw  = abs(mandate - mandate_true),
    distance_rank = abs(rank(mandate) - rank(mandate_true)),
    .by = c(knesset, name)
  )
  
distances <- df_mandates |> 
  summarise(footrule = mean(distance_raw),
            footrule_rank = mean(distance_rank),
            .by = c(knesset, name)) |> 
  arrange(footrule)

# raw footrule prefers bigger parties. the difference in smaller parties is usually tiny anyway.
hist(distances$footrule,breaks = 100, xlab = "footrule distance: mean(abs(estimate - true))",
     main = "Footrule distribution across cities x knesset")

# ranked distance measures how well the city elected the parties by order, thus puts equal weight on parties, makes small parties equelly important
hist(distances$footrule_rank,breaks = 100, xlab = "ranked footrule distance: mean(abs(rank(est) - rank(true)))",
     main = "Ranked footrule distribution across cities x knesset"
     )


# show --------------------------------------------------------------------

pt <- c(25,'פתח תקווה')
tza <- c(23,'צפרירים')
now_use <- pt

true_to_gg <- true_vals |> filter(knesset==now_use[1]) |>
  mutate(name = 'ארצי') |> select(name,id_eng,mandate = mandate_true,party)

df_mandates |> filter(knesset==now_use[1], name ==now_use[2]) |> 
  select(name,id_eng,mandate,party) |>
  
  bind_rows(true_to_gg) |> 
  mutate(mandate = ifelse(mandate < 1,0,mandate)) |> 
  filter(mandate>0) |> 
  ggplot(aes(x = mandate, y = fct_reorder(party,mandate), fill = name)) +
  geom_col(position = 'dodge2', width = 0.8) +
  geom_segment(x = 4,xend=4,y=0,yend=3, lty = 2, alpha = 0.05)+
  labs(x = 'מנדטים',
       y = NULL,
       fill = NULL,
       title = paste("דפוס הצבעה | כנסת",now_use[1])
       ) +
  theme(axis.text.y = element_text(size = rel(1.5)))


# Control for size
# footrule captures how a city is similar to national results, but big cities naturally effect the nationa results more than small ones
# conduct analysis while controlling for city size (range, obviously)
distances |> left_join(rich_data |> select(-yeshuv),by = c('knesset','name')) |>
  left_join(rich_data2) |> 
  slice_min(footrule, n = 1, by = size) |> view()
