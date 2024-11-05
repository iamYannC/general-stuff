# for noam
# Filter the data to the chosen cities (id) and the selected party
noam_df <- 
  voting_pattern_with_pop |>
  filter(district_2 == 'באר שבע') |>
  mutate(
    across(jews_others:arabs,\(x) parse_number(x, na = '-')),
    across(jews_others:arabs, \(x) ifelse(is.na(x),0,x))
    )

# first: make sure that the type and the actual population size correspond
noam_df |> mutate(
  arab_by_type = str_detect(type,'לא יהודיים'),
  arab_by_count = arabs > (jews+jews_others)
  ) |> filter(
    arab_by_count !=arab_by_type
  ) 
# They do. we can trust the type attribute

noam_df <- noam_df |> mutate(
  is_arab = str_detect(type,'לא יהודיים'),
  knesset = as.character(knesset),
) |> 
  janitor::remove_constant()


# EDA
noam_df |> 
  ggplot(aes(log(pop), fill = is_arab))+geom_histogram()+
  facet_wrap(vars(is_arab),scales = 'free')

party_id <- 'פה'
log_pop_quantile <- 5000 # 0.8Q or just use a constant... say 5000
heb <- 
  glue("
דפוסי הצבעה ל{party_id} ביישובים גדולים באזור באר שבע.
יישובים עם אוכלוסיה גדולה מ-{log_pop_quantile} נכללים בניתוח.
       ")

noam_df |> 
  filter(id == party_id,
         is_arab,
         pop > log_pop_quantile) |> 
  mutate(color = name == 
           (noam_df |> filter(id == party_id,is_arab,pop > log_pop_quantile) |> slice_max(pct) |> pull(name))
           ) |> 
  ggplot(aes(
  x = knesset,
  y = pct,
  group = name, color = color
)) +
  geom_line_interactive(lwd = 1.2,show.legend = F) +
  geom_point(show.legend = F) +
  annotate('text', x = 1.4, y = 18, size = 5, hjust = 0, color = 'tomato2',
                         label = glue("{noam_df |> filter(id == party_id,is_arab,pop > log_pop_quantile) |> slice_max(pct) |> pull(name)},
                                      pop: {noam_df |> filter(id == party_id,is_arab,pop > log_pop_quantile) |> slice_max(pct) |> pull(pop) |> scales::comma()}")
                       ) +
  coord_cartesian(clip = 'on',
                  xlim = c(1.5,4.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  scale_color_manual(values = c('grey30', 'tomato2')) +
  ggtext::geom_richtext(show.legend = F, size = 7, 
                        color = 'grey10',hjust = 0,
                        x = 2.8, y = 15,
    label = glue("**Voting patterns in Arab towns in Be'er Sheva district<br>with population greater than {scales::comma(log_pop_quantile)}.**<br>
                 *Party: {party_id}*")
  ) +
  labs(x = 'Knesset',
       y = glue("Percentage of votes to **{party_id}**"),
       # title = glue("**Voting patterns in Arab towns in Be'er Sheva district with population greater than {scales::comma(log_pop_quantile)}.**"),
       caption = '*Data: gov.il, Election to Knesset 21<sup>st</sup> to 25<sup>th</sup>.*'
       # subtitle = glue("Party: {party_id}")
  ) +
  guides(linetype = 'none', alpha = 'none', 
         color = 'none'
  ) +
  theme(axis.title.y = element_marquee(),
        axis.text.y = element_text(size = 13,face='bold'),
        plot.caption = element_marquee(hjust = 0,
                                       lineheight = 0.2),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
        plot.title.position = "plot",          # Align title with plot edges
        plot.title = element_marquee(width = 1,
                                     lineheight = 1.5,
                                     margin = margin(l = 8,b = 0)), # Reduce bottom margin of title
        plot.subtitle = element_text(size = 13,
                                     face = 'italic',
                                     margin = margin(l = 8, t = 0))
  ) 

# What happend to that village?
name_ <- noam_df |> filter(id == party_id,is_arab,pop > log_pop_quantile) |> slice_max(pct) |> pull(name)
yeshuv_ <- noam_df$yeshuv[noam_df$name==name_] |> unique()

plot_change_yeshuv <- function(data, yeshuv_, party_ids, knesset_,...){
  
  x_discrete <- length(knesset_) - 0.4 # just a twick to make the x axis nice
  yesh_name <- data |> filter(yeshuv == yeshuv_) |> pull(name) |> unique()
  
  
  data |> 
  filter(yeshuv == yeshuv_ ,
         id %in% party_ids,
         knesset %in% knesset_
         ) |> 
  select(party,knesset,yeshuv, pct, id) |> 
  reshape2::melt() |> 
  ggplot(aes(x = knesset,
             y = value,
             group = id,
             color = party))+
  geom_point(...)+
  geom_line(...,show.legend = F)+
    coord_cartesian(clip = 'on',
                    xlim = c(1.5,x_discrete)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    labs(x = 'Knesset', color = NULL,
         y = glue("Percentage of votes**"),
         title = glue("**Shift in voting in the village {yesh_name}**"),
         caption = '*Data: gov.il, Election to Knesset 21<sup>st</sup> to 25<sup>th</sup>.*'
    ) +
    theme(axis.title.y = element_marquee(),
          axis.text.y = element_text(size = 13,face='bold'),
          plot.caption = element_marquee(hjust = 0,
                                         lineheight = 0.2),
          legend.margin = margin(l = -150),
          plot.margin = margin(t = 0, r = 5, b = 5, l = 5),
          legend.text = element_text(size = 13),
          legend.text.position = 'left',
          legend.position = 'top',
          plot.title.position = "plot",          # Align title with plot edges
          plot.title = element_marquee(width = 1,
                                       lineheight = 1.5,
                                       margin = margin(l = 8,b = -55, r = -100)) # Reduce bottom margin of title
    )
}


plot_change_yeshuv(noam_df, yeshuv_, c('פה','מחל','שס','ד'),
                   c(22,23,24),
                   lwd = 1.2, size = 4)


plot_change_yeshuv_multy <- function(party_id,n=Inf, pop_threshold=5000,
                              lwd = 1.5, hide = NULL, 
                              knesset_=c(21,25)){
  # IT FILTERS FOR BEER SHEBA! #
  
  
  df <- bind_rows(
    ( national |> filter(id == party_id) |>
        select(id,pct,knesset) %>%
        mutate(SD_pct = sd(.$pct),
               name = national_txt
        )
    ),
    voting_consistency(noam_df,party_id, n = n, pop_threshold = pop_threshold)
  ) |>
    filter(knesset %in% knesset_) |> 
    mutate(pct_diff = last(pct) - first(pct),
           .by = yeshuv, .after = pct) |> 
    mutate(more_votes = pct_diff > 0, .after = pct_diff)
  
  
  # Here I'm assigning colors to each city based on change in pct to that party
  colors <- c('seagreen','orange3')
  if(!is.null(hide)){
    if(hide == 'positive'){
      colors <- c('grey70', 'orange3')
    } else if(hide == 'negative'){
      colors <- c('seagreen', 'grey70')
    }
  }
  
  x_discrete <- length(unique(df$knesset)) - 0.4 # just a twick to make the x axis nice
  
  df |> 
    ggplot(aes(x = as.character(knesset),
               y = pct,
               group = yeshuv, color = more_votes
    )) +
    geom_line(lwd = lwd,show.legend = F) +
    geom_point(show.legend = T) +
    coord_cartesian(clip = 'on', xlim = c(1.5,x_discrete)) +
    scale_color_manual(breaks = c(T,F), values = colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    labs(x = 'Knesset',
         y = glue("Percentage of votes to **{party_id}**"),
         title = glue::glue("Changes in voting to **{party_id}** *(Population > {scales::comma(pop_threshold)})*."),
         caption = '*Data: gov.il, Election to Knesset 21<sup>st</sup> to 25<sup>th</sup>.*'
    ) +
    guides(linetype = 'none', alpha = 'none', 
           color = if (n <= 10) guide_legend(title = NULL,
                                             direction = 'horizontal',
                                             position = 'top') else 'none'
    ) +
    theme(axis.title.y = element_marquee(),
          legend.text = element_text(size = 15),
          legend.key.width = unit(4, "mm"),
          legend.key.size = unit(4,'mm'),
          legend.justification = 'left',
          plot.title = element_marquee(width = 1,
                                       # lineheight = 1,
                                       margin = margin(0)),
          plot.caption = element_marquee(hjust = 0, lineheight = 0.2)
    ) 
  
}
plot_change_yeshuv_multy(party_id = 'פה',n = 20,pop_threshold = 0,lwd = 1.2,hide = 'negative')
   