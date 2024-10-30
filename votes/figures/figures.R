source('votes/votes.R')
library(camcorder)
gg_record(dir = 'votes/figures', device = 'png', width = 10, height = 6, dpi = 200)


party_id <- 'מחל'
n <- 10
t <- 10000

voting_consistency(party_id,n=n,pop_threshold = t) |>
  ggplot(aes(y = fct_reorder(name,SD_pct),
             x = SD_pct,
  )) +
  geom_point(aes(size = pop), show.legend = F) +
  geom_text(aes(label = round(SD_pct,2),
                # size = SD_pct
  ),vjust = -1,show.legend = F) +
  geom_segment(
    aes(y = name, yend = name, x = 0, xend = SD_pct), lty = 2, alpha = 0.2
    
  ) +
  labs(
    title = glue::glue("Least {n} consistent villages *(Population > {scales::comma(t)})* in voting patterns to **{party_id}**
                       Dot size represents population size"),
    y = NULL, x = glue::glue("Standard Deviation in voting patterns to **{party_id}**"),
    caption = '*Data: gov.il, Election to Knesset 21<sup>st</sup> to 25<sup>th</sup>.*'
  ) +
  # scale_color_manual(values =  c('darkred','navyblue'))+
  theme(plot.title = marquee::element_marquee(width = 1,
                                              lineheight = .95),
        plot.caption = marquee::element_marquee(hjust = 0, lineheight = 0.2),
        axis.title.x =  marquee::element_marquee(hjust = .5,lineheight = 0) ,
        legend.position = 'none'
  )


bind_rows((k_df |> filter(id == party_id) |> select(id,pct,knesset) %>%
             mutate(SD_pct = sd(.$pct),
                    name = 'ארצי')
             ),
          voting_consistency(party_id,n=n,pop_threshold = t)
          ) |> mutate(line = is.na(party)) |> 
  ggplot(aes(x = as.character(knesset),
             y = pct,
             group = id, color = name
  )) +
  geom_line(aes(linetype = line, alpha = !line),
            lwd = 1.5,show.legend = F) +
  geom_point(show.legend = T) +
  coord_cartesian(clip = 'on', xlim = c(1.5,4.6)) +
  scale_linetype_manual(values = c(1,2)) +
  scale_alpha_manual(values = c(0.4,1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
  labs(x = 'Knesset',
       y = glue("Percentage of votes to **{party_id}**"),
       title = glue::glue("Least {n} consistent villages *(Population > {scales::comma(t)})* in voting patterns to **{party_id}**"),
       caption = '*Data: gov.il, Election to Knesset 21<sup>st</sup> to 25<sup>th</sup>.*'
       ) +
  theme(axis.title.y = element_marquee(),
        legend.text = element_text(size = 15),
        legend.margin = margin(0),
        plot.title = element_marquee(width = 1,
                                      lineheight = .01,
                                     margin = margin(0)),
        plot.caption = element_marquee(hjust = 0, lineheight = 0.2)
        ) +
  guides(linetype = 'none', alpha = 'none', 
         color = if (n <= 10) guide_legend(title = NULL,
                              direction = 'horizontal',
                              position = 'top') else 'none'
         ) 
  



# check correlation between consistency and pop size and income gdp
