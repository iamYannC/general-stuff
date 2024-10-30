source('votes/votes.R')
library(camcorder)
gg_record(dir = 'votes/figures', device = 'png', width = 10, height = 6, dpi = 200)
theme_set(theme_classic(base_size = 12))

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


plot_consistency(party_id,n,t)

heb <- c('עם','ל','ג','מחל','אמת')
# check correlation between consistency and pop size and income gdp
map(heb, \(id) plot_consistency(id,8,t,lwd = 0.8))
