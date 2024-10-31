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
national_func <- function(k){
  url <- paste0("https://votes",k,".bechirot.gov.il/")
  i_read_table(url, ".TableData", k)
}

# store election data for each yeshuv
get_yeshuv <- function(yeshuv, k) {
  url <- paste0("https://votes", k, ".bechirot.gov.il/cityresults?cityID=", yeshuv)
  return(
    list(
      yeshuv_general = i_read_table(url, ".ResultsSummary", k, yeshuv),
      yeshuv_pattern = i_read_table(url, ".TableDataBox", k, yeshuv)  
    )
  )
}

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


# Get yeshuv general across the years (can be modified to specific knesset)
yeshuv_general_years <-function(yeshuv_id, knesset = 21:25){
  #' Get voting results across the years for a specific yeshuv
  #' 
  #' Since the 4th column can be either numeric or character, there is some error handling
  list_k <- vector('list',length(knesset)) |> set_names(paste0('k',knesset))
  
  for(k in paste0('k',knesset)){
    
    tmp <- knesset_list[[k]][[paste0('id',yeshuv_id)]][[1]]
    
    if(nrow(tmp)==0){
      next
    }
    
    list_k[[k]] <- 
      tmp |> 
      mutate(across(everything(),\(x) if (is.character(x))
        parse_number(x) else x))  
  }
  return(list_k |> list_rbind())
}

# Get consistency in voting pattern across all yeshuvim
voting_consistency <- function(party_id,pop_threshold = 0,n, no_filter = F ){
  if (!no_filter) {
      tmp <- voting_patterns |> filter(id == party_id)
      } else {
      tmp <- voting_patterns
      }
  tmp <- tmp |>
    left_join(yesh) |> 
    filter(parse_number(pop) > pop_threshold) |>
    mutate(
      SD_pct = sd(pct),
      pop = parse_number(pop),
      .by = yeshuv, .after = pct
    )
  yeshuvim <- tmp |> distinct(yeshuv,.keep_all=T) |> slice_max(SD_pct, n = n) |> pull(yeshuv)
  
  tmp |> filter(yeshuv %in% yeshuvim)
}

# Plot consistency in voting pattern for party_id
  # in n most inconsistent villages
  # with population size larger than pop_threshold
plot_consistency <- function(party_id,n,pop_threshold,lwd = 1.5){
  
  national_txt <- "ארצי"
  
  df <- bind_rows(
    ( national |> filter(id == party_id) |>
        select(id,pct,knesset) %>%
        mutate(SD_pct = sd(.$pct),
               name = national_txt
        )
    ),
    voting_consistency(party_id, n = n, pop_threshold = pop_threshold)
  ) |> mutate(line = is.na(party))
  
  
  # Here I'm assigning colors to each city,
  # making sure that the national avg is gray
  colors <- scales::hue_pal()(n + 1)
  names(colors) <- unique(c(national_txt, df$name))
  colors[national_txt] <- "gray50"
  
  x_discrete <- length(unique(df$knesset)) - 0.4 # just a twick to make the x axis nice
  
  df |> 
    ggplot(aes(x = as.character(knesset),
               y = pct,
               group = yeshuv, color = name
    )) +
    geom_line(aes(linetype = line, alpha = !line),
              lwd = lwd,show.legend = F) +
    geom_point(show.legend = T) +
    coord_cartesian(clip = 'on', xlim = c(1.5,x_discrete)) +
    scale_linetype_manual(values = c(1,2)) +
    scale_color_manual(values = colors) +
    scale_alpha_manual(values = c(0.4,1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    labs(x = 'Knesset',
         y = glue("Percentage of votes to **{party_id}**"),
         title = glue::glue("Least {n} consistent villages *(Population > {scales::comma(pop_threshold)})* in voting patterns to **{party_id}**"),
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
                                       lineheight = .01,
                                       margin = margin(0)),
          plot.caption = element_marquee(hjust = 0, lineheight = 0.2)
    ) 
   
}
