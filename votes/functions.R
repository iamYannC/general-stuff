
# LOAD AND CLEAN DATA -----------------------------------------------------

i_read_table <- function(url, class, k, yeshuv=NULL){
  #' Scrape and return table from a URL
  #' 
  #' This function scrapes a table from a given URL and returns it as a tibble.
  #' It also adds columns for the Knesset and the village, if specified.
  #' @param url Character. The URL to scrape data from.
  #' @param class Character. The HTML class of the table to scrape.
  #' @param k Character. The Knesset identifier.
  #' @param yeshuv Character. The village identifier. Default is NULL.
  #' 
  #' @return A tibble containing the scraped table with additional columns for Knesset and village.
  #' 
  #' @examples
  #' i_read_table(url = "https://votes21.bechirot.gov.il/",class = ".TableData",k = "21")
  
  
  read_html(url) |>
    html_nodes(class) |>
    html_table() %>%
    .[[1]] |>
    as_tibble() |>
    mutate(knesset = as.character(k), yeshuv = if(is.null(yeshuv)) yeshuv else as.character(yeshuv))
}

national_func <- function(k,pattern = TRUE){
  #' Get national election results by Knesset number
  #'
  #' This function retrieves national election results for a given Knesset number.
  #' It constructs the URL based on the provided Knesset number and scrapes the
  #' relevant data using the `i_read_table` function.
  #'
  #' @param k Character. The Knesset number.
  #' @param pattern Logical. If TRUE, use ".TableData" class; otherwise, use ".ResultsSummary" class. Default is TRUE.
  #'
  #' @return A tibble containing the national election results.
  #'
  #' @examples
  #' national_func("23")
  
  url <- paste0("https://votes",k,".bechirot.gov.il/")
  class <- if(pattern) ".TableData" else ".ResultsSummary"
  i_read_table(url, class, k)
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
  #' Store election data for each yeshuv
  #'
  #' This function retrieves and stores election data for a given village (yeshuv) and Knesset number.
  #' It constructs the URL based on the provided Knesset number and village ID, and scrapes the relevant data using the `i_read_table` function.
  #' 
  #' @param yeshuv Character. The village identifier. 
  #' @param k Character. The Knesset number.
  #' 
  #' @return A list containing two tibbles: general election data and pattern election data for the specified village.
  #' 
  #' @examples 
  #' get_yeshuv(5000, 23)
  #' get_yeshuv(5000) # For all Knesset elections.
  
  
  list_k <- vector('list',length(knesset)) |> set_names(paste0('k',knesset))
  for(k in paste0('k',knesset)){
    
    tmp <-  knesset_list[[k]][[paste0('id',yeshuv_id)]][[2]]
    if(nrow(tmp)==0){
      next
    }
    list_k[[k]] <- tmp |> mutate(across(pct:votes,\(x) as.numeric(str_remove_all(x, '[%,]'))),
                                 knesset = as.character(knesset)
                                 )
     
  }
  return(list_k |> list_rbind())
}


# Get yeshuv general across the years (can be modified to specific knesset)
yeshuv_general_years <-function(yeshuv_id, knesset = 21:25){
  #' Get voting results across the years for a specific yeshuv
  #'
  #' This function retrieves voting results across different years for a specific yeshuv (village).
  #' It constructs a list of tibbles containing the election data for each specified Knesset number,
  #' and combines them into a single tibble.
  #'
  #' @param yeshuv_id Character. The identifier of the yeshuv.
  #' @param knesset Numeric vector. The range of Knesset numbers to retrieve data for. Default is 21:25.
  #'
  #' @return A tibble containing the voting results for the specified yeshuv across the specified Knesset numbers.
  #'
  #' @examples
  #' yeshuv_general_years(5000)
  list_k <- vector('list',length(knesset)) |> set_names(paste0('k',knesset))
  
  for(k in paste0('k',knesset)){
    
    tmp <- knesset_list[[k]][[paste0('id',yeshuv_id)]][[1]]
    
    if(nrow(tmp)==0){
      next
    }
    
    list_k[[k]] <-
      tmp |> mutate(across(everything(),\(x) as.numeric(str_remove_all(x, '[%,]'))),
                    across(knesset:yeshuv, as.character))
  }
  return(list_k |> list_rbind())
}




# ANALYSIS ----------------------------------------------------------------

# Get consistency in voting pattern across all yeshuvim
voting_consistency <- function(data=voting_pattern, party_id,pop_threshold = 0,n, no_filter = F ){
  if (!no_filter) {
      tmp <- data |> filter(id == party_id)
      } else {
      tmp <- data
      }
  tmp <- tmp |>
    left_join(yesh) |> 
    filter(pop > pop_threshold) |>
    mutate(
      SD_pct = sd(pct),
      .by = yeshuv, .after = pct
    )
  yeshuvim <- tmp |> distinct(yeshuv,.keep_all=T) |> slice_max(SD_pct, n = n) |> pull(yeshuv)
  
  tmp |> filter(yeshuv %in% yeshuvim)
}


# PLOT --------------------------------------------------------------------

# Plot consistency in voting pattern for party_id
  # in n most inconsistent villages
  # with population size larger than pop_threshold
plot_consistency <- function(party_id,n,pop_threshold,lwd = 1.5){
  
  national_txt <- 'All'
  
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


plot_compare_yeshuvim <- function(party_id, yeshuvim,lwd = 1.5,
                                  show_national = FALSE){
  
  # Filter the data to the chosen cities (id) and the selected party
  df <- 
    voting_pattern_with_pop |> 
    filter(id == party_id , (yeshuv %in% c(yeshuvim,show_national*999) )) |> 
    mutate(knesset = as.character(knesset),
           line = yeshuv == 999
    )
  
  
  # Here I'm assigning colors to each city,
  # making sure that the national avg is gray
  colors <- scales::hue_pal()(length(yeshuvim) + 1)
  names(colors) <- unique(c(national_txt, df$name))
  colors[national_txt] <- "gray50"  
  
  x_discrete <- length(unique(df$knesset)) - 0.4 # just a twick to make the x axis nice
  
  if(length(yeshuvim) == 2){
    title_ <- glue("Comarison of voting percentage to **{party_id}** between **{df$name[df$yeshuv==yeshuvim[1]][1]} & {df$name[df$yeshuv==yeshuvim[2]][1]}**")
  } else {
    title_ <-glue("Comparison of voting percentage to **{party_id}**")
  }
  
  
  df |> ggplot(aes(
    x = knesset,
    y = pct,
    group = name, color = name
  )) +
    geom_line(aes(linetype = line, alpha = !line),
              lwd = lwd,show.legend = F) +
    geom_point(show.legend = T) +
    coord_cartesian(clip = 'on',
                    xlim = c(1.5,x_discrete)) +
    scale_linetype_manual(values = c(1,2)) +
    scale_color_manual(values = colors) +
    scale_alpha_manual(values = c(0.4,1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    labs(x = 'Knesset',
         y = glue("Percentage of votes to **{party_id}**"),
         title = title_,
         caption = '*Data: gov.il, Election to Knesset 21<sup>st</sup> to 25<sup>th</sup>.*'
    ) +
    guides(linetype = 'none', alpha = 'none', 
           color = if (length(yeshuvim) <= 10) guide_legend(title = NULL,
                                                            direction = 'horizontal',
                                                            position = 'top') else 'none'
    ) +
    theme(axis.title.y = element_marquee(),
          legend.text = element_text(size = 15),
          legend.key.width = unit(4, "mm"),
          legend.key.size = unit(-10,'mm'),
          legend.justification = 'left',
          legend.margin = margin(t = 0, b = 0),  # Reduce top and bottom margins
          plot.caption = element_marquee(hjust = 0,
                                         lineheight = 0.2),
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
          # plot.title.position = "plot",          # Align title with plot edges
          plot.title = element_marquee(width = 1,
                                       lineheight = 1.5,
                                       margin = margin(b = 5)), # Reduce bottom margin of title
          
    ) 
  
}

plot_change_yeshuv_multy <- function(df = voting_pattern,
                                     party_id, n = Inf,
                                     pop_threshold = 5000,
                                     knesset_= c(21,25),
                                     filter_district = NULL, lwd = 1.5, hide = NULL
){
  # IT FILTERS FOR BEER SHEBA! #
  
  max_pop <- df |>
    filter(knesset %in% knesset_,
           (id %in% party_id | id_eng %in% party_id),
           if( !is.null(filter_district) ){ district_2 %in% filter_district} else {TRUE} # hack to filter district only if provided (or value is not ALl if it goes to shiny..)
    ) |> pull(pop) |> max(na.rm = T)
  
  pop_threshold <- ifelse(pop_threshold > max_pop, round(max_pop * 0.9),
                          pop_threshold
  )
  
  df <-  
    df |>
    filter(knesset %in% knesset_,
           pop > pop_threshold,
           (id %in% party_id | id_eng %in% party_id),
           if( !is.null(filter_district) ){ district_2 %in% filter_district} else {TRUE} # hack to filter district only if provided (or value is not ALl if it goes to shiny..)
    ) |> 
    mutate(pct_diff = last(pct) - first(pct),
           .by = c(yeshuv,id), .after = pct) |> 
    mutate(more_votes = pct_diff > 0, .after = pct_diff)
  
  names_top_n <- df |> distinct(name,pop) |> slice_max(pop, n = n, with_ties = F) |> pull(name)
  
  
  df_flt <- df |> filter(name %in% names_top_n)
  
  
  df_start <- df_flt |> filter(knesset == min(knesset)) |> 
    mutate(name = ifelse(row_number() %% 2 == 1,
                         name, NA
    ),.by = id)
  
  df_end <- df_flt |>
    filter(knesset == max(knesset)) |> 
    mutate(name = ifelse(row_number() %% 2 == 0,
                         name, NA
    ),.by = id)
  # Here I'm assigning colors to each city based on change in pct to that party
  colors <- c('seagreen','orange3')
  if(!is.null(hide)){
    if(hide == 'positive'){
      colors <- c('grey70', 'orange3')
    } else if(hide == 'negative'){
      colors <- c('seagreen', 'grey70')
    }
  }
  
  x_discrete <- length(unique(df$knesset)) - 0.45 # just a twick to make the x axis nice
  
  if(length(party_id) == 1){
    title_ <- glue::glue("Changes in voting to **{party_id}** *(Population > {scales::comma(pop_threshold)})*.")
    facets <- NULL
    y_axs_lab <- glue("Percentage of votes to **{party_id}**")
  } else{
    title_ <- glue::glue("Changes in voting to multiple parties *(Population > {scales::comma(pop_threshold)})*.")
    facets <- facet_wrap(vars(id),scales = 'free')
    y_axs_lab <- "Percentage of votes"
  }
  
  
  static_gg <-  
    df_flt |> 
    ggplot(aes(x = as.character(knesset),
               y = pct,data_id = yeshuv,
               group = yeshuv, color = more_votes
    )) +
    ggiraph::geom_line_interactive(lwd = lwd,show.legend = F
    ) +
    geom_point(show.legend = F) +
    ggrepel::geom_text_repel(data = df_start,
                             aes(label = name),
                             nudge_x = -0.7,
                             direction = 'y',
                             size = 5,
                             nudge_y = 0.5,
                             hjust = 1
    ) +
    ggrepel::geom_text_repel(data = df_end,
                             aes(label = name),
                             nudge_x = 190.4,
                             direction = 'y',
                             size = 5,
                             nudge_y = 0.5,
                             hjust = 0
    ) +
    facets +
    coord_cartesian(clip = 'on', xlim = c(1.5,x_discrete)) +
    scale_color_manual(breaks = c(T,F), values = colors) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    labs(x = 'Knesset', y = y_axs_lab, title = title_,
         caption = '*Data: gov.il, Election to Knesset 21<sup>st</sup> to 25<sup>th</sup>.*'
    ) +
    guides(linetype = 'none', alpha = 'none', color = 'none'
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
    ) |> suppressWarnings()
  return(static_gg)
  # ggiraph::girafe(ggobj = static_gg) |> ggiraph::girafe_options(ggiraph::opts_hover(css = "background-color: #FF0000;"))
}
