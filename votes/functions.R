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
    
    if(nrow(tmp)==0){
      next
    }
    
    list_k[[k]] <- 
      knesset_list[[k]][[paste0('id',yeshuv_id)]][[1]] |> 
      mutate(across(everything(),\(x) if (is.character(x))
        parse_number(x) else x))  
  }
  return(list_k |> list_rbind())
}

# Get consistency in voting pattern across all yeshuvim
voting_consistency <- function(party_id,pop_threshold = 0,n ){
  tmp <- 
    voting_patterns |> filter(id == party_id) |>
    mutate(id = as.character(yeshuv)) |> 
    left_join(yesh) |> 
    filter(parse_number(pop) > pop_threshold) |>
    mutate(
      SD_pct = sd(pct),
      pop = parse_number(pop),
      .by = id, .after = pct
    )
  ids <- tmp |> distinct(id,.keep_all=T) |> slice_max(SD_pct, n = n) |> pull(id)
  
  tmp |> filter(id %in% ids)
}