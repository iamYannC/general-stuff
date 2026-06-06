size <- 6
start <- sample(size,2)
term <- sample(size,2)
reward_1 <- sample(size,2)
reward_2 <- sample(size,2)


big_env <- make_env(row = size,
                    cols = size,
                    start = start,
                    terminal = term,
                    wall_reward = -Inf,
                    gamma = 0.9) |> 
  add_reward(pos = reward_1, value = 3, k = 1) |>
  add_reward(pos = reward_2, value = 5, k = 1)

# temp plot rewards: #
plot_rewards <- function(env) {
  # Build base grid data frame matching environmental dimensions
  grid_df <- expand.grid(
    row = seq_len(env$rows),
    col = seq_len(env$cols),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Assign the base transition penalty value everywhere
  grid_df$reward_val <- env$step_cost
  
  # Map custom rewards over coordinate locations (tracking k bounds dynamically)
  reward_metadata <- character(0)
  if (length(env$rewards) > 0) {
    for (i in seq_along(env$rewards)) {
      rw <- env$rewards[[i]]
      mask <- grid_df$row == rw$pos[1] & grid_df$col == rw$pos[2]
      grid_df$reward_val[mask] <- grid_df$reward_val[mask] + rw$value
      
      # Build descriptive metadata string for the subtitle
      reward_metadata <- c(
        reward_metadata, 
        paste0("Reward #", i, " at (", rw$pos[1], ",", rw$pos[2], ") [Value: ", rw$value-1, ", Max Collections (k): ", rw$k, "]")
      )
    }
  }
  
  subtitle_str <- if (length(reward_metadata) > 0) {
    paste(reward_metadata, collapse = "\n")
  } else {
    "No config"
  }
  
  # Standardize labels for map reading
  grid_df <- grid_df |> 
    dplyr::mutate(
      label_text = as.character(round(reward_val, 1)),
      label_text = dplyr::case_when(
        row == env$terminal[1] & col == env$terminal[2] ~ paste0(label_text, "\n(Terminal)"),
        row == env$start[1]    & col == env$start[2]    ~ paste0(label_text, "\n(Start)"),
        TRUE ~ label_text
      )
    )
  
  ggplot2::ggplot(grid_df, ggplot2::aes(x = col, y = row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = reward_val), colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = label_text), colour = "white", fontface = "bold", size = 6) +
    ggplot2::scale_fill_gradient2(
  low      = "#3325ce",  
  mid      = "#96ea75",  
  high     = "#701214", 
  midpoint = 0
) +
    ggplot2::scale_x_continuous(breaks = seq_len(env$cols), position = "top", expand = c(0, 0)) +
    ggplot2::scale_y_reverse(breaks = seq_len(env$rows), expand = c(0, 0)) +
    ggplot2::labs(
      title    = "Reward Matrix",
      subtitle = subtitle_str,
      x        = NULL,
      y        = NULL,
      fill     = "Reward"
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      plot.title     = ggplot2::element_text(face = "bold", size = rel(1), margin = ggplot2::margin(b = 4)),
      plot.subtitle  = ggplot2::element_text(color = "gray30", size = rel(0.8), face = "italic", margin = ggplot2::margin(b = 12)),
      panel.grid     = ggplot2::element_blank(),
      axis.title     = ggplot2::element_text(face = "bold", size = rel(0.8)),
      axis.text      = ggplot2::element_text(face = "bold", color = "black", size = rel(0.85)),
      legend.position = 'none'
    )
}

plot_rewards(big_env)
res <- solve_mdp_policy(big_env)
plot_policy(res,1)
print_grid(res,"policy")
traj <- rollout(res)
traj

sid <- lookup_state(res$states, 5, 4, c(k1=0L, k2=0L))
for (i in 1:15) {
  s      <- res$states[res$states$state_id == sid, ]
  action <- res$policy[as.character(sid)]
  tr     <- transition(big_env, res$states, sid, action)
  ns     <- res$states[res$states$state_id == tr$next_id, ]
  cat(sprintf("step %2d: (%d,%d) k1=%d k2=%d [%s] -> (%d,%d) k1=%d k2=%d  r=%.2f\n",
    i, s$row, s$col, s$k1, s$k2, action,
    ns$row, ns$col, ns$k1, ns$k2, tr$reward))
  sid <- tr$next_id
  
}
