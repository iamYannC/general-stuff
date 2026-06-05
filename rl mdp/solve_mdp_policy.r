# Policy iteration solver for the gridworld in mdp_gridworld.R.
# Source mdp_gridworld.R first, or source this file from the project root.
if (!exists("build_state_space", mode = "function") ||
    !exists("transition", mode = "function")) {
  source("mdp_gridworld.R")
}

solve_mdp_policy <- function(env,
                             theta = 1e-6,
                             max_iter = 100,
                             eval_theta = theta,
                             max_eval_iter = 10000,
                             verbose = TRUE) {

  states   <- build_state_space(env)
  n_states <- nrow(states)
  actions  <- c("up", "down", "left", "right")
  s_ids    <- states$state_id

  terminal_ids <- states$state_id[
    states$row == env$terminal[1] & states$col == env$terminal[2]
  ]
  nonterminal_ids <- setdiff(s_ids, terminal_ids)

  V <- setNames(rep(0, n_states), as.character(s_ids))
  policy <- setNames(rep(NA_character_, n_states), as.character(s_ids))

  state_distance <- function(sid) {
    s <- states[states$state_id == sid, ]
    abs(s$row - env$terminal[1]) + abs(s$col - env$terminal[2])
  }

  state_diff <- function(new, old) {
    diff <- abs(new - old)
    if (is.nan(diff)) 0 else diff
  }

  same_value <- function(a, b) {
    if (is.infinite(a) || is.infinite(b)) {
      return(identical(a, b))
    }
    abs(a - b) <= theta
  }

  # Start with a simple valid-looking policy: prefer moves that get closer to
  # the terminal after any trap teleport has been applied.
  for (sid in nonterminal_ids) {
    candidates <- data.frame(
      action = actions,
      next_id = NA_integer_,
      reward = NA_real_,
      distance = NA_real_,
      stringsAsFactors = FALSE
    )

    for (i in seq_along(actions)) {
      tr <- transition(env, states, sid, actions[i])
      candidates$next_id[i]  <- tr$next_id
      candidates$reward[i]   <- tr$reward
      candidates$distance[i] <- state_distance(tr$next_id)
    }

    finite_reward <- is.finite(candidates$reward)
    if (any(finite_reward)) {
      candidates <- candidates[finite_reward, ]
    }

    ord <- order(candidates$distance, -candidates$reward)
    policy[as.character(sid)] <- candidates$action[ord[1]]
  }

  history <- vector("list", max_iter)

  for (iter in seq_len(max_iter)) {

    # Policy evaluation.
    eval_delta <- Inf
    eval_iter <- 0L
    for (eval_iter in seq_len(max_eval_iter)) {
      V_old <- V
      eval_delta <- 0

      for (sid in nonterminal_ids) {
        action <- policy[as.character(sid)]
        tr     <- transition(env, states, sid, action)
        val    <- tr$reward + env$gamma * V_old[as.character(tr$next_id)]

        V[as.character(sid)] <- val
        eval_delta <- max(eval_delta, state_diff(val, V_old[as.character(sid)]))
      }

      if (eval_delta < eval_theta) {
        break
      }
    }

    if (eval_iter == max_eval_iter && eval_delta >= eval_theta) {
      warning(sprintf(
        "Policy evaluation did not converge after %d iterations (delta = %.2e)",
        max_eval_iter,
        eval_delta
      ))
    }

    # Policy improvement.
    policy_stable <- TRUE
    policy_changes <- 0L

    for (sid in nonterminal_ids) {
      old_action <- policy[as.character(sid)]

      q_values <- setNames(rep(NA_real_, length(actions)), actions)
      for (a in actions) {
        tr <- transition(env, states, sid, a)
        q_values[a] <- tr$reward + env$gamma * V[as.character(tr$next_id)]
      }

      best_value <- max(q_values)
      best_action <- names(q_values)[which.max(q_values)]

      if (!is.na(old_action) && same_value(q_values[old_action], best_value)) {
        best_action <- old_action
      }

      if (!identical(old_action, best_action)) {
        policy[as.character(sid)] <- best_action
        policy_stable <- FALSE
        policy_changes <- policy_changes + 1L
      }
    }

    snap                  <- states
    snap$V                <- V[as.character(s_ids)]
    snap$policy           <- policy[as.character(s_ids)]
    snap$iter             <- iter
    snap$delta            <- eval_delta
    snap$eval_iter        <- eval_iter
    snap$policy_changes   <- policy_changes
    history[[iter]]       <- snap

    if (policy_stable) {
      history <- history[seq_len(iter)]
      if (verbose) {
        message(sprintf(
          "Policy iteration converged in %d improvements (last eval_delta = %.2e)",
          iter,
          eval_delta
        ))
      }
      break
    }

    if (iter == max_iter) {
      warning(sprintf(
        "Policy iteration did not stabilize after %d improvements",
        max_iter
      ))
    }
  }

  history_tbl <- do.call(rbind, history)

  list(
    states      = states,
    V           = V,
    policy      = policy,
    history     = history,
    history_tbl = history_tbl,
    n_iter      = length(history),
    env         = env
  )
}

solve_mdp_policy_iteration <- solve_mdp_policy
