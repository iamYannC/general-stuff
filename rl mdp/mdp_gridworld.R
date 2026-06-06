# =============================================================================
# TABLE OF CONTENTS
# =============================================================================
#
# 1. MAIN CONSTRUCTORS
#    - make_env ............................. Initialize grid world parameters
#    - add_reward ........................... Append collectible reward cells
#    - add_trap ............................. Append teleporting hazard cells
#    - build_state_space .................... Generate canonical flat state space
#    - lookup_state ......................... Map position coordinates to state IDs
#    - transition ........................... Execute (state, action) dynamics
#    - solve_mdp_value ............................ Converge value function via value iteration
#    - solve_mdp_policy ..................... Converge value function via policy iteration
#
# 2. ANALYTICS & UTILITIES
#    - print_grid ........................... Slice and view value/policy matrix
#    - rollout .............................. Execute greedy path simulations
#    - run_experiments ...................... Batch solve multiple grid configs
#    - plot_policy .......................... Facet plot policy evolution over iters
#    - plot_rewards ......................... Plot static reward matrix layout
#    - breakeven_value ...................... Check post-collection limits
#    - minimum_reward ....................... Compute detour viability bounds
#
# =============================================================================

# =============================================================================
# 1. MAIN CONSTRUCTORS
# =============================================================================

#' Define a grid world environment
#'
#' @param rows         Number of rows
#' @param cols         Number of columns
#' @param start        c(row, col) — agent start position, 1-indexed
#' @param terminal     c(row, col) — absorbing goal state, V = 0
#' @param step_cost    Reward applied on every valid transition
#' @param wall_reward  Reward for an out-of-bounds attempt. Agent stays in place.
#'                     Must be < step_cost to avoid wall-hitting being optimal.
#' @param gamma        Discount factor in (0, 1]
#'
#' @return A named list representing the environment
make_env <- function(rows, cols,
                     start,
                     terminal,
                     step_cost   = -1,
                     wall_reward = -Inf,
                     gamma       = 1.0) {
  stopifnot(
    is.numeric(rows), rows >= 1,
    is.numeric(cols), cols >= 1,
    length(start)    == 2,
    length(terminal) == 2,
    gamma > 0, gamma <= 1
  )
  list(
    rows        = as.integer(rows),
    cols        = as.integer(cols),
    start       = as.integer(start),
    terminal    = as.integer(terminal),
    step_cost   = step_cost,
    wall_reward = wall_reward,
    gamma       = gamma,
    rewards     = list(),
    traps       = list()
  )
}


#' Add a collectable reward cell
#'
#' @param env    Output of make_env()
#' @param pos    c(row, col)
#' @param value  Reward value on collection
#' @param k      Max times collectible. Must be a finite positive integer.
#'
#' @return Updated env
add_reward <- function(env, pos, value, k = 1L) {
  stopifnot(
    length(pos) == 2,
    is.finite(k), k >= 1
  )
  env$rewards <- append(
    env$rewards,
    list(list(pos = as.integer(pos), value = value, k = as.integer(k)))
  )
  env
}


#' Add a trap cell
#'
#' Stepping on a trap teleports the agent to dest.
#' step_cost still applies. trap$reward is added on top.
#'
#' @param env             Output of make_env() or add_reward()
#' @param pos             c(row, col) — trap location
#' @param dest            c(row, col) — teleport destination. Must differ from pos.
#' @param reward          Extra reward on top of step_cost when trap triggers
#'
#' @return Updated env
add_trap <- function(env, pos, dest, reward = 0) {
  stopifnot(
    length(pos)  == 2,
    length(dest) == 2,
    !identical(as.integer(pos), as.integer(dest))
  )
  env$traps <- append(
    env$traps,
    list(list(
      pos             = as.integer(pos),
      dest            = as.integer(dest),
      reward          = reward
    ))
  )
  env
}
#' Build the full state space as a flat tibble
#'
#' State = (row, col, k1, k2, ...) where ki in 0..Ki for each reward.
#' Rows are ordered by (row, col, k1, k2, ...) and state_id is a simple
#' integer sequence — 1-based, no gaps.
#'
#' @param env  Output of make_env() + add_reward() calls
#'
#' @return A tibble with columns: row, col, [k1, k2, ...], state_id
build_state_space <- function(env) {

  # position grid — all (row, col) combinations
  pos_grid <- expand.grid(
    row = seq_len(env$rows),
    col = seq_len(env$cols),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  if (length(env$rewards) == 0) {
    states <- pos_grid
  } else {
    # one counter column per reward: ki in 0..Ki
    counter_grids <- lapply(seq_along(env$rewards), function(i) {
      ki <- env$rewards[[i]]$k
      setNames(
        data.frame(seq.int(0L, ki), stringsAsFactors = FALSE),
        paste0("k", i)
      )
    })
    # cartesian product of all counters
    counter_space <- Reduce(function(a, b) merge(a, b, by = NULL), counter_grids)
    # cross with position grid
    states <- merge(pos_grid, counter_space, by = NULL)
  }

  # canonical ordering: row, col, then counters left to right
  k_cols  <- grep("^k[0-9]+$", names(states), value = TRUE)
  ord_cols <- c("row", "col", k_cols)
  states   <- states[do.call(order, states[ord_cols]), ]

  # assign state_id as a clean 1-based integer sequence
  states$state_id <- seq_len(nrow(states))
  rownames(states) <- NULL

  states
}
#' Look up state_id given position and counter values
#'
#' @param states   Tibble from build_state_space()
#' @param row      integer
#' @param col      integer
#' @param counters named integer vector e.g. c(k1=0L, k2=1L). NULL if no rewards.
#'
#' @return single integer state_id
lookup_state <- function(states, row, col, counters) {
  mask <- states$row == row & states$col == col
  for (nm in names(counters)) {
    mask <- mask & states[[nm]] == counters[[nm]]
  }
  sid <- states$state_id[mask]
  stopifnot(length(sid) == 1L)
  sid
}


#' Compute one transition: (state, action) -> (next_state_id, reward)
#'
#' Precedence inside the function:
#'   1. wall check  — if out of bounds, stay and return wall_reward
#'   2. trap check  — teleport before reward collection
#'   3. reward check — collect if counter < k
#'
#' @param env      Environment list
#' @param states   Tibble from build_state_space()
#' @param s_id     integer state_id of current state
#' @param action   one of "up", "down", "left", "right"
#'
#' @return list(next_id = integer, reward = numeric)
transition <- function(env, states, s_id, action) {

  s      <- states[states$state_id == s_id, ]
  r      <- s$row
  c      <- s$col
  k_cols <- grep("^k[0-9]+$", names(states), value = TRUE)

  # current counter values as named integer vector
  counters <- if (length(k_cols) == 0) {
    setNames(integer(0), character(0))
  } else {
    setNames(as.integer(s[k_cols]), k_cols)
  }

  # intended next position
  delta <- switch(action,
    up    = c(-1L,  0L),
    down  = c( 1L,  0L),
    left  = c( 0L, -1L),
    right = c( 0L,  1L)
  )
  nr <- r + delta[1]
  nc <- c + delta[2]

  # 1. wall check
  if (nr < 1L || nr > env$rows || nc < 1L || nc > env$cols) {
    return(list(
      next_id = s_id,
      reward  = env$wall_reward
    ))
  }

  reward       <- env$step_cost
  new_counters <- counters

  # 2. trap check
for (trap in env$traps) {
  if (trap$pos[1] == nr && trap$pos[2] == nc) {
    reward <- reward + trap$reward
    nr     <- trap$dest[1]
    nc     <- trap$dest[2]
    break
  }
}

  # 3. reward collection — applied to landing cell (after potential teleport)
  for (i in seq_along(env$rewards)) {
    rw    <- env$rewards[[i]]
    k_col <- paste0("k", i)
    if (rw$pos[1] == nr && rw$pos[2] == nc) {
      if (new_counters[[k_col]] < rw$k) {
        reward                  <- reward + rw$value
        new_counters[[k_col]]   <- new_counters[[k_col]] + 1L
      }
    }
  }

  next_id <- lookup_state(states, nr, nc, new_counters)
  list(next_id = next_id, reward = reward)
}
#' Solve MDP via value iteration
#'
#' @param env      Fully configured environment
#' @param theta    Convergence threshold on max|V_new - V_old|
#' @param max_iter Hard iteration cap
#'
#' @seealso [solve_mdp_policy()]
#' @return named list:
#'   $states      — state space tibble from build_state_space()
#'   $V           — named numeric vector, keys are state_id (as character)
#'   $policy      — named character vector, keys are state_id (as character)
#'   $history     — list of length n_iter, each element is the states tibble
#'                  with V, policy, iter, delta columns appended
#'   $history_tbl — all history snapshots bound into one long data.frame
#'   $n_iter      — number of iterations until convergence
#'   $env         — original env passed through
solve_mdp_value <- function(env, theta = 1e-6, max_iter = 1000) {

  states   <- build_state_space(env)
  n_states <- nrow(states)
  actions  <- c("up", "down", "left", "right")
  s_ids    <- states$state_id

  # terminal state ids — all counter combinations at terminal position
  terminal_ids <- states$state_id[
    states$row == env$terminal[1] & states$col == env$terminal[2]
  ]

  # initialise V = 0 everywhere, policy = NA
  V      <- setNames(rep(0, n_states),          as.character(s_ids))
  policy <- setNames(rep(NA_character_, n_states), as.character(s_ids))

  history <- vector("list", max_iter)

  for (iter in seq_len(max_iter)) {
    V_old <- V
    delta <- 0

    for (sid in s_ids) {

      if (sid %in% terminal_ids) next

      best_val <- -Inf
      best_act <- NA_character_

      for (a in actions) {
        tr  <- transition(env, states, sid, a)
        val <- tr$reward + env$gamma * V_old[as.character(tr$next_id)]
        if (val > best_val) {
          best_val <- val
          best_act <- a
        }
      }

      V[as.character(sid)]      <- best_val
      policy[as.character(sid)] <- best_act
      delta <- max(delta, abs(best_val - V_old[as.character(sid)]))
    }

    # snapshot — append V, policy, iter, delta to state tibble
    snap        <- states
    snap$V      <- V[as.character(s_ids)]
    snap$policy <- policy[as.character(s_ids)]
    snap$iter   <- iter
    snap$delta  <- delta
    history[[iter]] <- snap

    if (delta < theta) {
      history <- history[seq_len(iter)]
      message(sprintf("Converged in %d iterations (delta = %.2e)", iter, delta))
      break
    }

    if (iter == max_iter) {
      warning(sprintf("Did not converge after %d iterations (delta = %.2e)", max_iter, delta))
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
#' Solve MDP via policy iteration
#'
#' Alternates between policy evaluation (iterating V under the current policy
#' until convergence) and policy improvement (greedy one-step update) until
#' the policy stabilises across a full sweep.
#'
#' Initialises with a distance-heuristic policy: prefer actions that minimise
#' Manhattan distance to terminal, breaking ties by immediate reward.
#'
#' @param env          Fully configured environment
#' @param theta        Convergence threshold for policy improvement stability
#' @param max_iter     Hard cap on number of improvement steps
#' @param eval_theta   Convergence threshold for inner policy evaluation loop.
#'                     Defaults to theta.
#' @param max_eval_iter Hard cap on inner evaluation iterations per improvement step
#' @param verbose      If TRUE, prints convergence message
#'
#' @seealso [solve_mdp_value()]
#' @return Named list — identical schema to solve_mdp_value():
#'   $states        — state space tibble from build_state_space()
#'   $V             — named numeric vector, keys are state_id (as character)
#'   $policy        — named character vector, keys are state_id (as character)
#'   $history       — list of per-improvement-step snapshots, each is the states
#'                    tibble with V, policy, iter, delta, eval_iter,
#'                    policy_changes columns appended
#'   $history_tbl   — all snapshots bound into one long data.frame
#'   $n_iter        — number of improvement steps until convergence
#'   $env           — original env passed through
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
  abs(a - b) <= theta # using theta as a tolerance for numerical stability
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

# =============================================================================
# 2. ANALYTICS & UTILITIES
# =============================================================================

resolve_counters <- function(states, counters = NULL) {
  k_cols <- grep("^k[0-9]+$", names(states), value = TRUE)

  if (length(k_cols) == 0) {
    return(setNames(integer(0), character(0)))
  }

  full_counters <- setNames(rep(0L, length(k_cols)), k_cols)

  if (is.null(counters)) {
    return(full_counters)
  }

  if (is.null(names(counters)) || any(names(counters) == "")) {
    if (length(counters) != length(k_cols)) {
      stop(sprintf(
        "Unnamed counters must have length %d, one value for each of: %s.",
        length(k_cols),
        paste(k_cols, collapse = ", ")
      ))
    }
    names(counters) <- k_cols
  }

  unknown <- setdiff(names(counters), k_cols)
  if (length(unknown) > 0) {
    stop(sprintf(
      "Unknown counter(s): %s. Valid counters are: %s.",
      paste(unknown, collapse = ", "),
      paste(k_cols, collapse = ", ")
    ))
  }

  full_counters[names(counters)] <- as.integer(counters)
  full_counters
}

#' Print V or policy as a grid matrix for a specific counter slice
#'
#' Indexing is explicit: each cell (r,c) is looked up directly from the state
#' tibble. No matrix fill-order assumptions.
#'
#' @param result    Output of solve_mdp()
#' @param what      "V" or "policy"
#' @param counters  Named integer vector specifying counter values to slice,
#'                  e.g. c(k1=1L, k2=0L). Defaults to all zeros.
#'
#' @return Invisibly returns the matrix (for programmatic use)
print_grid <- function(result, what = "V", counters = NULL) {

  what <- match.arg(what, c("V", "policy"))

  states  <- result$states
  k_cols  <- grep("^k[0-9]+$", names(states), value = TRUE)
  counters <- resolve_counters(states, counters)

  # filter to the requested counter slice
  mask <- rep(TRUE, nrow(states))
  for (nm in names(counters)) {
    mask <- mask & states[[nm]] == as.integer(counters[[nm]])
  }
  slice <- states[mask, ]

  if (nrow(slice) != result$env$rows * result$env$cols) {
    stop(sprintf(
      "Counter slice returned %d rows, expected %d. Check counter values.",
      nrow(slice), result$env$rows * result$env$cols
    ))
  }

  # build matrix by explicit (row, col) lookup — no fill-order assumption
  rows <- result$env$rows
  cols <- result$env$cols
  mat  <- matrix(NA_character_, nrow = rows, ncol = cols)

  for (i in seq_len(nrow(slice))) {
    r   <- slice$row[i]
    c   <- slice$col[i]
    sid <- slice$state_id[i]

    val <- if (what == "V") {
      round(result$V[as.character(sid)], 2)
    } else {
      p <- result$policy[as.character(sid)]
      if (is.na(p)) "★" else p
    }
    mat[r, c] <- as.character(val)
  }

  counter_str <- if (length(counters) == 0) {
    "none"
  } else {
    paste(names(counters), counters, sep = "=", collapse = ", ")
  }

  cat(sprintf("\n=== %s (counters: %s) ===\n", what, counter_str))
  print(mat, quote = FALSE)
  invisible(mat)
}

#' Simulate the greedy policy from start to terminal
#'
#' Follows result$policy step by step, recording every transition.
#' Stops when terminal is reached or max_steps is exceeded.
#'
#' @param result    Output of solve_mdp()
#' @param max_steps Safety cap — prevents infinite loops if policy is cyclic
#'
#' @return A data.frame with one row per step:
#'   step          — step index
#'   row, col      — position before action
#'   [k1, k2, ...] — counter values before action
#'   state_id      — state id before action
#'   action        — action taken
#'   reward        — reward received
#'   cum_reward    — cumulative reward up to and including this step
#'   next_row      — position after action
#'   next_col      — position after action
rollout <- function(result, max_steps = 1000) {

  env     <- result$env
  states  <- result$states
  k_cols  <- grep("^k[0-9]+$", names(states), value = TRUE)

  # start state — all counters at zero
  start_counters <- setNames(rep(0L, length(k_cols)), k_cols)
  sid <- lookup_state(states, env$start[1], env$start[2], start_counters)

  terminal_ids <- states$state_id[
    states$row == env$terminal[1] & states$col == env$terminal[2]
  ]

  rows <- vector("list", max_steps)
  cum  <- 0
  disc_cum <- 0
  for (step in seq_len(max_steps)) {

    if (sid %in% terminal_ids) break

    s      <- states[states$state_id == sid, ]
    action <- result$policy[as.character(sid)]

    if (is.na(action)) stop(sprintf("No policy defined at state_id %d", sid))

    tr     <- transition(env, states, sid, action)
    cum    <- cum + tr$reward
    disc_cum <- disc_cum + (env$gamma ^ (step - 1)) * tr$reward
    ns     <- states[states$state_id == tr$next_id, ]

    row_entry                  <- as.list(s[, c("row", "col", k_cols, "state_id")])
    row_entry$action           <- action
    row_entry$reward           <- tr$reward
    row_entry$cum_reward       <- cum
    row_entry$disc_cum_reward  <- disc_cum
    row_entry$next_row         <- ns$row
    row_entry$next_col         <- ns$col

    rows[[step]] <- row_entry
    sid          <- tr$next_id
  }

  if (step == max_steps && !(sid %in% terminal_ids)) {
    warning(sprintf("Rollout did not reach terminal after %d steps.", max_steps))
  }

  do.call(rbind, lapply(rows[!sapply(rows, is.null)], as.data.frame))
}

#' Run multiple MDP experiments and return unified results
#'
#' @param experiments Named list of environments e.g. list(baseline = env1, trap = env2)
#' @param theta       Convergence threshold passed to solve_mdp()
#' @param max_iter    Max iterations passed to solve_mdp()
#'
#' @return list with:
#'   $results       — named list of solve_mdp() outputs, one per experiment
#'   $summary       — tibble with one row per experiment: name, n_iter, n_states,
#'                    start_value, final_delta
#'   $history_tbl   — unified long tibble with experiment column prepended
run_experiments <- function(experiments, theta = 1e-6, max_iter = 1000) {
  stopifnot(is.list(experiments), !is.null(names(experiments)))
 
  results <- lapply(names(experiments), function(nm) {
    message(sprintf("\n--- Running: %s ---", nm))
    solve_mdp(experiments[[nm]], theta = theta, max_iter = max_iter)
  })
  names(results) <- names(experiments)

summary_tbl <- do.call(rbind, lapply(names(results), function(nm) {
  res <- results[[nm]]
  
  # Identify if this specific experiment's state space uses counter columns
  k_cols <- grep("^k[0-9]+$", names(res$states), value = TRUE)
  
  # If counter columns exist, start state requires them initialized to 0L
  start_counters <- if (length(k_cols) > 0) {
    setNames(rep(0L, length(k_cols)), k_cols)
  } else {
    setNames(integer(0), character(0))
  }
  
  start_sid <- lookup_state(
    res$states,
    res$env$start[1],
    res$env$start[2],
    start_counters
  )
  
  tibble::tibble(
    experiment   = nm,
    n_states     = nrow(res$states),
    n_iter       = res$n_iter,
    start_value  = round(res$V[as.character(start_sid)], 4),
    final_delta  = round(
      res$history_tbl |> dplyr::filter(iter == res$n_iter) |> dplyr::pull(delta) |> dplyr::first(),
      8
    )
  )
}))
 
 history_tbl <- dplyr::bind_rows(lapply(names(results), function(nm) {
  results[[nm]]$history_tbl |> dplyr::mutate(experiment = nm, .before = 1)
}))
 
  list(
    results     = results,
    summary     = summary_tbl,
    history_tbl = history_tbl
  )
}

#' Plot policy evolution for specific iterations
#'
#' @param result Output list from solve_mdp()
#' @param max_facets Maximum number of intermediate iteration plots to display (including final)
#' @param counters Named integer vector specifying counter values to slice,
#'                 e.g. c(k1=1L, k2=0L). Defaults to all zeros.
#'
#' @return A ggplot object showing grid tiles, policy directions, and V value fills
plot_policy <- function(result, max_facets = 5, counters = NULL) {
  env       <- result$env
  env_start <- env$start
  env_term  <- env$terminal
  
reward_info <- "No Rewards"
  if (length(env$rewards) > 0) {
    # Grabs the first reward details as a baseline description
    rw <- env$rewards[[1]]
    reward_info <- paste0("Reward at ", rw$pos[1], ",", rw$pos[2], " (val: ", rw$value, ")")
  }

  # Filter data to the requested counter slice
  plot_data <- result$history_tbl
  k_cols    <- grep("^k[0-9]+$", names(plot_data), value = TRUE)
  counters  <- resolve_counters(plot_data, counters)
  if (length(k_cols) > 0) {
    for (nm in names(counters)) {
      plot_data <- plot_data |>
        dplyr::filter(.data[[nm]] == as.integer(counters[[nm]]))
    }
  }

  # Select which iteration snapshots to show
  unique_iters <- unique(plot_data$iter)
  total_iters  <- length(unique_iters)
  
  if (total_iters > max_facets) {
    # Generate evenly spaced indexes, always guaranteeing the last iteration is included
    idx <- unique(c(round(seq(1, total_iters - 1, length.out = max_facets - 1)), total_iters))
    keep_iters <- unique_iters[idx]
    plot_data <- plot_data |> dplyr::filter(iter %in% keep_iters)
  }
  
  # Construct title combining layout vectors and reward metadata
  title_str <- paste0(
    "Policy Evolution (Start: ", env_start[1], ",", env_start[2], 
    " | Terminal: ", env_term[1], ",", env_term[2], ")\n",
    "Config: ", reward_info, " | Step Cost: ", env$step_cost,
    " | Counters: ", if (length(counters) == 0) {
      "none"
    } else {
      paste(names(counters), counters, sep = "=", collapse = ", ")
    }
  )
  
  plot_data |>
    dplyr::mutate(policy = dplyr::coalesce(policy, "★")) |>
    ggplot2::ggplot(ggplot2::aes(x = col, y = -row, label = policy)) +
    ggplot2::geom_tile(ggplot2::aes(fill = V), colour = "white", show.legend = F) +
    ggplot2::geom_text(colour = "white", size = 4) +
    ggplot2::geom_text(
      data = ~dplyr::filter(.x, row == env_start[1] & col == env_start[2]), 
      label = "Start", color = "tomato", vjust = -1.5, fontface = "bold", size = 5
    ) +
    ggplot2::geom_text(
      data = ~dplyr::filter(.x, row == env_term[1] & col == env_term[2]), 
      label = "Terminal", color = "springgreen", vjust = -1.5, fontface = "bold", size = 5
    ) +
    ggplot2::scale_fill_gradient2(low = "firebrick", mid = "gray20", high = "dodgerblue4", midpoint = 0) +
    ggplot2::facet_wrap(~iter, labeller = ggplot2::label_both) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::labs(title = title_str, fill = "V Value")
}

#' Plot base environment configuration rewards
#'
#' @param env Fully configured environment list object
#'
#' @return A ggplot object showing static step costs, bounds, and asset values
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
  
  # Map custom rewards over coordinate locations (ignoring limits/k counters)
  if (length(env$rewards) > 0) {
    for (rw in env$rewards) {
      mask <- grid_df$row == rw$pos[1] & grid_df$col == rw$pos[2]
      grid_df$reward_val[mask] <- grid_df$reward_val[mask] + rw$value
    }
  }
  
  # Standardize labels for map reading
  grid_df <- grid_df |> 
    dplyr::mutate(
      label_text = as.character(round(reward_val, 1)),
      label_text = dplyr::case_when(
        row == env$terminal[1] & col == env$terminal[2] ~ paste0(label_text, " (Terminal)"),
        row == env$start[1]    & col == env$start[2]    ~ paste0(label_text, " (Start)"),
        TRUE ~ label_text
      )
    )
  
  ggplot2::ggplot(grid_df, ggplot2::aes(x = col, y = -row)) +
    ggplot2::geom_tile(ggplot2::aes(fill = reward_val), colour = "white", show.legend = F) +
    ggplot2::geom_text(ggplot2::aes(label = label_text), colour = "white", fontface = "bold", size = 4) +
    ggplot2::scale_fill_distiller(palette = "Spectral") +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::labs(
      title = "Reward Matrix",
      fill  = "Reward"
    )
}

breakeven_value <- function(result, pos, k_current, collect_pos,
                            return_path_steps = 2) {
  # V at collect_pos when counter is at k_current + 1 (post collection, exhausted)
  env     <- result$env
  states  <- result$states
  k_cols  <- grep("^k[0-9]+$", names(states), value = TRUE)

  # build counters for post-collection state
  counters_after        <- setNames(rep(0L, length(k_cols)), k_cols)
  counters_after["k1"]  <- as.integer(k_current + 1L)

  sid_after <- lookup_state(states, collect_pos[1], collect_pos[2], counters_after)
  V_after   <- result$V[as.character(sid_after)]

  # breakeven: v + gamma * V_after > 0  (the -1 cancels on both sides)
  # v > -gamma * V_after
  breakeven <- -env$gamma * V_after
  cat(
  "V(", collect_pos[1], ",", collect_pos[2], ") at k1=", k_current + 1L, 
  " (post-collect): ", round(V_after, 4), "\n",
  "Breakeven value: v > ", round(breakeven, 4), "\n", 
  sep = ""
)
  invisible(breakeven)
}


#' Compute the minimum reward value that makes a detour worthwhile
#'
#' Reads start, terminal, gamma, and step_cost directly from the env config.
#' The reward cell location is taken from env$rewards if present.
#' If no rewards are defined in env, reward_pos must be supplied explicitly.
#' Raises an error if neither source provides a reward location.
#'
#' Assumes Manhattan-distance optimal paths (no obstacles blocking shortest route).
#' For k=1 only (analytical). Use minimum_reward_k() for k > 1.
#'
#' @param env        Fully configured env (from make_env + optional add_reward)
#' @param reward_pos c(row, col) override — only used when env$rewards is empty
#'
#' @return Invisibly returns a named list:
#'   $v_min         — minimum reward to make detour worthwhile
#'   $v_current     — current reward value in env (NA if none defined)
#'   $worthwhile    — logical: is the current reward above v_min?
#'   $V_direct      — value of direct path (no reward)
#'   $d             — steps from start to reward cell
minimum_reward <- function(env, reward_pos = NULL) {

  # --- resolve reward position ---
  if (length(env$rewards) > 0) {
    if (!is.null(reward_pos)) {
      message("reward_pos argument ignored — using reward location from env config.")
    }
    rw         <- env$rewards[[1]]
    reward_pos <- rw$pos
    v_current  <- rw$value
  } else {
    if (is.null(reward_pos)) {
      stop("No rewards defined in env and reward_pos not supplied. Provide at least one.")
    }
    v_current <- NA_real_
  }

  # --- warnings ---
  if (length(env$rewards) > 1) {
    warning(sprintf(
      "env contains %d rewards. Only the first reward at (%d,%d) is used. ",
      length(env$rewards), reward_pos[1], reward_pos[2]
    ))
  }

  if (!is.na(v_current) && env$rewards[[1]]$k > 1) {
    warning(sprintf(
      "Reward at (%d,%d) has k=%d. minimum_reward() assumes k=1 only. Result may be inaccurate.",
      reward_pos[1], reward_pos[2], env$rewards[[1]]$k
    ))
  }

  # --- build baseline env: same config, no rewards, no traps ---
  env_base <- make_env(
    rows        = env$rows,
    cols        = env$cols,
    start       = env$start,
    terminal    = env$terminal,
    step_cost   = env$step_cost,
    wall_reward = env$wall_reward,
    gamma       = env$gamma
  )

  res_base  <- solve_mdp(env_base, theta = 1e-8)
  sid_start <- lookup_state(
    res_base$states,
    env$start[1], env$start[2],
    setNames(integer(0), character(0))
  )
  V_direct <- res_base$V[as.character(sid_start)]

  # --- discounted path costs ---
  gamma <- env$gamma
  d     <- abs(reward_pos[1] - env$start[1])    +
           abs(reward_pos[2] - env$start[2])      # steps to reward
  dp    <- abs(env$terminal[1] - reward_pos[1]) +
           abs(env$terminal[2] - reward_pos[2])   # steps reward -> terminal

  cost_to   <- sum(gamma ^ seq(0, d  - 1)) * env$step_cost
  cost_from <- if (dp > 1)
    sum(gamma ^ seq(d + 1, d + dp - 1)) * env$step_cost
  else
    0

  C_path <- cost_to + cost_from

  # --- breakeven ---
  v_min <- 1 + (V_direct - C_path) / gamma ^ d

  # --- report ---
  worthwhile <- if (!is.na(v_current)) v_current > v_min else NA

 cat(
  "Start:        (", env$start[1], ",", env$start[2], ")\n",
  "Terminal:     (", env$terminal[1], ",", env$terminal[2], ")\n",
  "Reward cell:  (", reward_pos[1], ",", reward_pos[2], ")\n",
  "Gamma:        ", round(env$gamma, 2), "\n",
  "V_direct:     ", round(V_direct, 4), "\n",
  "d (to reward): ", d, " steps\n",
  "Min reward:   v > ", round(v_min, 4), "\n",
  "Current v:    ", if (is.na(v_current)) {
    "not set"
  } else {
    paste0(
      round(v_current, 4), "  =>  ", 
      if (isTRUE(worthwhile)) "worthwhile ✓" else "not worthwhile ✗"
    )
  }, "\n",
  sep = ""
)

  invisible(list(
    v_min      = v_min,
    v_current  = v_current,
    worthwhile = worthwhile,
    V_direct   = V_direct,
    d          = d
  ))
}
