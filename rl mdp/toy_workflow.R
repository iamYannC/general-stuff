source("mdp_gridworld.R")

# 8x8 grid, two rewards, no trap
env <- make_env(
  rows = 8, cols = 8,
  start    = c(8, 1),
  terminal = c(1, 8),
  step_cost   = -1,
  wall_reward = -Inf,
  gamma       = 0.95
) |>
  add_reward(pos = c(1, 1), value = 6, k = 1) |>
  add_reward(pos = c(8, 8), value = 4, k = 1)

plot_rewards(env)

# solve with both methods
res_vi <- solve_mdp_value(env)
res_pi <- solve_mdp_policy(env)

cat("Value iterations:", res_vi$n_iter, "\nPolicy (π) iterations:", res_pi$n_iter)


# benchmark
bm <- bench::mark(
  value_iteration  = solve_mdp_value(env),
  policy_iteration = solve_mdp_policy(env),
  iterations = 5, # 5 times for each method, it'll take a few mins
  check = FALSE
)
bm[, c("expression", "min", "median", "mem_alloc")]

# policy evolution - value iteration
plot_policy(res_vi, max_facets = 2, counters = c(k1=0, k2=0))

# optimal trajectory
traj <- rollout(res_vi)
cat("\nSteps to terminal:", nrow(traj), "\n")
cat(
"Cumulative reward:", tail(traj$cum_reward, 1),
"\nDiscounted return:",round(tail(traj$disc_cum_reward, 1), 4)
)
tibble(traj)

# breakeven: is it worth re-collecting reward at (1,1) if agent is one step away?
# reward 1 is at (1,1), k=1 so k_current=0 is the only question
breakeven_value(res_vi, pos = c(1, 2), k_current = 0, collect_pos = c(1, 1))

res_vi$env$rewards[[1]]
# The environemnt is stored in the results. the reward's value is 6, larger than breakeven!


# Collecting reward in this environemnt is not even a detour.
minimum_reward(env)
