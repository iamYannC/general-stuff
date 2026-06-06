library(testthat)
source("mdp_gridworld.R")

# 3x3, no rewards - pure navigation
env_base <- make_env(
  rows = 3, cols = 3,
  start    = c(3, 3),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -Inf,
  gamma       = 0.9
)
res_base  <- solve_mdp_value(env_base)
traj_base <- rollout(res_base)

sid_terminal <- lookup_state(res_base$states, 1, 1, setNames(integer(0), character(0)))
sid_start    <- lookup_state(res_base$states, 3, 3, setNames(integer(0), character(0)))
sid_one_step <- lookup_state(res_base$states, 1, 2, setNames(integer(0), character(0)))

# 3x3, one reward worth visiting
env_reward <- make_env(
  rows = 3, cols = 3,
  start    = c(3, 3),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -Inf,
  gamma       = 0.9
) |> add_reward(pos = c(3, 1), value = 10, k = 1)

res_reward  <- solve_mdp_value(env_reward)
traj_reward <- rollout(res_reward)

sid_start_k0 <- lookup_state(res_reward$states, 3, 3, c(k1=0))

# 3x3, reward k=2 worth collecting twice
env_k2 <- make_env(
  rows = 3, cols = 3,
  start    = c(3, 3),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -Inf,
  gamma       = 0.9
) |> add_reward(pos = c(2, 2), value = 5, k = 2)

res_k2  <- solve_mdp_value(env_k2)
traj_k2 <- rollout(res_k2)

sid_k2_start <- lookup_state(res_k2$states, 3, 3, c(k1=0L))

# =============================================================================
test_that("V at terminal is exactly 0", {
  expect_equal(as.numeric(res_base$V[as.character(sid_terminal)]), 0)
})

test_that("V one step from terminal equals step_cost", {
  # from (1,2), one move left reaches terminal: V = -1 + 0.9*0 = -1
  expect_equal(unname(round(res_base$V[sid_one_step], 6)), -1)
})

test_that("V at start matches discounted Manhattan distance", {
  # (3,3) to (1,1): 4 steps, V = 0.9^t * (-1) for t = [0,1,2,3]
  expected <- sum(0.9^(0:3) * -1) 
  expect_equal(unname(round(res_base$V[sid_start], 4)), round(expected, 4))
})

test_that("V at start improves when a valuable reward is reachable", {
  expect_gt(
    res_reward$V[sid_start_k0],
    res_base$V[sid_start]
  )
})


test_that("rollout reaches terminal without warning", {
  expect_no_warning(rollout(res_base))
})

test_that("rollout step count equals Manhattan distance when no rewards", {
  expect_equal(nrow(traj_base), 4)
})

test_that("rollout disc_cum_reward matches V*(start)", {
  expect_equal(
    round(tail(traj_base$disc_cum_reward, 1), 4),
    unname(round(res_base$V[sid_start], 4))
  )
})

test_that("rollout lands on terminal position at final step", {
  expect_equal(tail(traj_base$next_row, 1), res_base$env$terminal[1])
  expect_equal(tail(traj_base$next_col, 1), res_base$env$terminal[2])
})


test_that("reward cell is visited when detour is worthwhile", {
  reward_pos <- res_reward$env$rewards[[1]]$pos
  expect_true(any(traj_reward$row == reward_pos[1] & traj_reward$col == reward_pos[2]))
})

test_that("reward collected exactly once when k=1", {
  expect_equal(sum(traj_reward$reward > 0), 1)
})

test_that("reward collected exactly twice when k=2 and value justifies it", {
  expect_equal(sum(traj_k2$reward > 0), 2)
})

test_that("disc_cum_reward matches V*(start) with k=2 reward", {
  expect_equal(
    round(tail(traj_k2$disc_cum_reward, 1), 4),
    unname(round(res_k2$V[sid_k2_start], 4))
  )
})


test_that("value iteration and policy iteration produce identical V", {
  res_pi <- solve_mdp_policy(env_base)
  expect_equal(max(abs(res_base$V - res_pi$V)), 0)
})

test_that("minimum_reward errors with no reward and no reward_pos", {
  env_no_rw <- make_env(rows=3, cols=3, start=c(1,1), terminal=c(3,3),
                        step_cost=-1, wall_reward=-Inf, gamma=0.9)
  expect_error(minimum_reward(env_no_rw), regexp = "No rewards defined")
})

test_that("minimum_reward warns when k > 1", {
  env_k <- make_env(rows=3, cols=3, start=c(1,3), terminal=c(1,1),
                    step_cost=-1, wall_reward=-Inf, gamma=0.9) |>
    add_reward(pos=c(3,1), value=5, k=3)
  expect_warning(minimum_reward(env_k), regexp = "k=3")
})