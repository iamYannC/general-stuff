source('mdp_gridworld.R')
# =============================================================================
# TEST 1 — 3x3 grid, no rewards, no trap
# =============================================================================
# Setup:
#   - start (3,3), terminal (1,1)
#   - step_cost = -1, wall_reward = -2, gamma = 0.9
#   - no rewards, no traps
env1 <- make_env(
  rows = 3, cols = 3,
  start    = c(3, 3),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -2,
  gamma       = 0.9
)

res1 <- solve_mdp(env1)

cat("\nIterations to converge:", res1$n_iter, "— expect < 10\n")

print_grid(res1, "V")
# Expected:
#      [,1]  [,2]  [,3]
# [1,]  0.00 -1.00 -1.90
# [2,] -1.00 -1.90 -2.71
# [3,] -1.90 -2.71 -3.44

print_grid(res1, "policy")

# Spot checks
sid_11 <- lookup_state(res1$states, 1, 1, setNames(integer(0), character(0)))
sid_12 <- lookup_state(res1$states, 1, 2, setNames(integer(0), character(0)))
sid_33 <- lookup_state(res1$states, 3, 3, setNames(integer(0), character(0)))

cat(sprintf("\nV(1,1) = %.4f  — expect 0.0000\n",   res1$V[as.character(sid_11)]))
cat(sprintf("V(1,2) = %.4f  — expect -1.0000\n",   res1$V[as.character(sid_12)]))
cat(sprintf("V(3,3) = %.4f  — expect -3.4390\n",   res1$V[as.character(sid_33)]))

traj1 <- rollout(res1)
cat(sprintf("\nRollout steps: %d  — expect 4 (Manhattan distance (3,3)->(1,1))\n", nrow(traj1)))
cat(sprintf("Cumulative discounted reward: %.4f  — expect -3.4390\n", tail(traj1$disc_cum_reward, 1)))
print(traj1)


# =============================================================================
# TEST 2a — 3x3 grid, one reward worth visiting
# =============================================================================
# Setup:
#   Same grid. Add reward at (3,1) value=10, k=1.
#   Shortest path start(3,3) -> terminal(1,1) = 4 steps, value = -3.439
#   Detour via (3,1): (3,3)->(3,2)->(3,1)[+10]->(2,1)->(1,1) = 4 steps too
#   Detour reward = -1 + 0.9*(-1) + 0.9^2*(10-1) + 0.9^3*(-1) + 0.9^4*0
#                 = -1 - 0.9 + 0.9^2*9 - 0.9^3
#                 = -1 - 0.9 + 7.29 - 0.729 = 4.661
#   Direct path value = -3.439
#   Detour wins clearly — agent should go left first to collect (3,1).
#
# Expected policy at (3,3): left (toward reward)

env2a <- make_env(
  rows = 3, cols = 3,
  start    = c(3, 3),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -2,
  gamma       = 0.9
) |>
  add_reward(pos = c(3, 1), value = 10, k = 1)

res2a <- solve_mdp(env2a)

cat("\nIterations to converge:", res2a$n_iter, "\n")
print_grid(res2a, "V",      counters = c(k1=0L))
print_grid(res2a, "policy", counters = c(k1=0L))
print_grid(res2a, "policy", counters = c(k1=1))
# Expected policy: (3,3) -> left, (3,2) -> left, (3,1) collects then up
# With k1=0, (2,1) says down. with k1=1, it should say up
sid_33_2a <- lookup_state(res2a$states, 3, 3, c(k1=0L))
cat(sprintf("\nV(3,3) k1=0 = %.4f  — expect ~5.561\n", res2a$V[as.character(sid_33_2a)]))
cat(sprintf("policy(3,3) k1=0 = %s  — expect left\n",  res2a$policy[as.character(sid_33_2a)]))


traj2a <- rollout(res2a)
cat(sprintf("\nRollout steps: %d\n", nrow(traj2a)))
cat(sprintf("Cumulative reward: %.4f\n", tail(traj2a$cum_reward, 1)))
print(traj2a)


# =============================================================================
# TEST 2b — same grid, reward too small to be worth visiting
# =============================================================================
#   To make detour not worth it, place reward off the optimal path — use (1,3) instead.
#   Direct path (3,3)->(2,3)->(1,3)->(1,2)->(1,1) passes through (1,3) anyway!
#   Use reward at (3,1) but make path to it longer than direct to terminal.
#   Switch start to (1,3), terminal stays (1,1).
#   Direct: (1,3)->(1,2)->(1,1) = 2 steps, value = -1 - 0.9 = -1.9
#   Detour via (3,1): (1,3)->(2,3)->(3,3)->(3,2)->(3,1)[+1.5]->(2,1)->(1,1) = 6 steps
#   = -1 -0.9 -0.81 -0.729 + 0.9^4*(1.5-1) -0.9^5 -0.9^6
#   = -1 -0.9 -0.81 -0.729 + 0.3281 - 0.5905 - 0.5314 = -4.243
#   Direct wins at -1.9. Agent should go straight left to terminal.

env2b <- make_env(
  rows = 3, cols = 3,
  start    = c(1, 3),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -2,
  gamma       = 0.9
) |>
  add_reward(pos = c(3, 1), value = 1.5, k = 1)

res2b <- solve_mdp(env2b)

cat("\nIterations to converge:", res2b$n_iter, "\n")
print_grid(res2b, "V",      counters = c(k1=0L))
print_grid(res2b, "policy", counters = c(k1=0L))

sid_13_2b <- lookup_state(res2b$states, 1, 3, c(k1=0L))
cat(sprintf("\nV(1,3) k1=0 = %.4f  — expect ~ -1.9\n",  res2b$V[as.character(sid_13_2b)]))
cat(sprintf("policy(1,3) k1=0 = %s  — expect left\n",   res2b$policy[as.character(sid_13_2b)]))

traj2b <- rollout(res2b)
cat(sprintf("\nRollout steps: %d  — expect 2\n", nrow(traj2b)))
print(traj2b)


# =============================================================================
# TEST 3 — 3x3, one reward k=2, worth re-visiting
# =============================================================================
# Setup:
#   start (3,3), terminal (1,1), reward at (2,2) value=5, k=2, gamma=0.9
#   Agent can collect (2,2) up to twice before heading to terminal.
#
#   Path collecting twice:
#   (3,3)->(2,3)->(2,2)[+5, k1=1]->(2,3)->(2,2)[+5, k1=2]->(1,2)->(1,1) ... wait
#   can't re-enter (2,2) immediately from (2,3) without a step in between.
#   Actually: (3,3)->(2,3)->(2,2)[+5,k1=1]->(2,1)->(2,2)? no, (2,1) isn't adjacent to (2,2) via left
#   Let me re-think: (2,2) neighbours are (1,2),(3,2),(2,1),(2,3)
#   Path: (3,3)->(2,3)->(2,2)[+5,k1=1]->(2,3)->(2,2)[+5,k1=2]->(1,2)->(1,1)
#   = 6 steps: rewards at steps 3 and 5
#   value = -1 -0.9 + 0.9^2*4 -0.9^3 + 0.9^4*4 -0.9^5 -0.9^6
#   = -1 -0.9 + 3.24 -0.729 + 2.6244 -0.59049 -0.531441 = 2.114
#
#   vs collecting once then terminal:
#   (3,3)->(2,3)->(2,2)[+5,k1=1]->(1,2)->(1,1) = 4 steps
#   = -1 -0.9 + 0.9^2*4 -0.9^3
#   = -1 -0.9 + 3.24 -0.729 = 0.611
#
#   Collecting twice wins at 2.114. Agent should bounce back to (2,2).
#
# Expected:
#   policy at (2,3) k1=0: left (go collect first time)
#   policy at (2,3) k1=1: left (go collect second time)
#   policy at (2,3) k1=2: up  (done collecting, head to terminal)

env3 <- make_env(
  rows = 3, cols = 3,
  start    = c(3, 3),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -2,
  gamma       = 0.9
) |>
  add_reward(pos = c(2, 2), value = 5, k = 2)

res3 <- solve_mdp(env3)

cat("\nIterations to converge:", res3$n_iter, "\n")

print_grid(res3, "V",      counters = c(k1=0L))
print_grid(res3, "policy", counters = c(k1=0L))
print_grid(res3, "policy", counters = c(k1=1L))
print_grid(res3, "policy", counters = c(k1=2L))

sid_23_k0 <- lookup_state(res3$states, 2, 3, c(k1=0L))
sid_23_k1 <- lookup_state(res3$states, 2, 3, c(k1=1L))
sid_23_k2 <- lookup_state(res3$states, 2, 3, c(k1=2L))

cat(sprintf("\npolicy(2,3) k1=0 = %s  — expect left\n", res3$policy[as.character(sid_23_k0)]))
cat(sprintf("policy(2,3) k1=1 = %s  — expect left\n",  res3$policy[as.character(sid_23_k1)]))
cat(sprintf("policy(2,3) k1=2 = %s  — expect up\n",    res3$policy[as.character(sid_23_k2)]))

rollout(res3)


# =============================================================================
# TEST 4 — collect once but not twice
# =============================================================================
# Setup:
#   3x3 grid, start (3,3), terminal (1,1)
#   reward at (2,2) value=1.8, k=2, gamma=0.9
#
# Key thresholds:
#   Re-collect worth it when v > 1.9 (derived from gamma=0.9, 2-step round trip)
#   value=1.8 < 1.9 => second collection NOT worth it
#   value=1.8 > 0   => first collection IS worth it over direct path
#
# Expected V*(3,3) at k1=0:
#   Path: (3,3)->(2,3)->(2,2)[+1.8]->(1,2)->(1,1) = 4 steps
#   = -1 + 0.9*(1.8-1) + 0.9^2*(-1) + 0.9^3*(-1)
#   = -1 + 0.72 - 0.81 - 0.729 = -1.819
#
# Expected policy:
#   k1=0: agent heads toward (2,2) to collect once
#   k1=1: agent heads straight to terminal — does NOT return to (2,2)
#   k1=2: agent heads straight to terminal (exhausted anyway)

env4 <- make_env(
  rows = 3, cols = 3,
  start    = c(3, 3),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -2,
  gamma       = 0.9
) |>
  add_reward(pos = c(2, 2), value = 1.71, k = 2)

res4 <- solve_mdp(env4)

cat("\nIterations to converge:", res4$n_iter, "\n")

# print_grid(res4, "V",      counters = c(k1=0L))
print_grid(res4, "policy", counters = c(k1=0L))
print_grid(res4, "policy", counters = c(k1=1L))
print_grid(res4, "policy", counters = c(k1=2L))

rollout(res4)

# This function computes the minimum reward value that would make returning to collect again worthwhile at k1=K
breakeven_value(res4, pos = c(1,2), k_current = 1L, collect_pos = c(2,2))

env_case1 <- make_env(
  rows = 3, cols = 3,
  start    = c(1, 3),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -2,
  gamma       = 0.9
) |> add_reward(pos = c(3, 1), value = 4.1, k = 21) 

# Warns for k>1 and more than one reward. only checks k collections on first reward.
minimum_reward(env_case1)

# Case 2: no reward in env, reward_pos supplied explicitly
env_case2 <- make_env(
  rows = 3, cols = 3,
  start    = c(1, 1),
  terminal = c(3, 1),
  step_cost   = -1,
  wall_reward = -2,
  gamma       = 0.9
)

minimum_reward(env_case2, reward_pos = c(2, 1))
# reward is on direct path — expect v_min ~ 0

minimum_reward(env_case2, reward_pos = c(2, 3))
# reward is far away, should be worth while