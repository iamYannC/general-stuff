# MDP Grid World
A fun toy project written with Claude. My attempt to adjust Python scripts and frameworks for MDP and RL in general I found on the web into a structured framework in R. Supports value iteration, policy iteration and k-collectable rewards.

origin at **top-left**.

```r
source("mdp_gridworld.R")
```

---

## Table of contents

1. [Constructors](#1-constructors)
2. [Solvers](#2-solvers)
3. [Analytics](#3-analytics)
4. [Utilities](#4-utilities)
5. [Complete workflow](#5-complete-workflow)
6. [Comparing solvers](#6-comparing-solvers)
7. [Design notes](#7-design-notes)

---

## 1. Constructors

### `make_env()`

Always the first call. Defines grid dimensions, agent positions (start & terminal), and global cost parameters.

| Parameter | Default | Description |
|---|---|---|
| `rows`, `cols` | — | Grid dimensions |
| `start` | — | `c(row, col)` agent starting position |
| `terminal` | — | `c(row, col)` absorbing goal. `V = 0` by definition |
| `step_cost` | `-1` | Cost applied on every valid move |
| `wall_reward` | `-Inf` | Reward for attempting an out-of-bounds move. Agent stays in place. |
| `gamma` | `0.95` | Discount factor `(0, 1]`. Use `< 1` with repeatable rewards (`k > 1`) |

```r
env <- make_env(
  rows = 4, cols = 4,
  start    = c(4, 1),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -Inf,
  gamma       = 0.95
)
```

---

### `add_reward()`

Attaches a collectable bonus to a cell. Chain multiple calls for multiple rewards. Returns the updated env.

| Parameter | Default | Description |
|---|---|---|
| `pos` | — | `c(row, col)` cell location |
| `value` | — | Reward on collection. Net transition reward = `step_cost + value` |
| `k` | `1` | Max collections. Each finite `k` adds a counter dimension to the state space: a cell with `k=3` contributes 4 counter states (0–3) |

```r
env <- env |>
  add_reward(pos = c(1, 4), value = 5, k = 1) |>
  add_reward(pos = c(2, 2), value = 2, k = 3)
```

---

### `add_trap()`

Marks a cell as a trap. Stepping on it teleports the agent to `dest`. Collection counters are **preserved** — the agent keeps what it collected. Step cost applies; an optional `reward` stacks on top.

| Parameter | Default | Description |
|---|---|---|
| `pos` | — | `c(row, col)` trap location |
| `dest` | — | `c(row, col)` teleport destination. Must differ from `pos` |
| `reward` | `0` | Extra reward on trap trigger, on top of `step_cost` |

```r
env <- env |>
  add_trap(pos = c(3, 3), dest = c(4, 1), reward = 0)
```

---

## 2. Solvers

Both solvers accept the same `env` and return an **identical output schema** — results are directly comparable.

### `solve_mdp_value()`

Value iteration. Sweeps all states repeatedly, applying the Bellman optimality update until `max|V_new - V_old| < theta`.

| Parameter | Default | Description |
|---|---|---|
| `theta` | `1e-6` | Convergence threshold on max Bellman residual |
| `max_iter` | `1000` | Hard iteration cap |

### `solve_mdp_policy()`

Policy iteration. Alternates between full policy evaluation (inner loop until convergence) and greedy policy improvement. Initialises with a Manhattan-distance heuristic policy.

| Parameter | Default | Description |
|---|---|---|
| `theta` | `1e-6` | Convergence threshold for policy stability |
| `max_iter` | `100` | Hard cap on improvement steps |
| `eval_theta` | `theta` | Inner evaluation convergence threshold |
| `max_eval_iter` | `10000` | Hard cap on inner evaluation iterations |
| `verbose` | `TRUE` | Print convergence message |

### Shared return schema

| Field | Description |
|---|---|
| `$states` | Full state space tibble: `row`, `col`, `k1`, `k2`, ..., `state_id` |
| `$V` | Named numeric vector. Keys = `state_id` (character). Final converged values |
| `$policy` | Named character vector. Keys = `state_id`. Values: `"up"`, `"down"`, `"left"`, `"right"` |
| `$history` | List of per-iteration snapshots. Each is `$states` with `V`, `policy`, `iter`, `delta` appended |
| `$history_tbl` | All snapshots bound into one long `data.frame`. ggplot2-ready |
| `$n_iter` | Iterations to convergence |
| `$env` | Original env (passed through) |

```r
res_vi <- solve_mdp_value(env)
res_pi <- solve_mdp_policy(env)

# V* at start should match across solvers
res_vi$V["1"]
res_pi$V["1"]
```

---

## 3. Analytics

### `print_grid()`

Prints `V` or `policy` as a console matrix for a specific counter slice.

| Parameter | Default | Description |
|---|---|---|
| `what` | `"V"` | `"V"` or `"policy"` |
| `counters` | `NULL` (all zeros) | e.g. `c(k1=1L, k2=0L)` — slice after collecting reward 1 once |

```r
print_grid(res_vi, "V")
print_grid(res_vi, "policy")

# after collecting reward 1 twice:
print_grid(res_vi, "policy", counters = c(k1=2L))
```

```
=== policy (counters: k1=0) ===
     [,1]  [,2]  [,3]  [,4]
[1,] ★     left  left  left
[2,] up    up    up    up
[3,] up    up    up    up
[4,] up    up    right up
```

---

### `rollout()`

Simulates the greedy policy from start to terminal. Returns one row per step.

| Parameter | Default | Description |
|---|---|---|
| `max_steps` | `1000` | Safety cap against cyclic policies |

**Returns a `data.frame`:**

| Column | Description |
|---|---|
| `row`, `col` | Position before action |
| `k1`, `k2`, ... | Counter values before action |
| `state_id` | State ID before action |
| `action` | Action taken |
| `reward` | Reward received (step_cost ± bonus) |
| `cum_reward` | Undiscounted cumulative reward |
| `disc_cum_reward` | Discounted cumulative reward. At last row = `V*(start)` |
| `next_row`, `next_col` | Landing position |

```r
traj <- rollout(res_vi)
print(traj)

# verify: disc_cum_reward at last row == V*(start)
start_sid <- lookup_state(res_vi$states, env$start[1], env$start[2], c(k1=0L))
all.equal(
  tail(traj$disc_cum_reward, 1),
  res_vi$V[as.character(start_sid)]
)
```

---

### `plot_policy()`

Faceted ggplot showing policy evolution across iterations. Tiles are coloured by V value.

| Parameter | Default | Description |
|---|---|---|
| `max_facets` | `5` | Max iteration snapshots shown. Always includes final iteration |
| `counters` | `NULL` | Counter slice to display. Defaults to all zeros |

```r
p <- plot_policy(res_vi, max_facets = 6)
print(p)

# save to disk
dir.create("assets", showWarnings = FALSE)
ggplot2::ggsave("assets/policy_evolution.png", p, width = 14, height = 8, dpi = 150)
```

---

### `plot_rewards()`

Static plot of the reward matrix. Shows step cost everywhere, with reward cells highlighted by their net collection value.

```r
p <- plot_rewards(env)
print(p)

ggplot2::ggsave("assets/reward_matrix.png", p, width = 8, height = 6, dpi = 150)
```

---

### `run_experiments()`

Batch-solves a named list of environments with `solve_mdp_value()` and returns unified results.

```r
exp <- run_experiments(list(
  baseline = env_no_reward,
  with_reward = env
))

exp$summary
#   experiment   n_states n_iter start_value final_delta
#   baseline           16      6      -3.000           0
#   with_reward        64     14       5.561           0

# convergence comparison
exp$history_tbl |>
  dplyr::group_by(experiment, iter) |>
  dplyr::summarise(delta = dplyr::first(delta), .groups = "drop") |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = delta, colour = experiment)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_log10() +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "Convergence", x = "Iteration", y = "Max delta (log)")
```

---

## 4. Utilities

### `breakeven_value()`

Given a solved result and a reward cell, computes the minimum reward value that makes **re-collecting** worthwhile from the current position.

```r
# is it worth collecting (2,2) a second time?
breakeven_value(res_vi, pos = c(1,2), k_current = 1L, collect_pos = c(2,2))

# V(2,2) at k1=2 (post-collect): -1.9000
# Breakeven value: v > 1.7100
```

Use this to set the precise threshold between one and two collections for a given gamma and grid topology.

---

### `minimum_reward()`

Computes the minimum reward value that makes a **detour to the reward cell worthwhile** at all, relative to going directly to terminal.

Reads all parameters from `env`. If `env$rewards` is empty, supply `reward_pos` explicitly.

**Assumptions:** k=1 only; Manhattan-distance paths (no obstacles). Warns on multiple rewards (uses first) or k > 1.

```r
# env with reward already configured
minimum_reward(env)

# Start:         (4,1)
# Terminal:      (1,1)
# Reward cell:   (1,4)
# Gamma:         0.95
# V_direct:      -2.8550
# d (to reward): 6 steps
# Min reward:    v > 4.2100
# Current v:     5.0000  =>  worthwhile ✓

# env without reward — supply position manually
minimum_reward(env_no_reward, reward_pos = c(1, 4))
```

---

## 5. Complete workflow

```r
source("mdp_gridworld.R")

# 1. Build environment
env <- make_env(
  rows = 4, cols = 4,
  start    = c(4, 1),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = -Inf,
  gamma       = 0.95
) |>
  add_reward(pos = c(1, 4), value = 5, k = 3) |>
  add_reward(pos = c(2, 2), value = 1, k = 1) |>
  add_trap(  pos = c(3, 3), dest = c(4, 1))

# 2. Visualise reward layout
plot_rewards(env)

# 3. Solve
res <- solve_mdp_value(env)
# Converged in 14 iterations (delta = 0.00e+00)

# 4. Inspect
print_grid(res, "V",      counters = c(k1=0L, k2=0L))
print_grid(res, "policy", counters = c(k1=0L, k2=0L))
print_grid(res, "policy", counters = c(k1=3L, k2=1L))  # fully collected

# 5. Simulate optimal trajectory
traj <- rollout(res)
print(traj)
cat("Steps to terminal:", nrow(traj), "\n")
cat("Discounted return:", tail(traj$disc_cum_reward, 1), "\n")

# 6. Policy evolution plot
p <- plot_policy(res, max_facets = 6, counters = c(k1=0L, k2=0L))
ggplot2::ggsave("assets/policy_evolution.png", p, width = 14, height = 8, dpi = 150)

# 7. Reward design
minimum_reward(env)
breakeven_value(res, pos = c(1,3), k_current = 1L, collect_pos = c(1,4))
```

---

## 6. Comparing solvers

Both solvers produce identical `V*` and `π*`. The difference is in convergence speed: policy iteration uses fewer outer iterations but each is more expensive (full evaluation inner loop). Value iteration uses many cheap sweeps.

```r
res_vi <- solve_mdp_value(env)
res_pi <- solve_mdp_policy(env)

# iterations
cat("Value iteration: ", res_vi$n_iter, "sweeps\n")
cat("Policy iteration:", res_pi$n_iter, "improvement steps\n")

# V* must match
start_sid <- lookup_state(res_vi$states, env$start[1], env$start[2], c(k1=0L, k2=0L))
cat("V*(start) — VI:", round(res_vi$V[as.character(start_sid)], 6), "\n")
cat("V*(start) — PI:", round(res_pi$V[as.character(start_sid)], 6), "\n")

# policy must match (modulo ties)
all(res_vi$policy == res_pi$policy, na.rm = TRUE)

# convergence curves side by side
dplyr::bind_rows(
  res_vi$history_tbl |> dplyr::mutate(solver = "value"),
  res_pi$history_tbl |> dplyr::mutate(solver = "policy")
) |>
  dplyr::group_by(solver, iter) |>
  dplyr::summarise(delta = dplyr::first(delta), .groups = "drop") |>
  ggplot2::ggplot(ggplot2::aes(x = iter, y = delta, colour = solver)) +
  ggplot2::geom_line() +
  ggplot2::scale_y_log10() +
  ggplot2::theme_minimal() +
  ggplot2::labs(title = "VI vs PI convergence", x = "Iteration", y = "Delta (log)")
```

---

## 7. Design notes

**State encoding.** State = `(row, col, k1, k2, ...)`. Each finite-k reward adds one counter dimension. State count = `rows × cols × (K1+1) × (K2+1) × ...`. A 4×4 grid with two rewards at k=3 and k=1 gives `4×4×4×2 = 128` states — tractable for value iteration.

**Trap semantics.** Counters are always preserved through a trap. The trap only changes position, not collection history. Step cost always applies on trap entry; `trap$reward` stacks on top.

**Wall reward.** Keep `wall_reward < step_cost`. With `gamma < 1`, even a slightly positive wall reward can become locally optimal because `Q(wall) = wall_reward + gamma * V(s)` and `gamma * V(s)` is discounted. Default `-Inf` prevents this unconditionally.

**Tie-breaking.** When actions tie on Q-value, the first in `c("up","down","left","right")` wins. `V*` is always exact; treat tied arrows as arbitrary. Use Q-value debug to inspect ties:
```r
sid <- lookup_state(res$states, 1, 2, c(k1=1L))
for (a in c("up","down","left","right")) {
  tr  <- transition(env, res$states, sid, a)
  val <- tr$reward + env$gamma * res$V[as.character(tr$next_id)]
  cat(sprintf("%s: Q=%.6f\n", a, val))
}
```

**`minimum_reward()` limitations.** Analytical, k=1 only, assumes Manhattan-distance paths. Does not account for traps or obstacles on the detour path. For k>1 or complex topologies, use empirical binary search instead.

**`lookup_state()` scaling.** Uses linear scan — fine up to ~10×10 grids with a few reward counters. Beyond that, replace with a pre-built hash index.
