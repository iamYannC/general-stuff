# MDP Grid World Framework

Value iteration on a finite, deterministic grid MDP. Supports custom rewards, k-collectable bonuses, trap cells, and wall penalties. All coordinates are **1-indexed**, `c(row, col)`, origin at top-left.

---

## Function reference

### `make_env()`

Defines the grid topology and global dynamics. This is always the first call — everything else attaches to the object it returns.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `rows`, `cols` | int | — | Grid dimensions |
| `start` | `c(r,c)` | — | Agent starting position |
| `terminal` | `c(r,c)` | — | Absorbing goal state. V = 0 here by definition |
| `step_cost` | numeric | `-1` | Applied on every valid transition |
| `wall_reward` | numeric | `1e-6` | Reward when agent attempts an out-of-bounds move. Agent stays in place. Set to a large negative value to actively penalise wall-hitting |
| `gamma` | numeric | `1.0` | Discount factor. Must be in `(0, 1]`. Use `< 1` when repeated bonus collection is possible, to guarantee convergence |

---

### `add_reward()`

Attaches a collectable reward to a cell. Can be called multiple times on the same env to place multiple reward cells. Returns the updated env.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `env` | list | — | Output of `make_env()` |
| `pos` | `c(r,c)` | — | Cell location |
| `value` | numeric | — | Reward received on collection |
| `k` | int or `Inf` | `1` | Max times reward can be collected. `k=1` = one-shot. `Inf` = unlimited (requires `gamma < 1`) |

> **State space note.** Each finite-`k` reward adds a collection counter to the state. A cell with `k=3` contributes 4 counter states (0, 1, 2, 3). Two such cells multiply the state space by 4×4 = 16. Still tractable for small grids.

---

### `add_trap()`

Marks a cell as a trap. When the agent steps on it, it is immediately teleported to `dest` and all collection counters are reset to zero. The step cost still applies; an optional extra `reward` can be specified (positive or negative).

| Parameter | Type | Default | Description |
|---|---|---|---|
| `env` | list | — | Output of `make_env()` or a prior `add_reward()` call |
| `pos` | `c(r,c)` | — | Trap cell location |
| `dest` | `c(r,c)` | — | Teleport destination. Must be a fixed cell |
| `reward` | numeric | `0` | Extra reward on top of `step_cost` when trap is triggered |

---

### `solve_mdp()`

Runs value iteration to convergence. Internally calls `build_state_space()` and `transition()` - the real heavy lifters.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `env` | list | — | Fully configured environment |
| `theta` | numeric | `1e-6` | Convergence threshold on `max|V_new - V_old|` |
| `max_iter` | int | `1000` | Hard iteration cap |

**Returns a named list:**

| Field | Description |
|---|---|
| `$states` | Tibble of the full state space: `row`, `col`, `k1`, `k2`, ..., `state_id` |
| `$V` | Named numeric vector. Keys are `state_id`. Final converged values |
| `$policy` | Named character vector. Keys are `state_id`. Values: `"up"`, `"down"`, `"left"`, `"right"` |
| `$history` | List of tibbles, one per iteration. Each has the same schema as `$states` plus `V`, `policy`, `iter`, `delta` |
| `$history_tbl` | All iterations bound into one long-format tibble. Ready for `ggplot2` |
| `$n_iter` | Number of iterations until convergence |
| `$env` | The original environment (passed through for downstream use) |

---

### `run_experiments()`

Runs `solve_mdp()` over a named list of environments and aggregates the results. Useful for comparing different reward structures, grid sizes, or trap configurations.

| Parameter | Type | Default | Description |
|---|---|---|---|
| `experiments` | named list | — | Each element is a fully configured env |
| `theta` | numeric | `1e-6` | Forwarded to `solve_mdp()` |
| `max_iter` | int | `1000` | Forwarded to `solve_mdp()` |

**Returns a named list:**

| Field | Description |
|---|---|
| `$results` | Named list of `solve_mdp()` outputs, one per experiment |
| `$summary` | One row per experiment: `experiment`, `n_states`, `n_iter`, `start_value`, `final_delta` |
| `$history_tbl` | All experiments, all iterations, bound into one tibble. Has an `experiment` column prepended |

---

### `print_grid()`

Prints `V` or `policy` as a matrix to the console. When collection counters exist, slices to a specific counter combination (defaults to all-zero, i.e. no rewards yet collected).

| Parameter | Type | Default | Description |
|---|---|---|---|
| `result` | list | — | Output of `solve_mdp()` |
| `what` | `"V"` or `"policy"` | `"V"` | Which quantity to display |
| `counters` | named int vector | `NULL` | e.g. `c(k1=1, k2=0)` to see the grid after collecting reward 1 once. Defaults to all zeros |

---

## `$history_tbl` schema

This is the primary output for your own analysis and visualisation. One row per `(experiment, state, iteration)`.

| Column | Type | Description |
|---|---|---|
| `experiment` | chr | Experiment name (only present in `run_experiments()` output) |
| `row`, `col` | int | Grid position |
| `k1`, `k2`, ... | int | Collection counter per reward cell (only present if `k` is finite) |
| `state_id` | int | Unique state identifier |
| `V` | dbl | Value estimate at this iteration |
| `policy` | chr | Greedy action at this iteration |
| `iter` | int | Iteration number |
| `delta` | dbl | Max Bellman residual across all states at this iteration |

---

## Complete workflow

```r
library(tidyverse)
source("mdp_gridworld.R")

# 1. Define environments
env_A <- make_env(
  rows = 4, cols = 4,
  start    = c(4, 1),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = 1e-6,
  gamma       = 1.0
) |>
  add_reward(pos = c(1, 4), value = 5, k = 1) |>
  add_reward(pos = c(2, 2), value = 1, k = 1) |>
  add_trap(  pos = c(3, 3), dest = c(4, 1), reward = 0)

env_B <- make_env(
  rows = 4, cols = 4,
  start    = c(4, 1),
  terminal = c(1, 1),
  step_cost   = -1,
  wall_reward = 1e-6,
  gamma       = 0.95       # gamma < 1 required with Inf or k > 1
) |>
  add_reward(pos = c(1, 4), value = 5, k = 3)  # collectable 3 times

# 2. Run experiments
exp <- run_experiments(list(with_trap = env_A, repeatable = env_B))

# 3. Compare summary metrics
exp$summary
#> # A tibble: 2 × 5
#>   experiment  n_states n_iter start_value final_delta
#>   <chr>          <int>  <int>       <dbl>       <dbl>
#> 1 with_trap         64     12          -3           0
#> 2 repeatable        80     47          ...          0

# 4. Inspect grids for a single environment
res_A <- exp$results$with_trap
print_grid(res_A, "V")
print_grid(res_A, "policy")

# 5. Visualise convergence across experiments
exp$history_tbl |>
  group_by(experiment, iter) |>
  summarise(max_delta = first(delta), .groups = "drop") |>
  ggplot(aes(x = iter, y = max_delta, colour = experiment)) +
  geom_line() +
  scale_y_log10() +
  labs(title = "Convergence", x = "Iteration", y = "Max delta (log scale)") +
  theme_minimal()

# 6. Animate policy evolution for one experiment (example: ggplot facet by iter)
exp$history_tbl |>
  filter(experiment == "with_trap", k1 == 0, k2 == 0) |>
  ggplot(aes(x = col, y = -row, label = policy)) +
  geom_tile(aes(fill = V), colour = "white") +
  geom_text(colour = "white", size = 4) +
  scale_fill_viridis_c() +
  facet_wrap(~iter) +
  theme_void() +
  labs(title = "Policy evolution across iterations")
```

---

## Design notes

**Why counters reset on trap.** The trap teleports the agent back to start; it is assumed the agent loses any collected bonuses on reset. If you want counters to persist through a trap, change the single line in `transition()` that sets `new_counters <- setNames(rep(0L, ...), ...)`.

**Tie-breaking.** When multiple actions achieve the same Q-value, the code picks whichever appears first in `c("up","down","left","right")`. This can produce misleading arrows at boundary cells. The value function `V` is always correct; treat tied arrows as arbitrary.

**Gamma and unbounded rewards.** If any reward has `k = Inf` and `gamma = 1.0`, the value function diverges. The code will hit `max_iter` without converging. Always set `gamma < 1` when using unlimited rewards.

**Scaling.** `lookup_state()` uses linear scan. For grids up to ~10×10 with a few collectable cells this is negligible. Beyond that, replace it with a pre-built integer index (`match()` on a hash key).
