# functions: current, sequential_sim, parallel_sim, vectorized_sim, vectorized_sim2

# vectorized vs sequential_sim
funcs <- list(seq = sequential_sim, vectorized = vectorized_sim)
sims <- c(100, 500)
vsizes <- c(10, 500, 1000)

daemons(n = parallel::detectCores() - 1, seed = 42)
results <- benchmark(funcs, n_sims = sims, vector_sizes = vsizes)
daemons(0)

df <- bench2df(
  results_from_benchmark = results,
  n_sims = sims,
  vector_sizes = vsizes
)

# verify distribution
plot_raw_dist(df)
plot_time(df)

# paralell vs paralell2

funcs <- list(parallel2 = parallel_sim2, paralell1 = parallel_sim)
sims <- c(3000)
vsizes <- c(1000)

daemons(n = parallel::detectCores() - 1, seed = 42)
results <- benchmark(funcs, n_sims = sims, vector_sizes = vsizes)
daemons(0)

df <- bench2df(
  results_from_benchmark = results,
  n_sims = sims,
  vector_sizes = vsizes
)

# verify distribution
plot_raw_dist(df)
plot_time(df)
