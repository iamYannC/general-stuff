library(tidyverse)
library(mirai)
library(matrixStats)
library(glue)

# Algorithm functions -----------------------------------------------------

# current implementation // modified a bit, to remove the converged value
current <- function(n_simulations, vector_size) {
  iterations <- integer(n_simulations)
  final_value <- integer(n_simulations)
  for (i in 1:n_simulations) {
    x <- 1:vector_size
    k <- 0
    while (length(unique(x)) != 1) {
      k <- k + 1
      x <- sample(x, vector_size, replace = TRUE)
    }
    iterations[i] <- k
    final_value[i] <- x[1]
  }
  # list(i = iterations, v = final_value)
  iterations
}

# new seq implementation:

sequential_sim <- \(n_simulations, vector_size){
  store_iterations <- integer(n_simulations)

  for (i in 1:n_simulations) {
    iteration <- 0
    v <- 1:vector_size

    while (length(unique(v)) > 1) {
      iteration <- iteration + 1
      v <- sample(v, vector_size, replace = TRUE)
    }
    store_iterations[i] <- iteration
  }
  return(store_iterations)
}

# parallel (cant use on posit server, no workers allocation)
chunk_sim <- function(reps, vector_size) {
  vapply(seq_len(reps), function(i) {
    v <- 1:vector_size
    c <- 0
    while (length(unique(v)) > 1) {
      v <- sample(v, vector_size, TRUE)
      c <- c + 1
    }
    c
  }, numeric(1))
}

parallel_sim <- \(n_sim, vector_size) {
  n_workers <- parallel::detectCores() - 1
  n_chunks <- min(n_sim, n_workers * 4)
  chunk_sizes <- diff(floor(seq(0, n_sim, length.out = n_chunks + 1)))

  unlist(mirai_map(chunk_sizes, chunk_sim, .args = list(vector_size = vector_size))[])
}


# vectorized
vectorized_sim <- function(n_simulations, vector_size) {
  N <- vector_size
  if (N <= 1) {
    return(integer(n_simulations))
  }

  V <- matrix(1:N, nrow = n_simulations, ncol = N, byrow = TRUE)
  orig_idx <- seq_len(n_simulations)
  store_iterations <- integer(n_simulations)
  iter <- 0L

  n_active <- n_simulations
  row_id <- rep(seq_len(n_active), times = N) # built once, reused while n_active is unchanged

  while (n_active > 0) {
    iter <- iter + 1L

    col_id <- sample.int(N, n_active * N, replace = TRUE)
    lin_idx <- row_id + (col_id - 1L) * n_active # column-major linear index, no cbind()
    V <- V[lin_idx]
    dim(V) <- c(n_active, N) # in-place reshape, no matrix() copy

    converged <- rowMaxs(V) == rowMins(V) # no n_active x N comparison matrix

    if (any(converged)) {
      keep <- !converged
      store_iterations[orig_idx[converged]] <- iter
      V <- V[keep, , drop = FALSE]
      orig_idx <- orig_idx[keep]
      n_active <- nrow(V)
      if (n_active > 0) row_id <- rep(seq_len(n_active), times = N) # rebuilt only on shrink
    }
  }

  store_iterations
}

# Analysis functions ------------------------------------------------------

# run the benchmark on a grid
benchmark <- \(funs_to_test,
  n_sims,
  vector_sizes
){
  Ns <- vector_sizes

  everything <- replicate(length(funs_to_test), replicate(length(n_sims), replicate(length(Ns), vector("list", 2), simplify = F), simplify = F), simplify = FALSE)
  # number of functions > n of simulations > vector length > 2 elements: raw dist & time

  for (f in seq_along(funs_to_test)) {
    print(glue("testing function {f} out of {length(funs_to_test)}\n{names(funs_to_test[f])}"))
    for (s in seq_along(n_sims)) {
      print(glue("executing {scales::comma(n_sims[s])} simulations:"))

      for (i in seq_along(Ns)) {
        print(glue("vector size = {Ns[i]}"))

        begin <- Sys.time()
        raw_dist <- funs_to_test[[f]](n_sims[s], Ns[i])
        end <- Sys.time()


        # populate with raw results (number of iterations at each simulation, its a distribution per N/S)
        everything[[f]][[s]][[i]][[1]] <- raw_dist
        everything[[f]][[s]][[i]][[2]] <- as.numeric(end - begin, units = "secs")
      }
    }
  }

  everything
}

# create a df from the list
bench2df <- \(results_from_benchmark, n_sims, vector_sizes){
  results_named <- results_from_benchmark |>
    set_names(names(funcs)) |>
    map(\(x) set_names(x, n_sims) |>
      map(\(y) set_names(y, vector_sizes)))

  # Extract and build the dataframe
  tibble(
    function_type = rep(names(funcs), each = length(n_sims) * length(vector_sizes)),
    simulations = rep(rep(n_sims, each = length(vector_sizes)), length(funcs)),
    v_size = rep(rep(vector_sizes, length(n_sims)), length(funcs)),
    time = map(results_named, \(f) map(f, \(s) map_dbl(s, 2))) |> unlist(),
    raw_dist = map(results_named, \(f) map(f, \(s) map(s, 1))) |> flatten() |> flatten()
  )
}


# show the distribution
plot_raw_dist <- function(df) {
  df_long <- df |>
    tidyr::unnest(raw_dist)

  ggplot2::ggplot(df_long, ggplot2::aes(x = raw_dist, fill = function_type)) +
    ggplot2::geom_density(alpha = 0.5, color = NA) +
    ggplot2::facet_grid(
      scales = "free",
      rows = ggplot2::vars(simulations),
      cols = ggplot2::vars(v_size),
      labeller = ggplot2::labeller(
        simulations = function(x) paste0("# simulations: ", x),
        v_size = function(x) paste0("N = ", x)
      )
    ) +
    labs(fill = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 14),
      legend.position = "bottom"
    )
}

# show the time as function of vector size
plot_time <- function(df) {
  df |>
    mutate(v_size = as.factor(v_size)) |>
    ggplot(aes(v_size, time, color = function_type, group = function_type)) +
    geom_line() +
    geom_point(show.legend = F) +
    scale_x_discrete(expand = 0.05) +
    labs(color = NULL, x = "Vector size", y = "Time (s)") +
    facet_wrap(vars(simulations),
      scales = "free", ncol = 1,
      labeller = labeller(simulations = function(x) paste0("# simulations: ", x))
    ) +
    theme(legend.position = "top", text = element_text(size = 14))
}
