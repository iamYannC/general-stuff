N <- 100
x <- sample(N,N,T)
x <- x+sample(x,N,F)


is_even <- (x %% 2 == 0) # 0.38
current_even <- is_even[-length(is_even)] # disregarding last one
next_even <- is_even[-1] # disregarding first, we're interested in next only


ee <- sum(current_even & next_even)
eo <- sum(current_even & !next_even)
oo <- sum(!current_even & !next_even)
oe <- sum(!current_even & next_even)

# transition probs
p_ee <- ee/(ee+eo)
p_eo <- 1-p_ee
p_oo <- oo/(oo+oe)
p_oe <- 1-p_oo


sim_trans <- rep(NA,N-1)
state <- ifelse(mean(is_even) > 0.5, 'E', 'O')

for( iter in 1:(N-1)){
  u <- runif(1)
  # in ODD
  if(state == 'O'){
    state <- ifelse(u < p_oo, 'O', 'E')
  }  else{
    state <- ifelse(u < p_ee, 'E', 'O')
  }
  sim_trans[iter] <- state
}
sim_evens <- sim_trans == 'E'
sim_curr_even <- sim_evens[-length(sim_evens)]
sim_next_even <- sim_evens[-1]

sim_ee_prob <- sum(sim_curr_even & sim_next_even) / sum(sim_curr_even)
sim_oo_prob <- sum(!sim_curr_even & !sim_next_even) / sum(!sim_curr_even)

print_empiric_sim <- function(label, emp_p, sim_p, rnd = 4) {
  cat(glue::glue("{label} Real: {round(emp_p, rnd)}, MC: {round(sim_p, rnd)}\n"))
}


print_empiric_sim("O->O", p_oo, sim_oo_prob)
print_empiric_sim("E->E", p_ee, sim_ee_prob)
