# now we are starting to use optimizers, after learning also about loss functions
  # mainly, the LBFGS and the line search, as added to optim_lbfgs

library(torch)

a <- 1
b <- 5

rosenbrock <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  (a - x1)^2 + b * (x2 - x1^2)^2
}

num_iterations <- 5

x <- torch_tensor(c(-1, 1), requires_grad = TRUE)

optimizer <- optim_lbfgs(x,line_search_fn = "strong_wolfe")

calc_loss <- function() {
  optimizer$zero_grad()
  
  value <- rosenbrock(x)
  cat("Value is: ", as.numeric(value), "\n")
  
  value$backward()
  value
}

for (i in 1:num_iterations) {
  cat("\nIteration: ", i, "\n")
  optimizer$step(calc_loss)
  print(x)
}


# CHAPTER 11: using optimizers --------------------------------------------

# repeating code from nn1 and 2 to init x, y and mlp network

n_obs <- 200
ft_in <- 8
hidden1 <- 24
hidden2 <- 48
hidden3 <- 8

x <- torch_randn(n_obs, ft_in)
coefs <- round(sample(runif(ft_in,-2,5)),1)
y <- x$matmul(coefs)$unsqueeze(2) + torch_randn(n_obs, 1)

# implement an MLP in a sequential manner
mlp <- nn_sequential(
  nn_linear(ft_in, hidden1),
  nn_relu(),
  nn_linear(hidden1, hidden2),
  nn_relu(),
  nn_linear(hidden2, 1)
)

opt <- optim_adam(mlp$parameters)

n_epochs <- 200
for (t in 1:n_epochs) {
  
  ### -------- Forward pass --------
  y_pred <- mlp(x)
  
  ### -------- Compute loss -------- 
  loss <- nnf_mse_loss(y_pred, y)
  if (t %% 10 == 0)
    cat("Epoch: ", t, "   Loss: ", loss$item(), "\n")
  
  ### -------- Backpropagation --------
  opt$zero_grad()
  loss$backward()
  
  ### -------- Update weights -------- 
  opt$step()
  
}

## Now execatly as the code in chp11
{
  # input dimensionality (number of input features)
  d_in <- 3
  # number of observations in training set
  n <- 100
  
  x <- torch_randn(n, d_in)
  coefs <- c(0.2, -1.3, -0.5)
  y <- x$matmul(coefs)$unsqueeze(2) + torch_randn(n, 1)
  
  # dimensionality of hidden layer
  d_hidden <- 32
  # output dimensionality (number of predicted features)
  d_out <- 1
  
  net <- nn_sequential(
    nn_linear(d_in, d_hidden),
    nn_relu(),
    nn_linear(d_hidden, d_out)
  )
  
  opt <- optim_adam(net$parameters)
  
  ### training loop --------------------------------------
  
  for (t in 1:200) {
    
    ### -------- Forward pass --------
    y_pred <- net(x)
    
    ### -------- Compute loss -------- 
    loss <- nnf_mse_loss(y_pred, y)
    if (t %% 10 == 0)
      cat("Epoch: ", t, "   Loss: ", loss$item(), "\n")
    
    ### -------- Backpropagation --------
    opt$zero_grad()
    loss$backward()
    
    ### -------- Update weights -------- 
    opt$step()
    
  }
  
}
