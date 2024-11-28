# in nn1 it was from scratch. now lets introduce abstractions

n_obs <- 200
ft_in <- 8
hidden1 <- 24
hidden2 <- 48
hidden3 <- 8

# implement an MLP in a sequential manner
mlp <- nn_sequential(
  nn_linear(ft_in, hidden1),
  nn_relu(),
  nn_linear(hidden1, hidden2),
  nn_relu(),
  nn_linear(hidden2, 1)
)

# Call the MLP using a tensor of shape N-Observations X N-Features
mlp(torch_randn(n_obs, ft_in))


# Custom modules ----------------------------------------------------------

n_obs <- 200
ft_in <- 8

# construct custom models

my_linear <- nn_module(
  initialize = function(in_features, out_features) {
    self$w <- nn_parameter(torch_randn(
      in_features, out_features
    ))
    self$b <- nn_parameter(torch_zeros(out_features))
  },
  forward = function(input) {
    input$mm(self$w) + self$b
  }
)

# call it as a single layer (output features is 1)
my_linear(ft_in,1)
# or to/from hidden layers
my_linear(hidden1,hidden2)


