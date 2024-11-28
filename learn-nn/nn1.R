library(torch)

# input dimensionality (number of input features)
d_in <- 5
# number of observations in training set
n <- 100

x <- torch_randn(n, d_in)
coefs <- round(sample(runif(d_in,-2,5)),1)
y <- x$matmul(coefs)$unsqueeze(2) + torch_randn(n, 1)


# build network -----------------------------------------------------------

# dimensionality of hidden layer 1
d_hidden1 <- 32

# dimensionality of hidden layer 2
d_hidden2 <- 64

# output dimensionality (number of predicted features)
d_out <- 1

# weights connecting input to hidden layer 1
w1 <- torch_randn(d_in, d_hidden1, requires_grad = TRUE) # 3 to 32

# weights connecting  hidden layer 1 to hidden layer 2
w2 <- torch_randn(d_hidden1, d_hidden2, requires_grad = TRUE) # 32 to 64

# weights connecting hidden layer 2 to output layer
w3 <- torch_randn(d_hidden2, d_out, requires_grad = TRUE) # 64 to 1

# hidden layer 1 bias
b1 <- torch_zeros(1, d_hidden1, requires_grad = TRUE)

# hidden layer 2 bias
b2 <- torch_zeros(1, d_hidden2, requires_grad = TRUE)

# output layer bias
b3 <- torch_zeros(1, d_out, requires_grad = TRUE)


# train -------------------------------------------------------------------

learning_rate <- 0.0001
n_epochs <- 1000

keep <- tibble(epoch = rep(NA,n_epochs),
       loss = NA)

### training loop ----------------------------------------

for (t in 1:n_epochs) {
  
  ### -------- Forward pass --------
  
  y_pred <- x$mm(w1)$add(b1)$relu()$
              mm(w2)$add(b2)$relu()$
              mm(w3)$add(b3)
  
  ### -------- Compute loss -------- 
  loss <- (y_pred - y)$pow(2)$mean()
  if (t %% 10 == 0)
    cat("Epoch: ", t, "   Loss: ", loss$item(), "\n")
  
  keep[t,1] <- t
  keep[t,2] <- loss$item()
  
  ### -------- Backpropagation --------
  
  # compute gradient of loss w.r.t. all tensors with
  # requires_grad = TRUE
  loss$backward()
  
  ### -------- Update weights -------- 
  
  # Wrap in with_no_grad() because this is a part we don't 
  # want to record for automatic gradient computation
  with_no_grad({
    w1 <- w1$sub_(learning_rate * w1$grad)
    w2 <- w2$sub_(learning_rate * w2$grad)
    w3 <- w3$sub_(learning_rate * w3$grad)
    b1 <- b1$sub_(learning_rate * b1$grad)
    b2 <- b2$sub_(learning_rate * b2$grad)  
    b3 <- b3$sub_(learning_rate * b3$grad)  
    
    # Zero gradients after every pass, as they'd
    # accumulate otherwise
    w1$grad$zero_()
    w2$grad$zero_()
    w3$grad$zero_()
    b1$grad$zero_()
    b2$grad$zero_()  
    b3$grad$zero_()  
  })
  
}

layers <- glue("
               {d_in} X {d_hidden1} X {d_hidden2} X {d_out}
               ")

print(
keep %>% 
  ggplot(aes(epoch,loss))+geom_line(group=1) +
  theme_classic(base_size = 14) + 
  labs(title = glue("Learning rate = {learning_rate} with {n_epochs} epochs"),
       subtitle = glue("First loss = {round(keep[1,2],2)}, Last loss = {round(keep[nrow(keep),2],2)}"),
       caption = paste("Architecture:",layers)
) + theme(plot.caption = element_text(hjust = 0,
                                      size = rel(1.1))
          )
)
