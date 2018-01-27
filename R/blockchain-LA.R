# basic structure linear algebra

sigmoid <- function(x)
  1 / (1 + exp(-x))
sig_to_der <- function(x)
  x * (1 - x)

# generate
x1 <- rbinom(50000, 1, 0.5)
x2 <- rbinom(50000, 1, 0.5)
x3 <- rbinom(50000, 1, 0.5)

# define Y as being equal to x1
# and write Y to the Global Environment
Y <- ifelse(x1 == x2, 0, 1)

X <- cbind(x1, x2, x3)

# print the top of the data
head( cbind(Y,X) )

n = dim(X)[1] # the number of rows or observations in X
input_dim = dim(X)[2]
output_dim = 1
# choose a hidden_dim
hidden_dim = 4

library(abind)
library(digest)

# initialize weights randomly between -1 and 1, with mean 0
weights_0 = matrix(runif(n = input_dim * output_dim,
                         min = -1,
                         max = 1
                         ),
                   nrow = input_dim,
                   ncol = hidden_dim)
weights_1 = matrix(runif(n = hidden_dim * output_dim,
                         min = -1,
                         max = 1),
                   nrow = hidden_dim,
                   ncol = output_dim)

weights_0_list <- vector("list", n+1)
weights_1_list <- vector("list", n+1)

weights_0_list[[1]] <- weights_0
weights_1_list[[1]] <- weights_1

block = blockchain( length(blockchain) )

digest(block,algo="sha256")

newblock = list(
  index = length (blockchain) + 1,
  timestamp = as.numeric(Sys.time()) ,
  prevHash = ,
  weights_0,
  weights_1
)



for (j in 1:n) {
  # Feed forward through layers 0, 1, and 2
  layer_0 = X[j, , drop = FALSE]
  layer_1 = sigmoid( layer_0 %*% weights_0_list[[j]] )
  layer_2 = sigmoid( layer_1 %*% weights_1_list[[j]] )
  # how much did we miss the target value?
  layer_2_error = Y[j] - layer_2
  if (j %% 10000 == 0)
    print(paste("Error:", mean(abs(layer_2_error))))
  # in what direction is the target value?
  # were we really sure? if so, don't change too much.
  layer_2_delta = layer_2_error * sig_to_der(layer_2)
  # how much did each layer_1 value contribute to
  # the layer_2 error (according to the weights)?
  layer_1_error = layer_2_delta %*% t(weights_1)
  # in what direction is the target layer_1?
  # were we really sure? if so, don't change too much.
  layer_1_delta = layer_1_error * sig_to_der(layer_1)
  # how much did layer_1 value contribute
  # to the error (according to the weights)?
  weights_1 = weights_1 + t(layer_1) %*% layer_2_delta
  weights_0 = weights_0 + t(layer_0) %*% layer_1_delta

  # add to chain
  weights_0_list[[j+1]] = weights_0
  weights_1_list[[j+1]] = weights_1
}
