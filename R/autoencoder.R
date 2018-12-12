
# Using mnist data
mnist <- dataset_mnist()

# Get train / test
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y
# reshape
dim(x_train) <- c(nrow(x_train), 784)
dim(x_test) <- c(nrow(x_test), 784)
# rescale
x_train <- x_train / 255
x_test <- x_test / 255
# one-hot encode to categories
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# Model hyperparamters
encoding_dim <- 2 # latent dimensions
input_dim <- 784  # input dim

# Create secuential model as an Autoencoder
model <- keras_model_sequential() %>%
  # Encoder
  layer_dense(4 * encoding_dim, input_shape=input_dim, activation='relu') %>%
  layer_dense(2 * encoding_dim, activation='relu') %>%
  layer_dense(encoding_dim, activation='relu', name="latent") %>%
  # Decoder
  layer_dense(2 * encoding_dim, activation='relu') %>%
  layer_dense(4 * encoding_dim, activation='relu') %>%
  layer_dense(input_dim, activation='relu')

# Configure a model for categorical classification.
model %>% compile(
  loss = "mse",
  optimizer = optimizer_adam(lr = 0.001),
  metrics = c("binary_crossentropy")
)
summary(model) # Plot summary

# Fit model
model %>% fit(
  x_train,
  x_train,
  epochs = 100,
  batch_size = 250
)

# Evaluate in test
model %>% evaluate(x_test, x_test, verbose = 1)

# Get intermediate layer
hidden_layer <- keras_model(inputs = model$input,
                            outputs = model$get_layer("latent")$output)

set.seed(123456)
ids <- sample(nrow(x_test), 100)
x_test_sample <- x_test[ids, ]
# Get intermediate layer
latent_hat <- predict(hidden_layer, x_test_sample)
plot(latent_hat, col= mnist$test$y[ids])
# Plot sample numbers
text(latent_hat, labels=mnist$test$y[ids], cex= 0.7)
text(latent_hat, labels=ids, cex= 0.8, srt=90, pos=4, col=2)

# Function to plot together two numbers
plotDigit <- function(idOne, idTwo, x_test, y) {
  print(paste("Digits:",y[idOne],",",y[idTwo],sep=" "))
  par(mfrow=c(1,2))
  image(t(matrix(x_test[idOne,], ncol = 28, nrow = 28)), col  = gray((0:32)/32))
  image(t(matrix(x_test[idTwo,], ncol = 28, nrow = 28)), col  = gray((0:32)/32))
  par(mfrow=c(1,1))
}
# Plot digits together
plotDigit(3344,391, x_test, mnist$test$y)
plotDigit(800,30, x_test, mnist$test$y)
plotDigit(3329,1284, x_test, mnist$test$y)
