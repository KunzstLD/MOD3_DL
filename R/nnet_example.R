
# iris example
data <- Filter(is.numeric, iris)
setDT(data)
data <- melt(data, measure.vars = names(data))
data[, mean_val_by_grp := mean(value), by = variable]
ggplot(data, aes(x = value)) +
  geom_density(fill = "gray",
               alpha = 0.7) +
  geom_vline(
    aes(xintercept = mean_val_by_grp),
    color = "steelblue",
    linetype = "dashed",
    size = 1
  ) +
  facet_wrap( ~ as.factor(variable)) +
  labs(x = "Value",
       y = "Density") +
  theme_classic()

# if data follow a more uniform distribution:
# normalize data; range [0, 1]
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
norm_iris <- lapply(iris[, -5], normalize) %>%
  as.data.frame() %>%
  cbind(., "Species" = iris[, 5])

# checking
lapply(norm_iris, summary)

# Dt way
# iris[, lapply(.SD, normalize), .SDcols = c(
#   "Sepal.Length",
#   "Sepal.Width",
#   "Petal.Length",
#   "Petal.Widt"
# )]

# if the data follow more a normal distribution: 
# standardize (i.e. mean = 0 and sd = 1)
stand_iris <- scale(iris[, -5], center = TRUE, scale = TRUE) %>%
  as.data.frame() %>%
  cbind(., "Species" = iris[, 5])

lapply(stand_iris, summary)

# Divide data into test and training data
# Alternative: createDataPartition
# Does sample from within data partitions!
index <- sample(1:nrow(stand_iris), ceiling(0.75 * nrow(stand_iris)))
train <- stand_iris[index, ]
test <- stand_iris[-index, ]

# From the docs: "The random sampling is done within the levels of y when 
# y is a factor in an attempt to balance the class distributions within the splits"
ind <- createDataPartition(stand_iris$Species, p = 0.75)
train <- stand_iris[ind[[1]], ]
test <- stand_iris[-ind[[1]], ]

# Check that response variable is a factor!
str(stand_iris)


# Can use a grid of tuning parameters
# Calculates resampling based performance measure (few data are held out)
# access to parameters in nnet()
# lineout = TRUE , linear output nodes, otherwise logistic/sigmoid output nodes
# the hidden layers use the sigmoid activation function
# classification when response is a factor
# Pretty handy function from caret::train()
# can use these models here: 
# http://topepo.github.io/caret/train-models-by-tag.html
# Or define a custom model:
# http://topepo.github.io/caret/using-your-own-model-in-train.html
# preprocessing with preProcess function

# nnet: either linear activation function (linout = TRUE) or logistic activation
# function (linout = FALSE)
test_net <- train(
  Species ~ .,
  data = train,
  method = 'nnet', 
  tuneGrid = expand.grid(size = 5, # hidden units, 
  # can be more complex to test different hyperparameters
                         decay = 5e-4), # Weight decay
  maxit = 50,
  metric = "Accuracy"
)

# Information about the model
test_net

# getModelInfo(model = "nnet")
# Plot our neural network
plotnet(test_net)

# Alternative way of specifying the model:
# train(
#   train[, 1:4],
#   train[, 5],
#   method = 'nnet', 
#   tuneGrid = expand.grid(size = 5,
#                          decay = 5e-4),
#   maxit = 100,
#   metric = "Accuracy"
# )

# Predictions on training data:
pred_train <- predict(test_net, newdata = train)
conf1 <- caret::confusionMatrix(pred_train, train$Species)

# Predictions on test data:
pred_test <- predict(test_net, newdata=test)
conf2 <- confusionMatrix(pred_test, test$Species)

# Get the probabilities for each species in the test set
predict(test_net,
        newdata = test,
        type = 'prob') %>% 
        apply(., 2, function(y) round(y, digits = 4))


# Variable importance based on weights
# (for single hidden layer neural networks)
# Deconstucion of model weights
# The relative importance (or strength of association) of a specific explanatory variable
# for the response variable can be determined by identifying all weighted connections between 
# the nodes of interest.
# That is, all weights connecting the specific input node that pass through the hidden layer
# to the response variable are identified. This is repeated for all other explanatory variables
# until a list of all weights that are specific to each input variable is obtained. 
# The connections are tallied for each input node and scaled relative to all other inputs. 
# A single value is obtained for each explanatory variable that describes the relationship 
# with the response variable in the model (see the appendix in Goh 1995 for a more detailed description).
# The original algorithm indicates relative importance as the absolute magnitude from
# zero to one such the direction of the response cannot be determined.
garson(test_net)