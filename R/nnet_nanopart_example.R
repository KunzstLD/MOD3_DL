#___________________________________________________________________________________________________
# Classification of nanoparticles using nnet()
#___________________________________________________________________________________________________

# Load nanoparticle data
nanopart <- readRDS(file.path(data_in,"nanopart_preproc.RDS"))

# have a look at the dataset
View(nanopart)
names(nanopart)
str(nanopart$fulvic_acid)
dim(nanopart)

#___________________________________________________________________________________________________
#### Standardisation or normalisation ###
#___________________________________________________________________________________________________

# normalisation can be done using (here using the iris dataset):
# normalise <- function(x) {
#   (x - min(x)) / (max(x) - min(x))
# }
# norm_iris <- lapply(iris[, -5], normalise) %>%
#   as.data.frame() %>%
#   cbind(., "Species" = iris[, 5])

# scaling (mean zero, sd 1)
stand_nanopart <-
  scale(nanopart[, -ncol(nanopart)], center = TRUE, scale = TRUE) %>%
  as.data.frame() %>%
  cbind(., "fulvic_acid" = nanopart[, "fulvic_acid"])

# look at the columns
lapply(stand_nanopart, summary)

#___________________________________________________________________________________________________
#### Divide data into test and training data
#___________________________________________________________________________________________________

# Divide data into test and training data using the sample function
# index <- sample(1:nrow(stand_nanopart), ceiling(0.75 * nrow(stand_nanopart)))
# train <- stand_nanopart[index, ]
# test <- stand_nanopart[-index, ]

# createDataParatition() does sample from within factors
# From the docs: "The random sampling is done within the levels of y when 
# y is a factor in an attempt to balance the class distributions within the splits"
ind <- createDataPartition(stand_nanopart$fulvic_acid, p = 0.75)
train <- stand_nanopart[ind[[1]], ]
test <- stand_nanopart[-ind[[1]], ]

#___________________________________________________________________________________________________
#### Training ####
#___________________________________________________________________________________________________
# List of models that can be used with train: 
# http://topepo.github.io/caret/train-models-by-tag.html
# Classification: metric "Accuracy"
nn_nanopart <- train(
  fulvic_acid ~ .,
  data = train,
  method = 'nnet', 
  tuneGrid = expand.grid(size = 5, # hidden units
                         decay = 5e-4), # Weight decay (value when a weight gets removed)
  maxit = 100,
  metric = "Accuracy"
)

# Look at the model 
str(nn_nanopart)

# Plot our neural network
plotnet(nn_nanopart)

#___________________________________________________________________________________________________
#### Predictions ####
#___________________________________________________________________________________________________

# Predict on training data
result_train <- predict(nn_nanopart, newdata = train)
conf_train <- caret::confusionMatrix(result_train, train$fulvic_acid)

# predictions on test data:
results_test <- predict(nn_nanopart, newdata=test)
conf_test <- confusionMatrix(results_test, test$fulvic_acid)

# get the probabilities for each class in the test set
predict(nn_nanopart,
        newdata = test,
        type = 'prob')

#___________________________________________________________________________________________________
#### Variable importance ####
#___________________________________________________________________________________________________

# Variable importance based on weights
VI <- garson(nn_nanopart)
VI$data[VI$data$x_names == "Mass_39_03", ]
