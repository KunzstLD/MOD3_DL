#___________________________________________________________________________________________________
# Solution classification of nanoparticles using nnet()
# Task: 
# Previous analysis of the data suggested that the following masses seem to
# be important to distinguish samples that contained fulvic acid from samples
# without fulvic acid:
#  Mass_39_03, 
#  Mass_38_02, 
#  Mass_65_04, 
#  Mass_53_04, 
#  Mass_45_98
  
# 1. Create a neural network classifier with only these 5 parameters, predicting whether
# a sample contained fulvic acid or not. 
# Bonus: vary the number of hidden units between 2 and 5.
# 2. How many weights has the neural network? 
# 3. Compare the results with a logistic regression model

#___________________________________________________________________________________________________

# Read in data
nanopart <- readRDS(file.path(data_in, "nanopart_preproc.RDS"))

#___________________________________________________________________________________________________
#### Standardisation ####
#___________________________________________________________________________________________________

# scaling (mean zero, sd 1)
stand_nanopart <-
  scale(nanopart[, -ncol(nanopart)], center = TRUE, scale = TRUE) %>%
  as.data.frame() %>%
  cbind(., "fulvic_acid" = nanopart[, "fulvic_acid"])

# look at the columns
lapply(stand_nanopart, summary)

#___________________________________________________________________________________________________
#### Create training and test dataset
#___________________________________________________________________________________________________
ind <- createDataPartition(stand_nanopart$fulvic_acid, p = 0.75)
train <- stand_nanopart[ind[[1]],]
test <- stand_nanopart[-ind[[1]],]

#___________________________________________________________________________________________________
#### Training ####
#___________________________________________________________________________________________________

# set the seed for reproducible results
set.seed(12345)

# train neural network
nn_nanopart <- train(
  fulvic_acid ~ Mass_39_03 + Mass_38_02 + Mass_65_04 + Mass_53_04 + Mass_45_98,
  data = train,
  method = "nnet",
  tuneGrid = expand.grid(size = 2:5,
                         decay = 5e-4),
  maxit = 50,
  metric = "Accuracy"
)

# Check output, states which final model was used by caret
nn_nanopart

# plot network
plotnet(nn_nanopart)

# predict on training data
result_train <- predict(nn_nanopart, newdata = train)
conf_train <-confusionMatrix(result_train, train$fulvic_acid)

# predictions on test data:
result_test <- predict(nn_nanopart, newdata = test)
conf_test <- confusionMatrix(result_test, test$fulvic_acid)

# probabilities
predict(nn_nanopart,
        newdata = test,
        type = 'prob')

#___________________________________________________________________________________________________
#### Number of weights/parameters ####
#___________________________________________________________________________________________________
# 36 weights
nn_nanopart$finalModel

# calculation
5*5 + 5*1 + 5+1

#___________________________________________________________________________________________________
#### Training & predictions GLM ####
#___________________________________________________________________________________________________
glm_nanopart <- train(
  fulvic_acid ~ Mass_39_03 + Mass_38_02 + Mass_65_04 + Mass_53_04 + Mass_45_98,
  data = train,
  method = "glm",
  family = "binomial"
)

# predict on training data
result_train_glm <- predict(glm_nanopart, newdata = train)
# cbind(result_train_glm, ground_truth = train[, "fulvic_acid"])
conf_train_glm <-confusionMatrix(result_train_glm, train$fulvic_acid)

# Predictions on test data:
result_test_glm <- predict(glm_nanopart, newdata = test)
conf_test_glm <- confusionMatrix(result_test_glm, test$fulvic_acid)

# probabilities
predict(glm_nanopart,
        newdata = test,
        type = 'prob')