# load libraries
pkgs = c('caret', 'corrplot', 'dplyr', 'keras', 'kernlab', 'Metrics')
lapply(pkgs, library, character.only = T)

# load data ####
data = read.csv('/home/rodox/00.git/02.agencias_bancarias/agencia_bancaria_model_total.csv')
apply(data[, 1:8], 2, unique)

# change indepdent variable name to facilitate
# don't forget that 'cons' represents light and hvac consumption for the whole building
colnames(data)[ncol(data)] = 'cons'

# create dummy variables
dummy_model = dummyVars(cons ~ ., data = data)
dummy_data = data.frame(predict(dummy_model, newdata = data))
dummy_data$cons = data$cons

# split data into train and test sets
train_part = createDataPartition(dummy_data$cons, p = 0.8, list = F)
train_data = dummy_data[train_part, ]
test_data = dummy_data[-train_part, ]

# pre-process model
prep_obj = preProcess(train_data[, c(4, 7:9)], method = c('center', 'scale'))
train_data[, c(4, 7:9)] = predict(prep_obj, train_data[, c(4, 7:9)])
test_data[, c(4, 7:9)] = predict(prep_obj, test_data[, c(4, 7:9)])

# model ####
# set.seed(100)

# models characteristics
modelLookup('earth')
modelLookup('svmRadial')

# random forest ('earth') ####
# training control
rf_fit_ctrl <- trainControl(
  method = 'cv',
  number = 10,
  savePredictions = 'final',
  verboseIter = TRUE
)

# grid for hyperparameters optimization
rf_hyp_grid <-  expand.grid(nprune = c(5, 10, 15), degree = c(1, 2, 3, 4, 5))

# train
rf_fit = train(cons ~ ., data = train_data, trControl = rf_fit_ctrl,
                  tuneGrid = rf_hyp_grid, method = 'earth', metric = 'RMSE')
# show results
print(rf_fit)

# evaluation
model_targ = predict(rf_fit, newdata = test_data)
rmse(test_data$cons, model_targ)
mae(test_data$cons, model_targ)

# support vector machine ('svmRadial') ####
# train
svm_fit = train(cons ~ ., data = train_data, method = 'svmRadial', metric = 'RMSE')
# show results
print(svm_fit)

# evaluation
model_targ = predict(svm_fit, newdata = test_data)
rmse(test_data$cons, model_targ)
mae(test_data$cons, model_targ)
