# load libraries ####
pkgs = c('caret', 'keras')
lapply(pkgs, library, character.only = T)

# load data ####
data = read.csv('/home/rodox/00.git/02.commercial_model/agencia_bancaria_model_total.csv')
str(data)
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

train_labels = data[train_part, 1:8]
train_targets = data[train_part, 9]
test_labels = data[-train_part, 1:8]
test_targets = data[-train_part, 9]

# build the model ####
model = keras_model_sequential()

model %>%
  layer_dense(units = 64, activation = 'relu', input_shape = ncol(train_labels)) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 1)

model %>%
  compile(loss = 'mse', optimizer = optimizer_rmsprop(),
          metrics = list('mean_absolute_error'))

summary(model)

# fit the model
history_fit = model %>%
  fit(train_labels, train_targets, epochs = 100,
      validation_split = 0.2, verbose = 1)

# plot(history)
plot(history, metrics = 'mean_absolute_error')
plot(history, metrics = 'mean_absolute_error', smooth = FALSE) +
  coord_cartesian(ylim = c(0, 5))

# evaluate the model
model %>%
  evaluate(test_labels, test_targets)