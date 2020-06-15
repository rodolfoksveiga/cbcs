# load libraries ####
pkgs = c('caret', 'dplyr', 'keras', 'tensorflow')
lapply(pkgs, library, character.only = T)

# base functions ####
# create dummy variables
CreateDummies = function(data) {
  dummy_model = dummyVars(targ ~ ., data = data)
  dummy_data = data.frame(predict(dummy_model, newdata = data))
  dummy_data$targ = data$targ
  return(dummy_data)
}
# split data into train and test sets
SplitData = function(train, dummy_data, train_prop, seed = 100) {
  # reproduce
  set.seed(seed)
  train_part = createDataPartition(dummy_data$targ, p = train_prop, list = F)
  if(train) {
    data = dummy_data[train_part, ]
  } else {
    data = dummy_data[-train_part, ]
  }
  return(data)
}
# pre-process data
PPData = function(data, pp_model) {
  data = predict(pp_model, data)
  return(data)
}

data_path = '/home/rodox/git/commercial_model/data/data_sobol.csv'
data_extra_path = '/home/rodox/git/commercial_model/data/data_extra_rv.csv'
weather_var = 'ghr'

# load data
raw_data = read.csv(data_path)
raw_data_extra = read.csv(data_extra_path)
# data inspection
str(raw_data)
str(raw_data_extra)
# create dummy variables
dummy_data = CreateDummies(raw_data)
dummy_data_extra = raw_data_extra
dummy_data_extra$hvac.split = ifelse(dummy_data_extra$hvac == 'split', 1, 0)
dummy_data_extra$hvac.vrf = ifelse(dummy_data_extra$hvac == 'vrf', 1, 0)
dummy_data_extra$afn.max = ifelse(dummy_data_extra$afn == 'max', 1, 0)
dummy_data_extra$afn.min = ifelse(dummy_data_extra$afn == 'min', 1, 0)
dummy_data_extra$boundaries.adiabatic = ifelse(dummy_data_extra$boundaries == 'adiabatic', 1, 0)
dummy_data_extra$boundaries.outdoors = ifelse(dummy_data_extra$boundaries == 'outdoors', 1, 0)
# rearrange data
if (weather_var == 'ghr') {
  dummy_data = select(dummy_data, hvac.split, hvac.vrf, envelope, lights,
                      shgc, afn.max, afn.min, azimuth, boundaries.adiabatic,
                      boundaries.outdoors, ghr, targ)
  test_data_extra = select(dummy_data_extra, hvac.split, hvac.vrf, envelope, lights,
                           shgc, afn.max, afn.min, azimuth, boundaries.adiabatic,
                           boundaries.outdoors, ghr, targ)
  pp_vars = c('envelope', 'lights', 'shgc', 'azimuth', 'ghr')
} else if (weather_var == 'temp') {
  dummy_data = select(dummy_data, hvac.split, hvac.vrf, envelope, lights,
                      shgc, afn.max, afn.min, azimuth, boundaries.adiabatic,
                      boundaries.outdoors, tbs, tbu, targ)
  test_data_extra = select(dummy_data_extra, hvac.split, hvac.vrf, envelope, lights,
                           shgc, afn.max, afn.min, azimuth, boundaries.adiabatic,
                           boundaries.outdoors, tbs, tbu, targ)
  pp_vars = c('envelope', 'lights', 'shgc', 'azimuth', 'tbs', 'tbu')
} else {
  stop('Invalid weather variable!')
}
# split data into train and test sets
data = vector('list', 2)
data = lapply(list('train' = TRUE, 'test' = FALSE), SplitData,
              dummy_data, 0.8)
# pre-process data
pp_model = preProcess(data$train[, pp_vars], method = c('center', 'scale'))
data = lapply(data, PPData, pp_model)
test_data_extra = PPData(dummy_data_extra, pp_model)

# keras model ####
# assign variables
feat = as.matrix(data$train[, 1:11])
targ = data$train$targ
# create sequential model
model = keras_model_sequential() %>%
  layer_dense(units = 8, activation = 'tanh', input_shape = 11) %>%
  layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = 1)
# compile model
model %>%
  compile(loss = 'mse', optimizer = 'rmsprop', metrics = list('mae'))
# fit model
# train the model
history = model %>%
  fit(feat,
      targ,
      epochs = 500,
      validation_split = 0.3,
      verbose = TRUE)

