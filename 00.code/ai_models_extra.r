# load libraries
LoadLib = function(pkgs) lapply(pkgs, library, character.only = T)
LoadLib(c('caret', 'dplyr', 'hydroGOF', 'Metrics'))

# load extra data
LoadData = function(input_path) {
  data = read.csv(input_path)
  colnames(data)[ncol(data)] = 'targ'
  
  return(data)
}
raw_data = LoadData('/home/rodox/00.git/02.commercial_model/02.source/data_bank.csv')
raw_data_extra = LoadData('/home/rodox/00.git/02.commercial_model/02.source/data_bank_extra.csv')

# create dummy variables
CreateDummies = function(data, train_prop) {
  dummy_model = dummyVars(targ ~ ., data = data)
  dummy_data = data.frame(predict(dummy_model, newdata = data))
  dummy_data$targ = data$targ
  
  return(dummy_data)
}
dummy_data = CreateDummies(raw_data, 0.8)

dummy_data_extra = raw_data_extra
dummy_data_extra$hvac.split = ifelse(dummy_data_extra$hvac == 'split', 1, 0)
dummy_data_extra$hvac.vrf = ifelse(dummy_data_extra$hvac == 'vrf', 1, 0)
dummy_data_extra$boundaries.adiabatic = ifelse(dummy_data_extra$boundaries == 'adiabatic', 1, 0)
dummy_data_extra$boundaries.outdoors = ifelse(dummy_data_extra$boundaries == 'outdoors', 1, 0)
dummy_data_extra$afn.max = ifelse(dummy_data_extra$afn == 'com', 1, 0)
dummy_data_extra$afn.min = ifelse(dummy_data_extra$afn == 'sem', 1, 0)
test_data_extra = select(dummy_data_extra, ghr, hvac.split, hvac.vrf, lights,
                         boundaries.adiabatic, boundaries.outdoors, envelope,
                         shgc, azimuth, afn.max, afn.min, targ)

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
data = vector('list', 2)
data = lapply(list('train' = TRUE, 'test' = FALSE), SplitData,
              dummy_data, 0.8)

pp_model = preProcess(data$train[, c(4, 7:9)], method = c('center', 'scale'))

PPData = function(data, pp_model) {
    data = predict(pp_model, data)
  
  return(data)
}
data = lapply(data, PPData, pp_model)
test_data_extra = PPData(test_data_extra, pp_model)

# load model
load('/home/rodox/00.git/02.commercial_model/00.codes/1st_models.RData')

# test
TestModel = function(model, test_data) {
  targ = predict(model, newdata = test_data)
  test = list('mae' = mae(test_data$targ, targ),
              'rmse' = rmse(test_data$targ, targ),
              'nrmse' = nrmse(as.numeric(targ), test_data$targ, norm = 'maxmin'))
  
  return(test)
}
tests = lapply(models, TestModel, data$test)
tests_extra = lapply(models, TestModel, test_data_extra)