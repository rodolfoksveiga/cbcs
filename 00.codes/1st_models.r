# base functions()
# load libraries
LoadLib = function(pkgs) lapply(pkgs, library, character.only = T)
LoadLib(c('caret', 'hydroGOF', 'keras', 'kernlab', 'Metrics', 'parallel'))

# load data
LoadData = function(input_path) {
  data = read.csv(input_path)
  colnames(data)[ncol(data)] = 'targ'
  
  return(data)
}
raw_data =
  LoadData('/home/rodox/00.git/02.commercial_model/02.source/agencia_bancaria_model_total.csv')

# data inspection
InspData = function(data) apply(data[, 1:ncol(data) - 1], 2, unique)
InspData(raw_data)

# create dummy variables
CreateDummies = function(data, train_prop) {
  dummy_model = dummyVars(targ ~ ., data = data)
  dummy_data = data.frame(predict(dummy_model, newdata = data))
  dummy_data$targ = data$targ
  
  return(dummy_data)
}
dummy_data = CreateDummies(raw_data, 0.8)

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
# data$train = raw_train_data = SplitData(dummy_data, 0.8, TRUE)
# data$test = SplitData(dummy_data, 0.8, FALSE)

pp_model = preProcess(data$train[, c(4, 7:9)], method = c('center', 'scale'))

PPData = function(train, train_data, test_data, pp_model) {
  if (train) {
    data = predict(pp_model, train_data)
  } else {
    data = predict(pp_model, test_data)
  }
  
  return(data)
}
data = lapply(c('train' = TRUE, 'test' = FALSE), PPData,
              data$train, data$test, pp_model)

# train
FitModel = function(train_tech, samp_tech, num_samp, repeats,
                    train_data, eval = 'RMSE', seed = 200) {
  # reproduce
  set.seed(seed)

  fit_ctrl = trainControl(samp_tech, num_samp, repeats, savePredictions = 'all',
                          returnResamp = 'final')
  fit = train(targ ~ ., train_data, trControl = fit_ctrl,
              tuneLength = 10, method = train_tech, metric = eval)
  
  return(fit)
}
models = mclapply(list('qrf' = 'qrf', 'gbrt' = 'blackboost',
                         'svm' = 'svmRadial', 'brnn' = 'brnn'),
                    FitModel, 'cv', 10, NA, data$train, mc.cores = detectCores())

# test
TestModel = function(model, test_data) {
  targ = predict(model, newdata = test_data)
  test = list('mae' = mae(test_data$targ, targ),
              'rmse' = rmse(test_data$targ, targ),
              'nrmse' = nrmse(as.numeric(targ), test_data$targ, norm = 'maxmin'))
  
  return(test)
}
tests = lapply(models, TestModel, data$test)

RenameRsq = function(x) sub('Rsquared', 'R²', x)

CompModels = function(models) {
  comp = resamples(models)
  comp$metrics[3] = 'R²'
  names(comp$values)[-1] = names(comp$values)[-1] %>%
    RenameRsq() %>%
    toupper()
  comp$models = toupper(comp$models)

  return(comp)
}
models_comp = CompModels(models)
models_summ = summary(models_comp)

SavePlot = function(plot, plot_name, output_dir, lx = 33.8, ly = 19) {
  png(filename = paste0(output_dir, plot_name, '.png'),
      width = lx, height = ly, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}

PlotComp = function(models_comp, output_dir) {
  plot = bwplot(models_comp, scales = list(x = list(relation = 'free'),
                                           y = list(relation = 'free')))
  SavePlot(plot, 'models_comp', output_dir)
}
PlotComp(models_comp, output_dir = '~/Desktop/')

PlotFit = function(model, plot_name, output_dir,
                   save_plot = T, lx = 33.8, ly = 19) {
  k = ifelse(nrow(model$modelInfo$parameters) == 1, 1.5, 2)
  
  plot = plot(model, asp = 1/k,
              main = paste0(model$modelInfo$label,
                            '\nOtimização dos Hiperparâmetros'),
              xlab = paste('Valor do Hiperparâmetro -',
                            model$modelInfo$parameters$label[1]),
              ylab = 'RMSE de validação (kWh/ano)')
  
  if (save_plot) {
    SavePlot(plot, plot_name, output_dir)
  } else {
    return(plot)
  }
}
mapply(PlotFit, models, paste0(names(models), '_fit'),
       MoreArgs = list('~/Desktop/'))

# base functions ####

# main functions ####

# plot functions ####

# main code ####
