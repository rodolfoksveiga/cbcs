# load libraries ####
LoadLib = function(pkgs) lapply(pkgs, library, character.only = T)
LoadLib(c('caret', 'hydroGOF', 'keras', 'kernlab', 'Metrics', 'parallel'))

# base functions ####
# load data
LoadData = function(input_path) {
  data = read.csv(input_path)
  colnames(data)[ncol(data)] = 'targ'
  
  return(data)
}

# data inspection
InspData = function(data) apply(data[, 1:ncol(data) - 1], 2, unique)

# rename analysis index r squared
RenameRsq = function(x) sub('Rsquared', 'R²', x)

# surrogate functions ####
# create dummy variables
CreateDummies = function(data, train_prop) {
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

# train
FitModel = function(train_tech, samp_tech, num_samp, repeats,
                    train_data, eval = 'RMSE', seed = 200) {
  # reproduce
  set.seed(seed)
  
  fit_ctrl = trainControl(samp_tech, num_samp, repeats, savePredictions = 'all',
                          returnResamp = 'final', verboseIter = TRUE)
  fit = train(targ ~ ., train_data, trControl = fit_ctrl,
              tuneLength = 10, method = train_tech, metric = eval)
  
  return(fit)
}

# test
TestModel = function(model, test_data) {
  targ = predict(model, newdata = test_data)
  test = list('mae' = mae(test_data$targ, targ),
              'rmse' = rmse(test_data$targ, targ),
              'nrmse' = nrmse(as.numeric(targ), test_data$targ, norm = 'maxmin'))
  
  return(test)
}

# stats and plot functions ####
# stats comparison between models
CompModels = function(models) {
  comp = resamples(models)
  comp$metrics[3] = 'R²'
  names(comp$values)[-1] = names(comp$values)[-1] %>%
    RenameRsq() %>%
    toupper()
  comp$models = toupper(comp$models)
  
  return(comp)
}

# plot comparison between models
PlotComp = function(models_comp, output_dir) {
  plot = bwplot(models_comp, scales = list(x = list(relation = 'free'),
                                           y = list(relation = 'free')))
  SavePlot(plot, 'models_comp', output_dir)
}

# plot training process
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

# save plot
SavePlot = function(plot, plot_name, output_dir, lx = 33.8, ly = 19) {
  png(filename = paste0(output_dir, plot_name, '.png'),
      width = lx, height = ly, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}

# main code ####
# load data
raw_data =
  LoadData('/home/rodox/00.git/02.commercial_model/04.source/data_bank.csv')
# data inspection
InspData(raw_data)
# create dummy variables
dummy_data = CreateDummies(raw_data, 0.8)
# split data into train and test sets
data = vector('list', 2)
data = lapply(list('train' = TRUE, 'test' = FALSE), SplitData,
              dummy_data, 0.8)
# pre-process data
pp_model = preProcess(data$train[, c(4, 7:9)], method = c('center', 'scale'))
data = lapply(data, PPData, pp_model)
# train
models = mclapply(list('lm' = 'lm', 'blm' = 'BstLm',
                       'qrf' = 'qrf', 'gbrt' = 'blackboost',
                       'svm' = 'svmRadial', 'brnn' = 'brnn'),
                  FitModel, 'cv', 10, NA, data$train, mc.cores = detectCores())
# test
tests = lapply(models, TestModel, data$test)
# stats comparison between models
models_comp = CompModels(models)
models_summ = summary(models_comp)
# plot comparison between models
PlotComp(models_comp, output_dir = '~/00.git/02.commercial_model/03.plot/')
# plot training process
mapply(PlotFit, models[-1], paste0(names(models[-1]), '_fit'),
       MoreArgs = list('~/00.git/02.commercial_model/03.plot/'))
