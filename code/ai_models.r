# load libraries ####
LoadLib = function(pkgs) lapply(pkgs, library, character.only = T)
LoadLib(c('caret', 'dplyr', 'hydroGOF', 'Metrics', 'parallel'))

# base functions ####
# rename analysis index r squared
RenameRsq = function(x) sub('Rsquared', 'R²', x)

# surrogate model functions ####
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

# train
FitModel = function(train_tech, samp_tech, num_samp, repeats,
                    train_data, eval = 'RMSE', seed = 200) {
  # reproduce
  set.seed(seed)
  
  fit_ctrl = trainControl(samp_tech, num_samp, repeats, savePredictions = 'final',
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
PlotComp = function(models_comp, weather_var, sobol, output_dir) {
  plot = bwplot(models_comp, scales = list(x = list(relation = 'free'),
                                           y = list(relation = 'free')))
  SavePlot(plot, paste0('models_comp_', ifelse(sobol, 'sobol', 'extreme'), '_',
                        weather_var), output_dir)
}

# plot training process
PlotFit = function(model, train_tech, weather_var, sobol, output_dir,
                   save_plot = T, lx = 33.8, ly = 19) {
  k = ifelse(nrow(model$modelInfo$parameters) == 1, 1.5, 2)
  
  plot = plot(model, asp = 1/k,
              main = paste0(model$modelInfo$label,
                            '\nOtimização dos Hiperparâmetros'),
              xlab = paste('Valor do Hiperparâmetro -',
                           model$modelInfo$parameters$label[1]),
              ylab = 'RMSE de validação (kWh/ano)')
  
  if (save_plot) {
    SavePlot(plot, paste0(train_tech, '_fit_', ifelse(sobol, 'sobol', 'extreme'),
                          '_', weather_var), output_dir)
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

ProcessModel = function(data_path, data_extra_path, weather_var, sobol, load_models,
                        models_path, save_plots, save_models, op_dir, om_dir) {
  
  data_path = '/home/rodox/git/commercial_model/data/data_sobol.csv'
  data_extra_path = '/home/rodox/git/commercial_model/data/data_extra_rv.csv'
  weather_var = 'ghr'
  sobol = T
  load_models = T
  models_path = '~/Desktop/models/models_sobol_ghr.RData'
  save_plots = T
  save_models = F
  om_dir = '~/Desktop/models/'
  op_dir = '~/Desktop/plots/'
  
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
  # train
  if (load_models) {
    load(models_path)
  } else {
    models = mclapply(list('lm' = 'lm', 'blm' = 'BstLm',
                           'qrf' = 'qrf', 'gbrt' = 'blackboost',
                           'svm' = 'svmRadial', 'brnn' = 'brnn'),
                      FitModel, 'cv', 10, NA, data$train, mc.cores = detectCores())
  }
  
  # plots and results
  # stats comparison between models
  models_comp = CompModels(models)
  if (save_plots) {
    # plot comparison between models
    PlotComp(models_comp, weather_var, sobol, op_dir)
    # plot training process
    mapply(PlotFit, models[-1], names(models[-1]), weather_var, sobol,
           MoreArgs = list(op_dir))
  }
  if (save_models) {
    save(models, file = paste0(om_dir, 'models_', ifelse(sobol, 'sobol', 'extreme'),
                               '_', weather_var, '.RData'))
  } else {
    models_summ = summary(models_comp)
    results = list('models' = models, 'summ' = models_summ,
                     'tests' = lapply(models, TestModel, data$test),
                   'tests_extra' = lapply(models, TestModel, test_data_extra))
    return(results)
  }
}

# application ####
results = ProcessModel(data_path = '/home/rodox/git/commercial_model/data/data_sobol.csv',
             data_extra_path = '/home/rodox/git/commercial_model/data/data_extra_rv.csv',
             weather_var = 'temp', sobol = T, load_models = F, save_plots = T, save_models = F,
             om_dir = '/home/rodox/git/commercial_model/code/',
             op_dir = '/home/rodox/git/commercial_model/plot_table/')

# data_paths = c(rep('/home/rodox/git/commercial_model/data/data_sobol.csv', 2),
#                rep('/home/rodox/git/commercial_model/data/data_extreme.csv', 2))
# data_extra_paths = c(rep('/home/rodox/git/commercial_model/data/data_extra_rv.csv', 2),
#                      rep('/home/rodox/git/commercial_model/data/data_extra_master.csv', 2))
# weather_vars = rep(c('ghr', 'temp'), 2)
# sobols = c(rep(T, 2), rep(F, 2))
# mapply(ProcessModel, data_paths, data_extra_paths, weather_vars, sobols,
#        load_models = F, save_plots = F, save_models = T,
#        om_dir = '~/Desktop/models/', op_dir = '~/Desktop/plots/')