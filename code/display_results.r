invisible({
  pkgs = c('ggcorrplot', 'caret', 'dplyr', 'forcats', 'ggthemr', 'ggplot2',
           'gridExtra', 'mgcv', 'moments', 'jsonlite', 'purrr', 'stringr',
           'tidyr', 'reshape2', 'RColorBrewer', 'RJSONIO')
  lapply(pkgs, library, character.only = TRUE)
})

# plot functions ####
# plot eui x area
PlotAreaEUI = function(data_path, output_dir) {
  df = data_path %>%
    read.csv() %>%
    mutate(consumo_area = consumo/area)
  gam = gam(consumo_area ~ area, data = df)
  rsquared = 1 - sum(gam$residuals^2)/sum((gam$y - mean(gam$y))^2)
  rsquared = round(rsquared, 4)
  ggthemr('pale', layout = 'scientific')
  plot = ggplot(df, aes(area, consumo_area)) +
    geom_point(size = 0.2, colour = 'black') +
    geom_smooth(method = 'gam', se = FALSE,
                aes(colour = 'Generalized Additive Model')) +
    geom_text(x = 2500, y = 750, label = paste('R² =', rsquared), size = 7) +
    labs(x = 'Area (m²)', y = 'EUI (kWh/m².year)') +
    scale_color_manual(name = 'Legend:', values = c('Generalized Additive Model' = 'red')) +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 16, face = 'bold'),
          legend.text = element_text(size = 15),
          legend.position = 'bottom')
  WritePlot(plot, 'area_eui', output_dir)
  ggthemr_reset()
}
# plot weather correlation using linear models
PlotWeatherVar = function(dbt_path, cdh_path, output_dir) {
  dbt = read.csv(dbt_path)
  rdbt = round(summary(lm(targ ~ dbt, dbt))$r.squared, 3)
  cdh = read.csv(cdh_path)
  rcdh = round(summary(lm(targ ~ cdh, cdh))$r.squared, 3)
  plot1 = ggplot(dbt, aes(y = targ, x = dbt)) +
    geom_point(size = 0.2, colour = 'black', alpha = 0.25) +
    geom_smooth(method = 'lm', se = FALSE, colour = 'red') +
    geom_text(aes(x = 18.5, y = 275), label = paste('R² =', rdbt), size = 7) +
    labs(x = 'mDBT', y = 'EUI (kWh/m².year)') +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text = element_text(size = 14))
  plot2 = ggplot(cdh, aes(y = targ, x = cdh)) +
    geom_point(colour = 'black', size = 0.2, alpha = 0.25) +
    geom_smooth(method = 'lm', se = FALSE, colour = 'red') +
    geom_text(aes(x = 19000, y = 300), label = paste('R² =', rcdh), size = 7) +
    labs(x = 'CDH', y = 'EUI (kWh/m².year)') +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14))
  ggthemr('pale', layout = 'scientific')
  plot = grid.arrange(plot1, plot2, ncol = 2)
  WritePlot(plot, 'weather_var_imp', output_dir)
  ggthemr_reset()
}
# plot target distribution
PlotTargDist = function(sample_path, output_dir) {
  sample_path = './result/saltelli_sample_cdh.csv'
  output_dir = './plot_table/'
  
  sample = read.csv(sample_path)
  kurt = round(kurtosis(sample$targ), 3)
  skew = round(skewness(sample$targ), 3)
  ggthemr('pale', layout = 'scientific')
  plot = ggplot(sample) +
    geom_density(aes(x = targ), colour = 'black', fill = 'blue', alpha = 0.1) +
    geom_text(aes(x = 250, y = 0.0105), label = paste('Skewness =', skew), size = 7) +
    geom_text(aes(x = 250, y = 0.0095), label = paste('Kurtosis =', kurt), size = 7) +
    labs(x = 'EUI (kWh/m².year)', y = 'Relative Probability') +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text = element_text(size = 14))
  WritePlot(plot, 'targ_dist', output_dir)
  ggthemr_reset()
}
# plot sobol effects
PlotSA = function(result_path, problem_path, output_dir) {
  vars = problem_path %>%
    read_json() %>%
    pluck('names') %>%
    unlist()
  ggthemr('pale', layout = 'scientific')
  plot = result_path %>%
    fromJSON() %>%
    keep(names(.) %in% c('S1', 'ST')) %>%
    as.data.frame() %>%
    mutate(Variable = as.factor(vars)) %>%
    arrange(ST) %>%
    mutate(Variable = fct_inorder(Variable),
           S1 = ifelse(S1 < 0, 0, S1)) %>%
    melt() %>%
    ggplot() +
    geom_bar(aes(x = Variable, y = value, fill = variable),
             stat = 'identity', position = 'dodge', colour = 'black') +
    geom_text(aes(x = Variable, y = value, label = round(value, 3)),
              position = position_dodge2(width = 1), hjust = -0.15) +
    geom_hline(yintercept = c(0.01, 0.025, 0.05), linetype = 'dashed', colour = 'black') +
    labs(x = 'Input variable', y = 'Sensitivity index value (adim.)') +
    coord_flip() +
    scale_fill_manual(name = 'Legend:', values = brewer.pal(2, 'Paired'),
                      labels = c('1st order', 'Total effect')) +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14, angle = 30),
          legend.title = element_text(size = 15, face = 'bold'),
          legend.text = element_text(size = 14),
          legend.position = 'bottom')
  WritePlot(plot, 'sobol_barplot', output_dir)
  ggthemr_reset()
}
# plot energy consumption by end use
PlotEUIEndUse = function(sample_path, output_dir) {
  cols = c('Lights', 'Fans', 'HVAC Heating', 'HVAC Cooling', 'Appliances', 'ATMs', 'Server')
  ggthemr('pale', layout = 'scientific')
  plot = sample_path %>%
    read.csv() %>%
    select(starts_with('targ_')) %>%
    `colnames<-`(cols) %>%
    gather() %>%
    ggplot() +
    geom_boxplot(aes(x = key, y = value, fill = key),
                 outlier.size = 0.3, show.legend = FALSE) +
    coord_flip() +
    labs(x = 'Energy end use', y = 'EUI (kWh/m².year)') +
    scale_fill_brewer(palette = 'Paired') +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14, angle = 30),
          plot.margin = margin(5.5, 20, 5.5, 5.5))
  WritePlot(plot, 'eui_end_use', output_dir)
  ggthemr_reset()
}
# plot input correlation agains the energy consumption
PlotEndUseCor = function(sample_path, output_dir) {
  ggthemr('pale', layout = 'scientific')
  plot = sample_path %>%
    read.csv() %>%
    mutate_if(is.character, function(x) as.numeric(as.factor(x))) %>%
    rename(Fans = targ_fan, 'HVAC Heating' = targ_heating, 'HVAC Cooling' = targ_cooling) %>%
    select_if(!str_detect(colnames(.), 'targ')) %>%
    cor() %>%
    as.data.frame() %>%
    select(-14:-12) %>%
    ggcorrplot(method = 'square', type = 'lower', lab = TRUE, legend.title = 'Legend:') +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text = element_text(size = 14),
          axis.line = element_line(colour = 'lightgrey'),
          axis.ticks = element_line(colour = 'lightgrey'),
          legend.title = element_text(size = 15, face = 'bold', hjust = 0.5),
          legend.text = element_text(size = 14, hjust = 1))
  WritePlot(plot, 'corr_end_use', output_dir)
  ggthemr_reset()
}
# plot machine learning performance
PlotMLPerf = function(test_path, model_path, output_dir) {
  test = read.csv(test_path)
  model = readRDS(model_path)
  pred = predict(model, newdata = test)
  df = data.frame(pred, sim = test$targ, cdh = test$cdh)
  df$cdh = cut(df$cdh, 3, labels = c('1870 ~ 29500', '29500 ~ 57100', '57100 ~ 84800'))
  ggthemr('pale', layout = 'scientific')
  plot1 = ggplot(df, aes(x = sim, y = pred)) +
    geom_point(aes(colour = cdh), size = 0.2, alpha = 0.5) +
    geom_abline(slope = 1, colour = 'black', size = 0.5) +
    labs(x = 'Simulated Energy Consumption (kWh/m².year)',
         y = 'Predicted Energy Consumption (kWh/m².year)') +
    scale_colour_brewer(palette = 'Dark2') +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text = element_text(size = 14),
          axis.line = element_line(colour = 'lightgrey'),
          axis.ticks = element_line(colour = 'lightgrey'),
          legend.title = element_text(size = 15, face = 'bold', hjust = 0.5),
          legend.text = element_text(size = 14, hjust = 1),
          legend.position = 'bottom')
  plot2 = ggplot(df) +
    geom_density(aes(x = pred - sim, colour = cdh), alpha = 0.1) +
    labs(x = 'Error (kWh/m².year)', y = 'Relative Probability', colour = 'CDH (°): ') +
    scale_colour_brewer(palette = 'Dark2') +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text = element_text(size = 14),
          axis.line = element_line(colour = 'lightgrey'),
          axis.ticks = element_line(colour = 'lightgrey'),
          legend.title = element_text(size = 15, face = 'bold', hjust = 0.5),
          legend.text = element_text(size = 14, hjust = 1),
          legend.position = 'bottom')
  plot = grid.arrange(plot1, plot2, ncol = 2)
  WritePlot(plot, 'model_perf', output_dir)
  ggthemr_reset()
}
# plot validation
PlotStudyPerf = function(study_path, dummy_path, model_path, output_dir) {
  data = study_path %>%
    read.csv() %>%
    cbind(source = 'Real') %>%
    mutate(case = str_to_upper(case))
  dummy_data = dummy_path %>%
    readRDS() %>%
    predict(newdata = data[, 1:12])
  predictions = model_path %>%
    readRDS() %>%
    predict(newdata = dummy_data) %>%
    as.data.frame() %>%
    cbind(case = data$case, source = 'Prediction') %>%
    rename(targ = '.')
  authors = 'Borgstein and Lamberts (2014)'
  borgstein = data.frame(
    targ = sapply(data$cdh, function(x) 136.5 + 0.001984*x),
    case = c('A', 'B', 'C'),
    source = authors
  )
  data = data %>%
    select(c('targ', 'case', 'source')) %>%
    rbind(predictions, borgstein) %>%
    mutate(source = factor(source, levels = c('Real', 'Prediction', authors)))
  ggthemr('pale', layout = 'scientific')
  plot = ggplot(data, aes(x = case, y = targ, fill = source)) +
    geom_bar(stat = 'identity', position = 'dodge', colour = 'black') +
    labs(x = 'Case', y = 'EUI (kWh/m².year)', fill = 'Legend:') +
    geom_text(aes(label = round(targ, 1)), size = 4, vjust = -0.5,
              position = position_dodge(width = 0.9)) +
    scale_fill_brewer(palette = 'Dark2') +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          legend.title = element_text(size = 15, face = 'bold'),
          legend.text = element_text(size = 14),
          legend.position = 'bottom')
  WritePlot(plot, 'study_perf', output_dir)
  ggthemr_reset()
}

# define characteristics to save the plot
WritePlot = function(plot, plot_name, output_dir) {
  # plot: plot variable 
  # plot_name: file name (without extension)
  # output_dir: output directory
  png(filename = paste0(output_dir, plot_name, '.png'),
      width = 33.8, height = 19, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}

# main function ####
DisplayResults = function(rd_path, sample_path, sdbt_path, sa_path, sp_path, output_dir) {
  PlotAreaEUI(rd_path, output_dir)
  PlotWeatherVar(sdbt_path, sample_path, output_dir)
  PlotEUIEndUse(sample_path, output_dir)
  PlotEndUseCor(sample_path, output_dir)
  PlotSA(sa_path, sp_path, output_dir)
  PlotTargDist(sample_path, output_dir)
  PlotMLPerf('./result/test_data.csv', './result/model.rds', './plot_table/')
  PlotStudyPerf('./source/study_data.csv', './result/dummy_nvar9.rds',
                './result/model.rds', './plot_table/')
}
