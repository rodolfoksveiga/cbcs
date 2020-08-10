# load libraries ####
pkgs = c('caret', 'ggplot2', 'stringr')
lapply(pkgs, library, character.only = TRUE)

# plot function ####
PlotHists = function(df, sample, var, index) {
  par(mfrow = c(1, 2))
  title = ifelse(var == 'area', 'Area (m²)', 'CDH (°C.h)')
  hist(df[[var]], xlab = title, ylab = 'Frequency', main = NULL)
  hist(sample[[var]], xlab = title, ylab = 'Frequency', main = NULL)
}

# main function ####
DefVars = function(df, var, prop) {
  set.seed(100)
  index = createDataPartition(df[[var]], p = prop, list = FALSE)
  sample = df[-index, ]
  PlotHists(df, sample, var, index)
  return(sample)
}

# application ####
data_paths = c('./source/real_data.csv', './source/inmet_list.csv')
dfs_list = lapply(data_paths, read.csv, stringsAsFactors = FALSE)
samples = mapply(DefVars, dfs_list, c('area', 'cdh'), 0.9)
