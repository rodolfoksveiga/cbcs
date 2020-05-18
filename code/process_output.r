# load libraries ####
library(dplyr)

# base functions ####
# load data
LoadData = function(x) read.csv(x)[-1]
# add weather variables (cdd and hdd) to the sample
AddWeather = function(sample,
                      cdd = c(3495, 14172, 23954, 31412,
                              45016, 54061, 63550, 71394),
                      hdd = c(1030, 288.3, 361.9,
                              49.6, 5.6, 0, 0, 0)) {
  weather = list('cdd' = cdd, 'hdd' = hdd) %>%
    sapply(function(x) rep(x, each = nrow(sample)))
  sample = sample %>%
    arrange(case) %>%
    select(-one_of('case', 'seed')) %>%
    cbind(weather)
  return(sample)
}

# main function ####
ProcessOutput = function(input_dir, sample_path, output_dir,
                         area = 600, unit = 'kwh') {
  div = ifelse(unit == 'kwh', 3600000, 1000)
  files_name = dir(input_dir, '.csv', full.names = T)
  dfs_list = lapply(files_name, LoadData)
  outputs = sapply(dfs_list, apply, 1, sum)
  outputs = outputs/(div*area)
  sample_path %>%
    read.csv() %>%
    AddWeather() %>%
    cbind(outputs) %>%
    write.csv(paste0(output_dir, 'data_sobol.csv'), row.names = F)
}

# application ####
ProcessOutput('/home/rodox/git/commercial_model/output/',
              '/home/rodox/git/commercial_model/code/sample.csv',
              '/home/rodox/git/commercial_model/data/')
