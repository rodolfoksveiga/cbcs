# load libraries ####
library(dplyr)

# base functions ####
LoadData = function(x) read.csv(x)[-1]

AddWeather = function(sample, ghr = c(3495, 14172, 23954, 31412,
                                      45016, 54061, 63550, 71394)) {
  ghr = rep(ghr, each = nrow(sample))
  sample = sample %>%
    arrange(case) %>%
    select(-one_of('case', 'seed')) %>%
    cbind(ghr)
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
    write.csv(paste0(output_dir, 'data.csv'), row.names = F)
}

# application ####
ProcessOutput('/home/rodox/00.git/02.commercial_model/04.output/',
              '/home/rodox/00.git/02.commercial_model/00.code/sample.csv',
              '/home/rodox/00.git/02.commercial_model/00.code/')
