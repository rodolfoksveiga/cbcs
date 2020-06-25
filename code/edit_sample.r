# load library
pkgs = c('caret', 'dplyr', 'stringr')
library(dplyr)

# base functions ####
# associate real values to the variables
AddVars = function(sample, area_path, inmet_path, var, prop) {
  dfs_list = lapply(c(area_path, inmet_path), read.csv, stringsAsFactors = FALSE)
  dfs_list = mapply(DefVals, dfs_list, c('area', 'cdh'), c(0.9, 0.7))
  sample$area = dfs_list[[1]]$area[sample$area]
  sample$epw = dfs_list[[2]]$arquivo_climatico[sample$cdh]
  sample$cdh = dfs_list[[2]]$cdh[sample$cdh]
  sample = unique(sample)
  sample = select(sample, case, seed, epw, everything())
  return(sample)
}
# define values for the variables
DefVals = function(df, var, prop) {
  set.seed(100)
  index = createDataPartition(df[[var]], p = prop, list = FALSE)
  values = df[-index, ]
  return(values)
}
# turn sample variables into discrete
DoubleVar = function(var, val1, val2) ifelse(var == 0, val1, val2)
# fix sample variables
FixSample = function(sample, split_path, vrf_path) {
  disc_vars = c('afn', 'area', 'atm', 'boundaries', 'cdh', 'envelope', 'hvac')
  sample[disc_vars] = ceiling(sample[disc_vars])
  sample[c('hvac', 'envelope', 'afn', 'boundaries')] =
    mapply(DoubleVar, sample[c('hvac', 'envelope', 'afn', 'boundaries')],
           list('split', 'light', 'min', 'adiabatic'), list('vrf', 'heavy', 'max', 'outdoors'))
  cont_vars = c('azimuth', 'lights', 'shgc')
  sample[cont_vars] = round(sample[cont_vars], 2)
  sample$azimuth = ifelse(sample$azimuth < 0, 0, sample$azimuth)
  sample = unique(sample)
  sample$seed = ifelse(sample$hvac == 'split', split_path, vrf_path)
  sample$case = LabelCase(sample)
  return(sample)
}
# label case
LabelCase = function(sample) {
  label = paste0('afn_', sample$afn, '_area_', sample$area, '_atm_', sample$atm,
                 '_azi_', sample$azi,  '_bounds_', sample$boundaries, '_cdh_',
                 sample$cdh, '_env_', sample$env, '_hvac_', sample$hvac, '_lights_',
                 sample$lights, '_shgc_', sample$shgc)
  return(label)
}
# load sample
LoadSample = function(path) sample = read.csv(path)[-1]

# main function ####
EditSample = function(sample_path, split_path,
                      vrf_path, area_path, inmet_path) {
  sample_path %>%
    LoadSample %>%
    FixSample(split_path, vrf_path) %>%
    AddVars(area_path, inmet_path) %>%
    write.csv(file = sample_path, row.names = FALSE)
}

# application ####
samples_paths = paste0('~/git/commercial_model/code/', c('train', 'test'), '_sample.csv')
lapply(samples_paths, EditSample,
       '~/git/commercial_model/seed/seed_split_bank.epJSON',
       '~/git/commercial_model/seed/seed_vrf_bank.epJSON',
       '~/git/commercial_model/source/real_data.csv',
       '~/git/commercial_model/source/inmet_list.csv')
