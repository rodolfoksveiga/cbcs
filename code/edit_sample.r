# load library
library(dplyr)

# base functions ####
# turn sample sample variables into discrete
DiscVar = function(var, val1, val2) ifelse(var <= 1, val1, val2)

# fix sample variables
FixSample = function(sample,
                     s1 = '/home/rodox/00.git/02.commercial_model/01.seed/seed_split_bank.epJSON',
                     s2 = '/home/rodox/00.git/02.commercial_model/01.seed/seed_vrf_bank.epJSON') {
  sample[c('hvac', 'envelope', 'afn', 'boundaries')] =
    mapply(DiscVar, sample[c('hvac', 'envelope', 'afn', 'boundaries')],
           list('split', 2.5, 'min', 'adiabatic'), list('vrf', 3.7, 'max', 'outdoors'))
  sample$atm = ceiling(sample$atm)
  sample = unique(sample)
  sample$seed = ifelse(sample$hvac == 'split', s1, s2)
  sample$case = LabelCase(sample)
  sample = select(sample, case, seed, everything())
  return(sample)
}

# label case
LabelCase = function(sample) {
  label = paste0('hvac_', sample$hvac, '_env_', sample$envelope, '_light_',
                 round(sample$lights, 2), '_shgc_', round(sample$shgc, 2),
                 '_afn_', sample$afn, '_atm_', sample$atm, '_azi_',
                 round(sample$azimuth, 2), '_bounds_', sample$boundaries)
  return(label)
}

# load sample
LoadSample = function(path) {
  sample = read.csv(path)[-1]
  return(sample)
}

# main function ####
EditSample = function(path) {
  path %>%
    LoadSample %>%
    FixSample %>%
    write.csv(file = path, row.names = F)
}

# application ####
EditSample('/home/rodox/00.git/02.commercial_model/00.code/sample.csv')