# main function ####
TidySample = function(sample_path, seeds_dir, models_dir, epws_dir, inmet) {
  
  sample_path = './result/sobol_sample.csv'
  seeds_dir = './seed/'
  models_dir = '~/rolante/cbcs/model/'
  epws_dir = '~/rolante/weather/'
  
  sample = read.csv(sample_path, stringsAsFactors = FALSE)
  quals = c('hvac', 'afn', 'boundaries', 'envelope', 'epw')
  sample[, quals] = floor(sample[, quals])
  hvac = c('split', 'vrf')
  afn = c('smart', 'min', 'inter', 'max')
  boundaries = c('adiabatic', 'outdoors')
  envelope = c('heavy', 'light')
  quals = quals[-length(quals)]
  sample[, quals] = mapply(function(x, y) y[x], sample[, quals],
                           list(hvac, afn, boundaries, envelope))
  seed_paths = paste0(seeds_dir, 'seed_', sample$hvac, '.json')
  epw_paths = paste0(epws_dir, inmet$arquivo_climatico[sample$epw], '.epw')
  sample = add_column(sample, seed_path = seed_paths, epw_path = epw_paths, .before = 1)
  sample = unique(sample)
  cases = str_pad(1:nrow(sample), 5, 'left', 0)
  sample = add_column(sample, prefix = paste0('case', cases), .before = 2)
  model_paths = paste0(models_dir, sample$prefix, '.epJSON')
  sample = add_column(sample, model_path = model_paths, .before = 2)
  return(sample)
}