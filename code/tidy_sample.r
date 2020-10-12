# main function ####
# join samples
JoinSamples = function(saltelli_path, sample_path) {
  quals = c('hvac', 'afn', 'boundaries', 'envelope')
  hvac = c('split', 'vrf')
  afn = c('smart', 'min', 'inter', 'max')
  boundaries = c('adiabatic', 'outdoors')
  envelope = c('heavy', 'light')
  sample = read.csv(sample_path)
  epw = sapply(sample$cdh, function(x, y) which.min(abs(x - y)), inmet$cdh)
  sample$cdh = inmet$cdh[epw]
  sample[, quals] = mapply(function(x, y) match(x, y), sample[, quals],
                           list(hvac, afn, boundaries, envelope))
  rm_cols = (ncol(sample) - 7):ncol(sample)
  cols = colnames(sample)[-rm_cols]
  saltelli_path %>%
    read.csv() %>%
    left_join(select(sample, -rm_cols[-1]), by = cols) %>%
    write.csv(saltelli_path, row.names = FALSE)
}
# tidy sample
TidySample = function(sample_path, seeds_dir, models_dir, epws_dir, inmet) {
  quals = c('hvac', 'afn', 'boundaries', 'envelope')
  hvac = c('split', 'vrf')
  afn = c('smart', 'min', 'inter', 'max')
  boundaries = c('adiabatic', 'outdoors')
  envelope = c('heavy', 'light')
  sample = read.csv(sample_path)
  sample[, quals] = floor(sample[, quals])
  epw = sapply(sample$cdh, function(x, y) which.min(abs(x - y)), inmet$cdh)
  sample$cdh = inmet$cdh[epw]
  write.csv(sample, sample_path, row.names = FALSE)
  sample[, quals] = mapply(function(x, y) y[x], sample[, quals],
                           list(hvac, afn, boundaries, envelope))
  seed_paths = paste0(seeds_dir, 'seed_', sample$hvac, '.json')
  epw_paths = paste0(epws_dir, inmet$arquivo_climatico[epw], '.epw')
  sample = add_column(sample, seed_path = seed_paths, epw_path = epw_paths, .before = 1)
  sample = unique(sample)
  cases = str_pad(1:nrow(sample), 5, 'left', 0)
  sample = add_column(sample, prefix = paste0('case', cases), .before = 2)
  model_paths = paste0(models_dir, sample$prefix, '.epJSON')
  sample = add_column(sample, model_path = model_paths, .before = 2)
  return(sample)
}