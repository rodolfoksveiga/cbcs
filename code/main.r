# avoid undesirable outputs on prompt
invisible({
  # load libraries and global environment ####
  pkgs = c('dplyr', 'jsonlite', 'reticulate',
           'parallel', 'purrr', 'stringr', 'tibble')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_model', 'calc_targets', 'run_ep_sim', 'tidy_sample')
  codes = paste0('./code/', codes, '.r')
  lapply(codes, source)
  inmet = read.csv('./source/inmet_list.csv')
  
  # variables ####
  saltelli_path = './result/saltelli_sample.csv'
  seeds_dir = './seed/'
  models_dir = '~/rolante/cbcs/model/'
  epws_dir = '~/rolante/weather/'
  output_dir = '~/rolante/cbcs/output/'
  sample_path = './result/sample.csv'
  cores_left = 0
  
  # main code ####
  # generate sample
  py_run_file('./code/saltelli_sample.py')
  # read and tidy up sample
  sample = TidySample(saltelli_path, seeds_dir, models_dir, epws_dir, inmet)
  # build cases
  mcmapply(BuildModel, sample$seed_path, sample$afn, sample$area, sample$atm, sample$azimuth,
           sample$boundaries, sample$cop, sample$envelope, sample$lights, sample$shgc,
           sample$model_path, mc.cores = detectCores() - cores_left)
  # run simulations
  ProcessEPSims(sample, output_dir, 0, inmet)
  # calculate targets and add them to the sample
  sample = CalcTargets(sample, output_dir)
  # write sample file 
  write.csv(sample, sample_path, row.names = FALSE)
  # join samples
  JoinSamples(saltelli_path, sample_path)
})