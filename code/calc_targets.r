# main function ####
CalcTargets = function(sample, input_dir, unit = 'kwh') {
  div = ifelse(unit == 'kwh', 3600000, 1000)
  sample$targ = input_dir %>%
    dir('\\.csv', full.names = TRUE) %>%
    lapply(read.csv, stringsAsFactors = FALSE) %>%
    lapply(select, -1) %>%
    sapply(apply, 1, sum)
  sample$targ = sample$targ/(div*sample$area)
  sample = select(sample, -1:-4)
  return(sample)
}