# base functions ####
# sum outputs according to a pattern
SumOutputs = function(pattern, df) sum(df[, str_detect(colnames(df), pattern)])
# apply SumOutputs()
ApplySumOutputs = function(df, patterns) as.data.frame(t(sapply(patterns, SumOutputs, df)))

# main function ####
CalcTargets = function(sample, input_dir, unit = 'kwh') {
  patterns = c(lights = 'Lights', fan = 'Fan',
               heating = '(Heating|Heater)', cooling = '(Cooling|Defrost)',
               appliances = '(?<!ATM_EQUIP|SERVIDOR)\\.Electric\\.Equipment',
               atm = 'ATM_EQUIP\\.Electric\\.Equipment',
               server = 'SERVIDOR\\.Electric\\.Equipment')
  div = ifelse(unit == 'kwh', 3600000, 1000)
  dfs = input_dir %>%
    dir('\\.csv', full.names = TRUE) %>%
    lapply(read.csv) %>%
    lapply(select, -1)
  sample$targ = dfs %>%
    sapply(apply, 1, sum)
  outputs = dfs %>%
    lapply(ApplySumOutputs, patterns) %>%
    bind_rows()
  sample = sample %>%
    select(-1:-4) %>%
    cbind(outputs)
  cols = (ncol(sample) - 7):ncol(sample)
  sample[, cols] = sample[, cols]/(div*sample$area)
  return(sample)
}
