# base functions ####
# sum outputs according to a pattern
SumOutputs = function(pattern, df) sum(df[, str_detect(colnames(df), pattern)])
# apply SumOutputs()
ApplySumOutputs = function(df, patterns) as.data.frame(t(sapply(patterns, SumOutputs, df)))

# main function ####
CalcTargets = function(sample, input_dir, unit = 'kwh') {
  patterns = c(targ_lights = 'Lights', targ_fan = 'Fan',
               targ_heating = '(Heating|Heater)', targ_cooling = '(Cooling|Defrost)',
               targ_appliances = '(?<!ATM_EQUIP|SERVIDOR)\\.Electric\\.Equipment',
               targ_atm = 'ATM_EQUIP\\.Electric\\.Equipment',
               targ_server = 'SERVIDOR\\.Electric\\.Equipment')
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
  sample[, cols] = round(sample[, cols]/(div*sample$area), 1)
  return(sample)
}
