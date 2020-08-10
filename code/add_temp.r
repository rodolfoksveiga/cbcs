# main function ####
AddTemp = function(data_path) {
  data = read.csv(data_path)
  tbs = c('3495' = 15.31, '14172' = 19.57, '23954' = 20.03, '31412' = 22.04,
          '45016' = 23.15, '54061' = 24.40, '63550' = 25.75, '71394' = 26.65)
  tbu = c('3495' = 11.66, '14172' = 13.85, '23954' = 14.93, '31412' = 16.51,
          '45016' = 18.69, '54061' = 19.80, '63550' = 20.88, '71394' = 21.83)
  data$tbs = tbs[as.character(data$ghr)]
  data$tbu = tbs[as.character(data$ghr)]
  data = dplyr::select(data, hvac, envelope, lights, shgc, afn,
                       azimuth, boundaries, ghr, tbs, tbu, targ)
  write.csv(data, file = data_path, row.names = F)
}