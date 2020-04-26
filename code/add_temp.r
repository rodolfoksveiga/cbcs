# main function ####
AddTemp = function(data_path) {
  data = read.csv(data_path)
  data$tbs = ifelse(data$ghr == 3495, 15.31,
                            ifelse(data$ghr == 14172, 19.57,
                                   ifelse(data$ghr == 23954, 20.03,
                                          ifelse(data$ghr == 31412, 22.04,
                                                 ifelse(data$ghr == 45016, 23.15,
                                                        ifelse(data$ghr == 54061, 24.40,
                                                               ifelse(data$ghr == 63550, 25.75, 26.65)))))))
  data$tbu = ifelse(data$ghr == 3495, 11.66,
                            ifelse(data$ghr == 14172, 13.85,
                                   ifelse(data$ghr == 23954, 14.93,
                                          ifelse(data$ghr == 31412, 16.51,
                                                 ifelse(data$ghr == 45016, 18.69,
                                                        ifelse(data$ghr == 54061, 19.80,
                                                               ifelse(data$ghr == 63550, 20.88, 21.83)))))))
  data = dplyr::select(data, hvac, envelope, lights, shgc, afn,
                       azimuth, boundaries, ghr, tbs, tbu, targ)
  write.csv(data, file = data_path, row.names = F)
}

# application ####
paths = c('/home/rodox/00.git/02.commercial_model/data/data_extreme.csv',
          '/home/rodox/00.git/02.commercial_model/data/data_sobol.csv')
lapply(paths, AddTemp)