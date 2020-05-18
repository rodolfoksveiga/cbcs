file_names = dir('~/git/commercial_model/output/extra/', pattern = '.csv', full.names = T)

dfs = vector('list', length(file_names))
dfs = lapply(file_names, read.csv)
names(dfs) = basename(file_names)

# curitiba
apply(dfs$'curitiba_test1.csv'[, 2:24], 1, sum)/(3600000*600)
apply(dfs$'curitiba_test2.csv'[, 2:24], 1, sum)/(3600000*600)
apply(dfs$'curitiba_test3.csv'[, 2:31], 1, sum)/(3600000*600)
apply(dfs$'curitiba_test4.csv'[, 2:31], 1, sum)/(3600000*600)

# maceio
apply(dfs$'maceio_test1.csv'[, 2:24], 1, sum)/(3600000*600)
apply(dfs$'maceio_test2.csv'[, 2:24], 1, sum)/(3600000*600)
apply(dfs$'maceio_test3.csv'[, 2:31], 1, sum)/(3600000*600)
apply(dfs$'maceio_test4.csv'[, 2:31], 1, sum)/(3600000*600)
