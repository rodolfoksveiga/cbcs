file_names = dir('~/Desktop/test/output/', pattern = '.csv', full.names = T)

dfs = vector('list', length(file_names))
dfs = lapply(file_names, read.csv)
names(dfs) = file_names

# curitiba
apply(dfs$'/home/rodox/Desktop/test/output//Curitiba_test1.csv'[, 2:25], 1, sum)/(3600000*600)
apply(dfs$'/home/rodox/Desktop/test/output//Curitiba_test2.csv'[, 2:25], 1, sum)/(3600000*600)
apply(dfs$'/home/rodox/Desktop/test/output//Curitiba_test3.csv'[, 2:32], 1, sum)/(3600000*600)
apply(dfs$'/home/rodox/Desktop/test/output//Curitiba_test4.csv'[, 2:32], 1, sum)/(3600000*600)

# maceio
apply(dfs$'/home/rodox/Desktop/test/output//Maceio_test1.csv'[, 2:25], 1, sum)/(3600000*600)
apply(dfs$'/home/rodox/Desktop/test/output//Maceio_test2.csv'[, 2:25], 1, sum)/(3600000*600)
apply(dfs$'/home/rodox/Desktop/test/output//Maceio_test3.csv'[, 2:32], 1, sum)/(3600000*600)
apply(dfs$'/home/rodox/Desktop/test/output//Maceio_test4.csv'[, 2:32], 1, sum)/(3600000*600)