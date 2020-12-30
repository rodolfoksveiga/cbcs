library(caret)
library(dplyr)

data = read.csv('./source/study_data.csv')
data = data[, 1:12]

dummy_model = readRDS('./result/dummy_nvar9.rds')
dummy_data = predict(dummy_model, newdata = data)

model = readRDS('./result/model.rds')
predictions = predict(model, newdata = dummy_data)
