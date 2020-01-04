setwd("~/Model Selection")

source('runSim.R')

data('mtcars')
mtcars
results = runSim(mtcars, 'regression', 'mpg', 'MAE')
dat = mtcars
problem.type = 'regression'
target = 'mpg'



data('iris')
iris
iris$flag = ifelse(iris$Species == 'versicolor', 1, 0)
iris$flag = factor(iris$flag)
results = runSim(iris, 'classification', 'flag', 'Accuracy')



data('diamonds')
diamonds
results = runSim(diamonds, 'regression', 'carat', 'RMSE')
dat = diamonds
problem.type = 'regression'
target = 'carat'
