require(caret)
require(tidyverse)
require(gridExtra)

runSim <- function(dat, problem.type, target, error.metric){
  # runs models and gives initial accuracy results
  # 
  # args: dataset, type of problem (regression or classification),
  #    target variable, error metric used to chose the best model
  # 
  # returns: dataframe of algorithm type and error metrics
  # check for required packages
  list.of.packages <- c('caret','tidyverse','brnn','xgboost','neuralnet',
                        'leaps','rpart','fastAdaboost','deepboost','elasticnet',
                        'arm','plyr','mboost','randomGLM','RSNNS','e1071','naivebayes',
                        'LiblineaR','gridExtra')
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,'Package'])]
  if(length(new.packages)){
    install.packages(new.packages)
  }
  
  # omit NAs and move the target variable to the first column
  dat %>% na.omit() %>% dplyr::select(target, everything()) -> dat
  
  # check target column type
  if(problem.type == 'regression' && class(dat[,1]) %in% c('factor', 'character', 'logical')){
    print('Check the target column type', quote = F)
    stop()
  }
  if(problem.type == 'classification' && class(dat[,1]) != 'factor'){
    print('Target column is not factor', quote = F)
    stop()
  }
  
  # create train and test sets
  set.seed(123)
  trainIndex <- createDataPartition(dat[[1]],
                                    p = 0.8,
                                    list = F,
                                    times = 1)
  train = dat[trainIndex,]
  test = dat[-trainIndex,]
  
  # set up training controls
  trainControl <- trainControl(method = 'cv',
                               number = 10)
  
  # specify the models/algorithms for regression and classification
  if(problem.type == 'regression'){
    model.methods = c('brnn','lm','leapBackward','leapForward','leapSeq',
                      'neuralnet','ridge','lasso','bayesglm','glmboost',
                      'rpart','randomGLM','xgbDART','xgbLinear','xgbTree',
                      'glm','mlp','mlpML','svmLinear2','knn')
  }
  if(problem.type == 'classification'){
    model.methods = c('adaboost','deepboost','naive_bayes','svmLinearWeights2',
                      'bayesglm','xgbDART','xgbLinear','xgbTree','knn')
  }
  
  # initialize the out dataframe
  out = tibble(Algorithm = character(length(model.methods)))
  
  # create the formula
  form = reformulate(termlabels = names(train[,-1]), response = names(train[1]))
  
  # initialize iterator for the out dataframe
  x = 1
  
  # initialize the model
  model = model.methods[1]
  
  # loop over the models
  for(model in model.methods){
    
    # train model via caret
    mod.caret <- caret::train(form,
                              data = train,
                              method = model,
                              trControl = trainControl,
                              preProc = c('center', 'scale'),
                              metric = error.metric)
    
    # fill in the algorithm and error metrics for each model
    out$Algorithm[x] = mod.caret$method
    if(problem.type == 'regression'){
      out$RMSE[x] = mean(mod.caret$results$RMSE, na.rm = T)
      out$MAE[x] = mean(mod.caret$results$MAE, na.rm = T)
      out$r.squared[x] = mean(mod.caret$results$Rsquared, na.rm = T)
    }
    if(problem.type == 'classification'){
      out$Accuracy[x] = mean(mod.caret$results$Accuracy, na.rm = T)
    }
    
    # increment x to move to the next row in our dataframe
    x = x + 1
  }
  # create plots of error metrics
  i = 1
  plotlist = list()
  for(i in 1:(ncol(out)-1)){
    if(i == 1){
      ylab = 'Algorithm'
    } else {
      ylab = ''
    }
    if(problem.type == 'regression'){
      ggplot(out, aes(x = !!sym(names(out)[i+1]), 
                      y = reorder(!!sym(names(out)[1]), desc(!!sym(names(out)[i+1]))))) +
        geom_point() +
        ylab(ylab) +
        theme_bw() -> p
    }
    if(problem.type == 'classification'){
      ggplot(out, aes(x = !!sym(names(out)[i+1]), 
                      y = reorder(!!sym(names(out)[1]), !!sym(names(out)[i+1])))) +
        geom_point() +
        ylab(ylab) +
        theme_bw() -> p
    }
    
    plotlist[[i]] = p
  }
  
  # combine plots into one
  p = grid.arrange(grobs = plotlist, nrow = 1)
  
  # print plots and return out dataframe
  print(p)
  return(out)
}
