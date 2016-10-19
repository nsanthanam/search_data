compare.algos <- function(DF, target.var, profit.col = 'profit', 
                          algos = c("xgboost", "ridge", "lasso", "enet", "rf.unbalanced", "rf.balanced"),
                          algo.params = list("ridge" = list("cutoff" = "mean"), "enet" = list("cutoff" = "mean"), "lasso" = list("cutoff" = "mean"), "xgboost" = list("cutoff" = "mean.plus.sd")),
                          len.samp = NA,
                          max.runs = 50,
                          data.version = '') {
  n.runs = seq(1, max.runs)
  
  def5 <- DF
  # Store precision and recall for various algos
  cnames.pr <- c('accuracy.class0', 'accuracy.class1', 'run', 'algo')
  prec.rec <- as.data.frame(matrix(nrow = length(n.runs)*length(algos), ncol = length(cnames.pr), dimnames = list(c(), cnames.pr)))
  
  def5[, target.var] = ifelse(def5[, target.var] != 0, 1, 0)
  def5[, target.var] = factor(def5[, target.var], levels = sort(unique(def5[, target.var])), ordered = T)
  
  for(j in algos) {
    for(k in n.runs) {
      tt <- train.test.split(def5, tv = target.var)
      tr.rows <- tt$tr.rows
      train.x <- tt$train.x
      train.y <- tt$train.y
      test.x <- tt$test.x
      test.y <- tt$test.y
      actual <- as.numeric(test.y)-1
      
      rl <- switch(j,
                   'ridge' = run.glmnet(train.x, train.y, test.x, test.y, algo = "ridge", algo.params$ridge),
                   'lasso' = run.glmnet(train.x, train.y, test.x, test.y, algo = "lasso", algo.params$lasso),
                   'enet' = run.glmnet(train.x, train.y, test.x, test.y, algo = "enet", algo.params$enet),
                   'xgboost' = run.xgboost(train.x, train.y, test.x, test.y, algo.params$xgboost),
                   'rf.unbalanced' = run.rf(def5, target.var, "unbalanced", tr.rows),
                   'rf.balanced' = run.rf(def5, target.var, "balanced", tr.rows))
      
      predicted.test <- rl$predicted.test
      tab <- rl$tab
      pos <- (which(algos == j)-1)*max.runs+k
      prec.rec[pos, 1] <- tab[1, 1]*100/(tab[1, 1] + tab[1, 2])
      prec.rec[pos, 2] <- tab[2, 2]*100/(tab[2, 1] + tab[2, 2])
      prec.rec[pos, 3] <- n.runs[k]
      prec.rec[pos, 4] <- j
    }
  }
  
  # Plot accuracy for various algorithms
  prec.rec.melt <- melt(prec.rec, id.vars = c('run', 'algo'))
  prec.rec.melt <- prec.rec.melt %>% .[.$variable %in% c('accuracy.class0', 'accuracy.class1'), ]
  prec.rec.melt$algo <- factor(prec.rec.melt$algo, ordered = T)
  acc_plot <- generic.ggplot(data = prec.rec.melt, x.axis = "run", y.axis = "value", clr = "algo", sz = 2, txt.cex = 0.65,
                             plot.type = c("smooth.no.se", "scatter"), save.figure = F, facet.x = "variable", x.lbl.size = 12,
                             facet.y = "algo", x.label = "Run Number", y.label = "Percentage", print.figure = F) + 
    scale_y_continuous(breaks = seq(0, 100, 25), labels = as.character(seq(0, 100, 25)))
  print.figure(plotname = acc_plot)
  
  retlist <- list('prec.rec' = prec.rec, 'prec.rec.plot' = acc_plot)
  return(retlist)
}

run.glmnet <- function(train.x, train.y, test.x, test.y, algo = c("ridge", "lasso", "enet"), algo.params = list("cutoff" = "mean")) {
  alpha <- switch(algo %>% tolower(), 
                  ridge = 0,
                  lasso = 1,
                  enet = 0.5)
  train.y.numeric <- train.y %>% as.numeric()-1
  if(train.y.numeric[train.y.numeric == 0] %>% length() >= 2) {
    glm.def <- glmnet(x = train.x, y = train.y, family = 'binomial', alpha = alpha)
    lambda <- glm.def$lambda %>% .[length(.)]
    pred.vals.glm <- as.vector(predict(glm.def, newx = test.x, type = 'response', s = lambda, exact = FALSE))
    
    # predicted.test <- ifelse(pred.vals.glm > mean(pred.vals.glm), 1, 0)
    predicted.test <- predict.based.on.cutoff(pred.vals.glm, criterion = algo.params$cutoff)
    tab <- table(test.y, predicted.test)
    if(ncol(tab) == 1) {
      tab <- cbind(tab, c(0, 0))
      colnames(tab) <- c(0, 1)
    }
  } else {
    print(train.y)
    predicted.test <- rep(NA, length(test.y))
    tab <- matrix(data = rep(NA, 4), nrow = 2, ncol = 2)
  }
  
  retlist <- list("predicted.test" = predicted.test, "tab" = tab)
  return(retlist)
}

run.rf <- function(DF, target.var, algo = c("balanced", "unbalanced"), tr.rows) {
  if(algo == "unbalanced") {
    rf <- randomForest(y = DF[, target.var], x = DF[, !colnames(DF) == target.var], 
                       ntree = 501, importance = F, proximity = F, nodesize = 10)
  } else {
    n.0 <- sum(DF[, target.var] == 0)
    n.1 <- sum(DF[, target.var] == 1)
    rf <- randomForest(y = DF[, target.var], x = DF[, !colnames(DF) == target.var], ntree = 501, importance = F, 
                       proximity = F, strata = DF[, target.var], sampsize = rep(min(n.0, n.1), 2), nodesize = 10)
  }
  
  tab <- rf$confusion
  predicted <- as.numeric(rf$predicted)-1
  predicted.test <- as.vector(predicted[-tr.rows])
  
  retlist <- list("predicted.test" = predicted.test, "tab" = tab)
  return(retlist)
}

run.xgboost <- function(train.x, train.y, test.x, test.y, algo.params = list("cutoff" = "mean")) {
  train.y <- as.numeric(train.y)-1
  xgboost.def <- xgboost(data = train.x, label = train.y, nround = 5, objective = "binary:logistic", verbose = 0)
  pred.vals.xgboost <- predict(xgboost.def, test.x)
  
  predicted.test <- predict.based.on.cutoff(pred.vals.xgboost, criterion = algo.params$cutoff)
  tab <- table(test.y, predicted.test)
  if(ncol(tab) == 1) {
    tab <- cbind(tab, c(0, 0))
    colnames(tab) <- c(0, 1)
  }
  
  retlist <- list("predicted.test" = predicted.test, "tab" = tab)
  return(retlist)
}

predict.based.on.cutoff <- function(predicted.values, criterion = c("mean.minus.sd", "mean", "mean.plus.sd", "mean.plus.1.5sd", "mean.plus.2sd")) {
  predicted.test <- switch(criterion,
                           mean.minus.sd = ifelse(predicted.values > mean(predicted.values)-sd(predicted.values), 1, 0),
                           mean = ifelse(predicted.values > mean(predicted.values), 1, 0),
                           mean.plus.sd = ifelse(predicted.values > mean(predicted.values)+sd(predicted.values), 1, 0),
                           mean.plus.1.5sd = ifelse(predicted.values > mean(predicted.values)+1.5*sd(predicted.values), 1, 0),
                           mean.plus.2sd = ifelse(predicted.values > mean(predicted.values)+2*sd(predicted.values), 1, 0))
  return(predicted.test)
}

train.test.split <- function(DF, tv, tr.frac = 0.75) {
  tr.rows <- sample(1:nrow(DF), tr.frac*nrow(DF))
  train <- as.data.frame(DF[tr.rows, ])
  train.x <- data.matrix(DF[tr.rows, !colnames(DF) %in% tv])
  train.y <- DF[tr.rows, tv]
  test <- as.data.frame(DF[-tr.rows, ])
  test.x <- data.matrix(DF[-tr.rows, !colnames(DF) %in% tv])
  test.y <- DF[-tr.rows, tv]
  
  retlist <- list('tr.rows' = tr.rows, 
                  'train' = train,
                  'train.x' = train.x,
                  'train.y' = train.y,
                  'test' = test,
                  'test.x' = test.x,
                  'test.y' = test.y)
  return(retlist)
}