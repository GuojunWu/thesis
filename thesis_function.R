
# "models.R" --------------------------------------------------------------------------------- 
AUC_LR <- function(model_lr, test) {
  prob_lr <- predict(model_lr, test, type = "response")
  pred_lr <- prediction(prob_lr, test$Y)
  perf <- performance(pred_lr, measure = "auc")
  auc <- perf@y.values[[1]]
  out <- list(auc = auc)
  return(out)
}



AUC_LA <- function(model_la, lambda, test) {
  prob_lasso <- predict(
    model_la,
    s = lambda,
    newx = model.matrix(Y ~ 0 + .,
                        data =  test),
    type = "response"
  )
  pred_lasso <- prediction(prob_lasso, test$Y)
  perf <- performance(pred_lasso, measure = "auc")
  auc <- perf@y.values[[1]]
  out <- list(auc = auc)
  return(out)
}

AUC_RF <- function(model_rf, test) {
  prob_rf <- predict(model_rf, newdata = test, type = "prob")
  pred_rf <- prediction(prob_rf[, 2], test$Y)
  perf <- performance(pred_rf, measure = "auc")
  auc <- perf@y.values[[1]]
  out <- list(auc = auc)
  return(out)
}



# "features_select.R" 1. Delete extreme value ----------------------------------------------------

# Description:
# 1. Variables
# imp_ord:Ordered importance (ascending)
# x_ord: ordered regressors (ascending)
# 
# top: how much top percentage of features you want to retain
# bottom: how much bottom percentage of features you want to drop
# eg:top = 0.95, down = 0.05 -> only have 5 %-95% range of data
# 
# level: only retain value above this level
# eg: level = 0, only retain postive importance
# default: all is selected
#
# 2. Function results
# x: get regressors after delecting
# imp: get imp of regressors after delecting
# both are orderd in ascending importance

delete <- function(value_ord, x_ord, top, bottom, level)
{
    
    # set null value
    if(missing(level))
    {level <- -999} else {
    
    # Only retain value above this level
    posit_imp_ord <- value_ord[value_ord > level]}
    # Compute top bottom cut off value
    #
    # If don't want selection: probs = 1, probs = 0
    top_imp <- quantile(posit_imp_ord, probs = top)
    bottom_imp <- quantile(posit_imp_ord, probs = bottom)
    
    # 1. Selected regressors (imp ascending)
    sel_x_ord <- x_ord[value_ord >= bottom_imp & value_ord <= top_imp] 
    
    # 2. Importance of selected regressors (ascending)
    sel_imp_ord <- imp[sel_x_ord, 1]
    sel_imp_ord <- as.matrix(sel_imp_ord)
    
    out <-
      list(
        x = sel_x_ord,
        imp = sel_imp_ord
      )
    return(out)

}


# "features_select.R" 2. Second derivative -------------------------------------------------------

trunc <- function(feature) {
  
  # Truncated power smooth
  index = 1:length(feature) 
  
  num.knots = max(5, min(floor(length(unique(
    index
  )) / 4), 35))
  knots = quantile(unique(index), seq(0, 1, length = (num.knots + 3))[-c(1, (num.knots +
                                                                               3))])
  n <- num.knots + 3
  matrix_x <- matrix(0, nrow = length(index), ncol = n)
  
  dimnames(matrix_x) <-
    list(paste("feature", index), paste("x", 1:n, sep = ""))
  matrix_x[, 1] <- index
  matrix_x[, 2] <- index ^ 2
  matrix_x[, 3] <- index ^ 3
  
  for (i in 4:n) {
    matrix_x[, i] <- (pmax((index - knots[[i - 3]]), 0)) ^ 3
  }
  
  y <- feature
  matrix_xnew <- cbind(y, matrix_x)
  
  fit_new <- lm(y ~ ., data = data.frame(matrix_x))
  
  predict_imp <- predict(fit_new, data.frame(matrix_x))
  
  # First Derivative #########################
  deri_x <- matrix(0, nrow = length(index), ncol = n)
  dimnames(deri_x) <-
    list(paste("feature", index), paste("x", 1:n, sep = ""))
  deri_x[, 1] <- 1
  deri_x[, 2] <- 2 * index
  deri_x[, 3] <- 3 * index ^ 2
  
  for (i in 4:n) {
    deri_x[, i] <- 3 * pmax((index - knots[[i - 2]]), 0) ^ 2
  }
  
  coefficient <- fit_new$coefficients
  deri_y <- matrix(0, nrow = length(index), ncol = n)
  
  for (i in 1:n) {
    deri_y[, i] <- deri_x[, i] * coefficient[[i + 1]]
  }
  
  sum_per_row <- apply(deri_y, 1, sum)
  
  # Second Derivative #######################
  deri_second_x <- matrix(0, nrow = length(index), ncol = n)
  dimnames(deri_second_x) <-
    list(paste("feature", index), paste("x", 1:n, sep = ""))
  deri_second_x[, 1] <- 0
  deri_second_x[, 2] <- 2
  deri_second_x[, 3] <- 6 * index
  
  for (i in 4:n) {
    deri_second_x[, i] <- 6 * pmax((index - knots[[i - 2]]), 0)
  }
  
  coefficient <- fit_new$coefficients
  deri_second_y <- matrix(0, nrow = length(index), ncol = n)
  
  
  for (i in 1:n) {
    deri_second_y[, i] <- deri_second_x[, i] * coefficient[[i + 1]]
  }
  
  sum_per_row_second <- apply(deri_second_y, 1, sum)
  
  out <-
    list(
      importPredict = predict_imp,
      first_derivative = sum_per_row,
      second_derivative = sum_per_row_second
    )
  return(out)
}

backward <- function(feature){
  
  n_derivat <- length(feature)
  
  # First Derivative #######################
  # (f(xi)-f(xi-1))/(1)
  derivat_first <- rep(0, n_derivat)
  derivat_first[1] <- c(NA)
  for (i in 2:n_derivat) {
    derivat_first[i] = feature[i] - feature[i - 1]
  
  }
  
  # Second Derivative #######################
  # (f(xi) - 2f(xi-1)+f(xi-2))/(1^2)
  derivat_second <- rep(0, n_derivat)
  derivat_second[1:2] <- c(NA,NA)
  for (i in 3:n_derivat) {
    derivat_second[i] = feature[i] - 2 * feature[i - 1] + feature[i - 2]
    
  }
  
  
  out <-
    list(
      first_derivative = derivat_first,
      second_derivative = derivat_second
    )
  return(out)
}

# "features_select.R" 3. Threshold ---------------------------------------------------------------


select <- function(dataset, x_ord,threshold,rule,peak){
  if (rule == 1){
    # position of number before first negative 2rd derivative in "sel_x_ord"
    sel_posit <- which(dataset[,4] < threshold)[1]-1 #!!! 0 can be any threshold :)
  } else if (rule == 2){
    
    dataset_drop <- dataset

    sel_posit = 0
    for (i in 1:peak) 
    {
      posit <- which.max(dataset_drop[,3])
      dataset_drop <- dataset_drop[-c(1:sel_posit),]
      sel_posit <- sel_posit+posit
      
    }
    
    
    
  } else {
    print(":)")
  }
  
  # threshold: regressor before the first negative number
  x_cut <- dataset[sel_posit,1]
  
  # selected features
  # position in "x_ord" 
  x_ord_rev <- rev(x_ord)
  posit <- which(x_ord_rev==x_cut)
  # selected features
  features_imp <- x_ord_rev[1:posit]
  features_unimp <- x_ord_rev[-c(1:posit)]
  
  out <-
    list(
      x = features_imp,
      x_unimp = features_unimp
    )
  return(out)
  
}


# "features_select.R" 4. formula ----------------------------------------------------------------

formula_imp <- function(features) {

  # "Y~X1+X2..."
  fo <- paste("Y",
              paste(features, collapse = " + "),
              sep = " ~ ")
  # Y~X1+X2...
  f <- as.formula(fo)
  
  out <-
    list(formula = f)
  return(out)
}








