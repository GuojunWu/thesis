# In this step, we are going to detect non-linearity

# 1. Partial Dependence Plot

train_pdp <- trainingdata
test_pdp <- test
var_all <- vector()
dummy <- vector()

for (id_linear in 1:length(variables_imp)) {
  AUC_pdp <- vector()
  
  trial <- data.frame(trainingdata[, features_imp[id_linear] + 1])
  colnames(trial) <- c(variables_imp[id_linear])
  p <-
    partial(RF,
            pred.var = variables_imp[id_linear],
            pred.grid = trial,
            probs = T)
  x <- p[, 1]
  y <- p[, 2]
  
  y <- log(y / (1 - y))
  
  # 2. Check if pdp is linear
  fit1 <- lm(y ~ x)
  reset <- resettest(fit1, power = 2:3)
  
  p_value <- reset$p.value
  if (p_value >= 0.05) {
    next
  }
  
  # 3. Select the kink
  
  # with default cp
  fit <- rpart(y ~ x, method = "anova")
  table <- as.data.frame(fit$cptable)
  
  
  var_d <- vector()
  for (m in 1:length(table$CP)) {
    fit <-
      rpart(y ~ x,
            method = "anova",
            data = trainingdata,
            cp = table$CP[m])
    splits <- fit$splits[, "index"]
    splits_ord <- sort(splits, decreasing = FALSE)
    
    # Dummy variable
    
    if (m >= 2) {
      dummy_name <- features_imp[id_linear]
      train_pdp <-
        cbind(train_pdp, ifelse(train_pdp[, dummy_name + 1] > splits_ord[m - 1], 1, 0))
      colnames(train_pdp)[length(train_pdp)] = paste("d", dummy_name, m, sep =
                                                       "_")
      
      test_pdp <-
        cbind(test_pdp, ifelse(test_pdp[, dummy_name + 1] > splits_ord[m - 1], 1, 0))
      colnames(test_pdp)[length(test_pdp)] = paste("d", dummy_name, m, sep =
                                                     "_")
      
      var_d <- c(var_d, paste("d", dummy_name, m, sep = "_"))
      
      
      
    }
    
    
    # K fold AUC
    folds = 10
    flds <- createFolds(trainingdata$Y, k = folds)
    AUC_fold <- rep(0, folds)
    for (f in 1:folds)
    {
      if (m == 1) {
        model <-
          glm(
            formula_imp(variables_imp),
            family = binomial(link = 'logit'),
            data = trainingdata[-flds[[f]],]
          )
        AUC_fold[f] <- AUC_LR(model, trainingdata[flds[[f]],])$auc
        
      } else{
        model <-
          glm(formula_imp(c(variables_imp[-id_linear], var_d)), family = binomial(link =
                                                                                    'logit'), train_pdp[-flds[[f]],])
        AUC_fold[f] <- AUC_LR(model, train_pdp[flds[[f]],])$auc
        
      }
      
    }
    
    AUC_pdp[m] <- mean(AUC_fold)
  }
  
  
  if (which.max(AUC_pdp) == 1) {
    var_d_sel <- NULL
    
  } else{
    var_d_sel <- var_d[c(1:(which.max(AUC_pdp) - 1))]
    dummy <- c(dummy, dummy_name)
    
    
  }
  
  if (run_plots) {
    plot(
      x,
      y,
      lty = 3,
      lwd = 2,
      cex.lab = 1.15,
      cex.axis = 1.15
    )
    fit <-
      rpart(y ~ x,
            method = "anova",
            data = trainingdata,
            cp = table$CP[which.max(AUC_pdp)])
    pd <- predict(fit , newdata = trainingdata)
    lines(x,
          pd,
          col = "red",
          lwd = 2,
          lty = 1)
    plot(
      table$nsplit,
      AUC_pdp,
      ylab = "AUC",
      xlab = "number of splits",
      type = "b",
      lty = 3,
      lwd = 2,
      cex.lab = 1.15,
      cex.axis = 1.15
    )
    
    
  }
  
  
  var_all <- c(var_d_sel, var_all)
  
}


no_dummy <- setdiff(features_imp, dummy)
no_dummy_var <- paste("X", no_dummy, sep = "")