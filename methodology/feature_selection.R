# This part is for selecting important features.
# After running random forest model, the importance is measured as
# the factor by which the model¡¯s prediction error increases when the feature is shuffled.
#
# (1) Compute importance
niter <- 100
datatoy <- matrix(0, length(trainingdata[-1]), niter)

for (i in 1:niter) {
  RF <-
    randomForest(Y ~ .,
                 data = trainingdata ,
                 importance = TRUE,
                 type = 1)
  imp <- importance(RF, type = 1, scale = F)
  datatoy[, i] = imp
}

imp_avg <- apply(datatoy, 1, mean)

x_ord <- order(imp_avg, decreasing = F)

datatoy_ord <- datatoy[x_ord, ]

# first difference
first_der <-
  datatoy_ord[2:length(trainingdata[, -1]), ] - datatoy_ord[1:(length(trainingdata[, -1]) -
                                                                 1), ]

first_der_std <- apply(first_der, 1, sd)

# CART
data_first_std <-
  data.frame(x = 1:(length(trainingdata[, -1]) - 1), y = first_der_std)
fit_first_std <-
  rpart(y ~ x,
        data = data_first_std,
        method = "anova",
        cp = 0)
pd <- predict(fit_first_std , newdata = data_first_std)
par(mfrow = c(2, 2))

# first difference of average imp
ord_imp <- imp_avg[x_ord]
first_der_avg <-
  ord_imp[2:length(trainingdata[, -1])] - ord_imp[1:(length(trainingdata[, -1]) -
                                                       1)]


if (run_plots) {
  plot(
    ord_imp,
    xlab = "features",
    ylab = "mean of importance",
    type = "b",
    lty = 3,
    lwd = 2,
    cex.lab = 1.15,
    cex.axis = 1.15
  )
  plot(
    first_der_avg,
    xlab = "features",
    ylab = "mean of first difference",
    type = "b",
    lty = 3,
    lwd = 2,
    cex.lab = 1.15,
    cex.axis = 1.15
  )
}

thres <- min(pd)
threshold <- n * thres
abline(h = threshold,
       lwd = 2,
       lty = 2,
       col = "red")

# select
sel_posit <- which(first_der_avg > threshold)[1]
# obtained
sel_x <- x_ord[sel_posit:length(trainingdata[, -1])]
# delete
del_x <- x_ord[-(sel_posit:length(trainingdata[, -1]))]


# (2) Run model on validation set

AUC_imp = rep(0, length(rev(sel_x)))

for (m in 1:length(rev(sel_x))) {
  # K fold
  folds = 10
  flds <- createFolds(trainingdata$Y, k = folds)
  AUC_fold <- rep(0, folds)
  for (f in 1:folds)
  {
    variables_imp <- paste("X", rev(sel_x)[1:m] , sep = "")
    model <-
      glm(formula_imp(variables_imp),
          family = binomial(link = 'logit'),
          trainingdata[-flds[[f]],])
    
    AUC_fold[f] <- AUC_LR(model, trainingdata[flds[[f]],])$auc
  }
  
  AUC_imp[m] <- mean(AUC_fold)
}

if (run_plots) {
  plot(
    first_der_std,
    xlab = "features",
    ylab = "standard deviation of first difference",
    type = "b",
    lty = 3,
    lwd = 2,
    cex.lab = 1.15,
    cex.axis = 1.15
  )
  lines(1:(length(trainingdata[, -1]) - 1), pd, col = "red", lwd = 2)
}

if (run_plots) {
  plot(
    1:length(rev(sel_x)),
    AUC_imp,
    xlab = "nested models",
    ylab = "AUC",
    type = "b",
    lty = 3,
    lwd = 2,
    cex.lab = 1.15,
    cex.axis = 1.15
  )
}


features_imp <- rev(sel_x)[1:which.max(AUC_imp)]
variables_imp <-
  paste("X", rev(sel_x)[1:which.max(AUC_imp)] , sep = "")
