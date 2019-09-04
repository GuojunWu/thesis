# This part is for running lr, lasso, rf.
# Large difference in AUC is expected.


# 1. Run models ---------------------------------------

# LR
LR <- glm(Y ~ ., family = binomial(link = 'logit'), data = trainingdata)
stepboth <-
  stepAIC(LR,
          k = log(ncol(data_new) - 1),
          scope = list(upper =  ~ ., lower =  ~ 1))
LR2 <-
  glm(stepboth$formula,
      family = binomial(link = 'logit'),
      data = trainingdata)
length(LR2$coefficients)
# LASSO
LA1 <- glmnet(
  x = model.matrix(Y ~ 0 + .,
                   data =  trainingdata),
  y = trainingdata[, 1],
  alpha = 1,
  family = "binomial"
)
LA2 <-
  cv.glmnet(
    x = data.matrix(trainingdata[, -1]),
    y = trainingdata[, 1],
    alpha = 1,
    family = "binomial"
  )
lambda_opt <- LA2$lambda.min
sum(coef(LA1, LA2$lambda.min)[, 1] != 0)


# RF
RF <-
  randomForest(Y ~ .,
               data = trainingdata ,
               importance = TRUE,
               type = 1)

# 2. Evaluation AUC ---------------------------------------
AUC_LR(LR2, test)
AUC_LA(LA1, lambda_opt, test)
AUC_RF(RF, test)
