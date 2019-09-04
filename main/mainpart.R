source('./thesis_function.r')
source('./library.r')

# Cancel scientific counting
options(scipen = 200)

# Whether to get output and command from source
output = T
command = F

# Whether to run plots
run_plots = T

# Section 1: load data ---------------------------------
# id = c(2, 24, 9 , 25, 30,  5, 33,  8,  7, 18, 17, 26, 14, 22, 15) # selecting from these id, source('./OpenML.r')

setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f")

setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f")
datasets = listOMLDataSets(
  number.of.instances = c(1, 1000),
  number.of.features = c(40, 100),
  number.of.classes = 2,
  number.of.missing.values = 0
)

ds = getOMLDataSet(data.id = datasets$data.id[1]) #! id[] !#
datanew <- ds$data
ds
str(datanew)

# Section 2: Preprocess -------------------------------
# relevel, reformate
y_name = "c"
Inst_name = "name"
class_name = "FALSE"

source('./preprocess.r', print.eval  = output, echo = command)

# Section 3: run basic models ---------------------------------

source('./models.r',       print.eval  = F,
       echo = F)
length(LR2$coefficients)
sum(coef(LA1, LA2$lambda.min)[, 1] != 0)
AUC_LR(LR2, test)
AUC_LA(LA1, lambda_opt, test)
AUC_RF(RF, test)

# Section 4:important features ---------------------------------

# 1. confidence interval for threshold
n = 1
source('./feature_selection.r',
       print.eval  = output,
       echo = command)

# 2. preminarly selected or deleted x
sel_x
del_x

# 3. finally selected
features_imp
# variables get from imp
variables_imp


# Section 5: non-linearity ----------------------------------------------
source('./non_linearity.r',       print.eval  = output,
       echo = command)

# discretizad variables get from non-linearity
var_all

# not dummied var
no_dummy
no_dummy_var




# Section 6: interaction ---------------------------------------
peak = 1
threshold.h = NULL
rule = 2

source('./interactionselect.r',
       print.eval  = output,
       echo = command)

feature_int



features1_int_imp <- c(var_all, no_dummy_var, feature_int)

# Formulation

f <- formula_imp(features = features1_int_imp)$formula

# 5. Improved LR ---------------------------------------------------------------------
LR4 <- glm(f, family = binomial(link = 'logit'), data = train_pdp)
AUC_LR(LR4, test_pdp)

# number of inputs
length(no_dummy_var)
length(variables_imp) - length(no_dummy_var)
length(feature_int)
length(var_all)

