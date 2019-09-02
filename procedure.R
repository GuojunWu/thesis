# Main code

source('./thesis_function.r')
source('./library.r')
# source('./install_library.r')

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

datasets = listOMLDataSets(
  number.of.instances = c(1, 1000),
  number.of.features = c(40, 100),
  number.of.classes = 2,
  number.of.missing.values = 0
)
# id = c(2, 24, 9 , 25, 30,  5, 33,  8,  7, 18, 17, 26, 14, 22, 15) # selecting dataset: source('./OpenML.r')
ds = getOMLDataSet(data.id = datasets$data.id[31]) #! id[] !#
datanew <- ds$data
ds
str(datanew)

# Get data from UCI
datanew <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/undocumented/connectionist-bench/sonar/sonar.all-data"),header = F,sep = ",", quote = "\"'")


# Section 2: Preprocess -------------------------------
# relevel, reformate
y_name = "V61"
Inst_name = "instance"
class_name = "M"

source('./preprocess.r',print.eval  = output,echo = command)


# Section 3: run basic models ---------------------------------

source('./models.r',       print.eval  = output,
       echo = command)

# Section 4:important features ---------------------------------

# 1. confidence interval for threshold
n = 1
source('./input_selection.r',       print.eval  = output,
       echo = command)
# 2. preminarly selected or deleted x
sel_x
del_x

# 3. finally selected 
features_imp
# variables get from imp
variables_imp


# Section 5: pdp ----------------------------------------------
source('./pdp.r',       print.eval  = output,
       echo = command)
# variables get from pdp
var_all

# not dummied var
no_dummy




# Section 6: interaction ---------------------------------------
peak = 1
threshold.h = NULL
rule = 2

source('./lala.r',       print.eval  = output,
       echo = command)

feature_int



length(no_dummy)
length(var_all)
length(feature_int)
features1_int_imp <- c(var_all,no_dummy,feature_int)


# Formulation 

f <- formula_imp(features = features1_int_imp)$formula

# 5. Improved LR ---------------------------------------------------------------------
LR4 <- glm(f,family=binomial(link='logit'),data=train_pdp)
AUC_LR(LR4,train_pdp)


LR4 <- glm(formula_imp(variables_imp),family=binomial(link='logit'),data=trainingdata)
