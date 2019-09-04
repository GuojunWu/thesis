# In this step, we do dataset preprocess, reforamt and split


# 1. Reformat dataset ---------------------------------------

str(datanew)


data_new <-
  cbind(dplyr::select(datanew, Y = contains(y_name)), datanew) # Rename y and move to the first place

data_new <-
  dplyr::select(data_new, -contains(y_name))  # Delet class variable and instance
data_new <-   dplyr::select(data_new, -contains(Inst_name))
# data_new <-   dplyr::select(data_new,-contains("ndex"))

data_new$Y <- relevel(data_new$Y, ref = class_name) # Relevel Y

for (m in 2:ncol(data_new))
  colnames(data_new)[m] <- paste("X", m - 1, sep = "") # Renamce X

str(data_new)

# 2. Split dataset into test and train 70% ------------------------

set.seed(123)
partition <- createDataPartition(data_new$Y, p = .7, list = F)
trainingdata <- data_new[partition, ]
test <- data_new[-partition, ]