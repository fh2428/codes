library(dplyr)
library(ggplot2)
library(readr)
library(magrittr)
library(mrfDepthLight)
library(class)
library(klaR)
library(e1071)
source('/home/ad/Desktop/KUL Course Material/Robust Statistics/Project/DO_code_clean.R')
source('/home/ad/Desktop/KUL Course Material/Robust Statistics/Project/classificationcode.R')

#####################################################################################################
## Hubert 2010 dataset (belgian national household 2005 survey): Processing
#####################################################################################################

vars = c("clothing", "alcoholic_drinks", "durable_consumer_goods", "energy", "food", "health", "leisure", "nonalcoholic_drinks", "transport", "housing", "income")

employed = read_csv("/home/ad/Desktop/KUL Course Material/Robust Statistics/Project/Data/Hubert2010_emp.csv", col_names = FALSE)

unemployed = read_csv("/home/ad/Desktop/KUL Course Material/Robust Statistics/Project/Data/Hubert2010_unemp.csv", col_names = FALSE)

names(employed) = vars
names(unemployed) = vars

employed$employment = 1

unemployed$employment = 0

hubert_data = rbind(employed, unemployed) %>% mutate(employment = as.factor(employment))

glimpse(hubert_data)

## pairs plot for the variables
pairs(formula = ~., data = hubert_data[,-12], col = hubert_data$employment)

#####################################################################################################
## Transforming the data for DistSpace + functions for transform and knn
#####################################################################################################

# function which transforms the data
dist_transform = function(transform.method = outlyingness) {
  
  # converting the string to name object
  # dist_method = as.name(method)
  
  # distance from unemployed to unemployed
  dist00 <- data.frame(
    distance = transform.method(hubert_data[grp0, -12], hubert_data[grp0, -12])$outlyingnessZ,
                       class = rep(0, nrow(hubert_data[grp0,]))
    )
  
  # distance from employed to unemployed
  dist01 <- data.frame(
    distance = transform.method(hubert_data[grp0, -12], hubert_data[grp1, -12])$outlyingnessZ,
                       class = rep(1, nrow(hubert_data[grp1,])))
  
  # distance from unemployed to employed
  dist10 <- data.frame(
    distance = transform.method(hubert_data[grp1, -12], hubert_data[grp0, -12])$outlyingnessZ,
                       class = rep(0, nrow(hubert_data[grp0,])))
  
  # distance from employed to employed
  dist11 <- data.frame(
    distance = transform.method(hubert_data[grp1, -12], hubert_data[grp1, -12])$outlyingnessZ,
                       class = rep(1, nrow(hubert_data[grp1,])))
  
  dist0 = rbind(dist00, dist01)
  dist1 = rbind(dist10, dist11)
  
  transformed = data.frame(distG0 = dist0, distG1 = dist1) %>% 
    dplyr::select(-distG1.class) %>% 
    rename(class = distG0.class, distG0 = distG0.distance, distG1 = distG1.distance) %>%
    mutate(class = as.factor(class)) 
  
  return(transformed)
}

# function which performs knn
knn_function = function(train, test) {
  
  # trying different neighbourhood lengths
  accuracy <- rep(0, 10)
  k <- 1:10
  
  for (x in k) {
    prediction <- knn(train[,c(1,3)], test[,c(1,3)], train[,2], k = x)
    accuracy[x] <- mean(prediction == test[, 2])
  }
  # plot(k, accuracy, type = 'b')
  
  k_best = which.max(accuracy)
  
  method_class = knn(train[,c(1,3)], test[,c(1,3)], train[,2], k = k_best)
  # summary(sdo_class)
  
  results = table(test[,2], method_class)
  
  # misclassifications
  misclassif = results[1, 2] + results[2, 1]
  #misclassif/length(test$class)
  
  return(list(misclassif = misclassif, k_best = k_best))
}

# getting the indices for the groups
grp0 = which(hubert_data$employment == 0)
grp1 = which(hubert_data$employment == 1)

#### SDO
dist_sdo = dist_transform(transform.method = outlyingness)

#### AO
dist_ao = dist_transform(transform.method = adjOutlyingness)

#### BD
#dist_bd = dist_transform(transform.method = bagdistance, element = bagdistance)
# this failed so doing it manually

bd00 <- data.frame(
  distance = bagdistance(hubert_data[grp0, -12], hubert_data[grp0, -12])$bagdistance, class = 0)

bd01 <- data.frame(
  distance = bagdistance(hubert_data[grp0, -12], hubert_data[grp1, -12])$bagdistance, class = 1)

bd10 <- data.frame(
  distance = bagdistance(hubert_data[grp1, -12], hubert_data[grp0, -12])$bagdistance, class = 0)

bd11 <- data.frame(
  distance = bagdistance(hubert_data[grp1, -12], hubert_data[grp1, -12])$bagdistance, class = 1)

bd0 = rbind(bd00, bd01)
bd1 = rbind(bd10, bd11)

dist_bd = data.frame(distG0 = bd0, distG1 = bd1) %>% 
  dplyr::select(-distG1.class) %>% 
  rename(class = distG0.class, distG0 = distG0.distance, distG1 = distG1.distance) %>%
  mutate(class = as.factor(class))

#### DO
#dist_do = dist_transform(transform.method = DOclassif)
do00 <- data.frame(
  distance = DOclassif(data.matrix(hubert_data[grp0, -12]), data.matrix(hubert_data[grp0, -12])), 
  class = 0)

do01 <- data.frame(
  distance = DOclassif(data.matrix(hubert_data[grp0, -12]), data.matrix(hubert_data[grp1, -12])), 
  class = 1)

do10 <- data.frame(
  distance = DOclassif(data.matrix(hubert_data[grp1, -12]), data.matrix(hubert_data[grp0, -12])), 
  class = 0)

do11 <- data.frame(
  distance = DOclassif(data.matrix(hubert_data[grp1, -12]), data.matrix(hubert_data[grp1, -12])), 
  class = 1)

do0 = rbind(do00, do01)
do1 = rbind(do10, do11)

dist_do = data.frame(distG0 = do0, distG1 = do1) %>% 
  dplyr::select(-distG1.class) %>% 
  rename(class = distG0.class, distG0 = distG0.distance, distG1 = distG1.distance) %>%
  mutate(class = as.factor(class)) 

#####################################################################################################
## Running Simulations
#####################################################################################################

## specifying the stuff for the simulation
n_obs = dim(hubert_data)[1]
n_sim = 2000 # 10

# data frame that contains the misclassification results for the simulations
data_results = data.frame(
  misclassSdo = rep(NA, n_sim), misclassPercSdo = rep(NA, n_sim), Sdo_k = rep(NA, n_sim),
  misclassBd = rep(NA, n_sim), misclassPercBd = rep(NA, n_sim), Bd_k = rep(NA, n_sim),
  misclassAo = rep(NA, n_sim), misclassPercAo = rep(NA, n_sim), Ao_k = rep(NA, n_sim),
  misclassDo = rep(NA, n_sim), misclassPercDo = rep(NA, n_sim), Do_k = rep(NA, n_sim),
  misclassKnn = rep(NA, n_sim), misclassPercKnn = rep(NA, n_sim), Knn_k = rep(NA, n_sim),
  misclassQda = rep(NA, n_sim), misclassPercQda = rep(NA, n_sim),
  misclassSvm = rep(NA, n_sim), misclassPercSvm = rep(NA, n_sim)
)

for (i in 1:n_sim) {  
  
  print(paste('Simulation ', i))
  
  # training and test sets 70-30
  train_ix = sample(length(hubert_data$employment), 0.7 * length(hubert_data$employment))
  
  #################################################################################################
  ####################  SDO #######################################################################
  
  # training and test sets
  train = dist_sdo[train_ix,]
  test = dist_sdo[-train_ix,]
  
  #knn 
  class_result = knn_function(train, test)
  
  # storing the results
  misclassif_sdo = class_result$misclassif
  
  data_results[i, 'misclassSdo'] = misclassif_sdo
  data_results[i, 'misclassPercSdo'] = misclassif_sdo/length(test$class)    
  data_results[i, 'Sdo_k'] = class_result$k_best
  
  #################################################################################################
  ####################  AO  #######################################################################
  
  train = dist_ao[train_ix,]
  test = dist_ao[-train_ix,]
  
  #knn 
  class_result = knn_function(train, test)
  
  # storing the results
  misclassif_ao = class_result$misclassif
  
  data_results[i, 'misclassAo'] = misclassif_ao
  data_results[i, 'misclassPercAo'] = (misclassif_ao)/length(test$class)    
  data_results[i, 'Ao_k'] = class_result$k_best
  
  #################################################################################################
  ####################  bag distance  #############################################################
  
  train = dist_bd[train_ix,]
  test = dist_bd[-train_ix,]
  
  # knn
  class_result = knn_function(train, test)
  
  # storing the results
  misclassif_bd = class_result$misclassif
  
  data_results[i, 'misclassBd'] = misclassif_bd
  data_results[i, 'misclassPercBd'] = (misclassif_bd)/length(test$class)    
  data_results[i, 'Bd_k'] = class_result$k_best
  
  #################################################################################################
  ####################  DO   #######################################################################
  
  train = dist_do[train_ix,]
  test = dist_do[-train_ix,]
  
  # knn
  class_result = knn_function(train, test)
  
  # storing the results
  misclassif_do = class_result$misclassif
  
  data_results[i, 'misclassDo'] = misclassif_do
  data_results[i, 'misclassPercDo'] = (misclassif_do)/length(test$class)    
  data_results[i, 'Do_k'] = class_result$k_best
  
  #################################################################################################
  ###################### KNN  #####################################################################
  
  train = hubert_data[train_ix,]
  test = hubert_data[-train_ix,]
  
  # leaving knn here since it is different from the rest
  accuracy <- rep(0, 10)
  k <- 1:10
  
  for (x in k) {
    prediction <- knn(train[,-12], test[,-12], train[,12]$employment, k = x)
    accuracy[x] <- mean(prediction == test[,12]$employment)
  }
  
  # plot(k, accuracy, type = 'b')
  
  k_best = which.max(accuracy)
  
  knn_class = knn(train[,-12], test[,-12], train[,12]$employment, k = k_best)
  # summary(sdo_class)
  
  knn_results = table(test[,12]$employment, knn_class)
  
  # misclassifications
  misclassif_knn = knn_results[1, 2] + knn_results[2, 1]
  #misclassif_knn/length(test$employment)
  
  data_results[i, 'misclassKnn'] = misclassif_knn
  data_results[i, 'misclassPercKnn'] = (misclassif_knn)/length(test$employment)   
  data_results[i, 'Knn_k'] = k_best
  
  ################### QDA ######################################################
  qda_fit <- qda(employment ~ ., data = train)
  qda_y = predict(qda_fit, newdata = test[,-12])$class
  qda_res = table(test[,12], qda_y)
  misclassQda = qda_res[1, 2] + qda_res[2, 1]
  data_results[i, 'misclassQda'] = misclassQda
  data_results[i, 'misclassPercQda'] = (misclassQda)/length(test$employment)   
  
  ################### SVM ######################################################
  fit_svm = svm(employment ~ ., data = train, type = 'C-classification')
  svm_y = predict(fit_svm, test[, -12])
  svm_res = table(test[,12], svm_y)
  misclassSvm = svm_res[1, 2] + svm_res[2, 1]
  data_results[i, 'misclassSvm'] = misclassSvm
  data_results[i, 'misclassPercSvm'] = (misclassSvm)/length(test$employment)
  
}

beepr::beep(3)

#####################################################################################################
## Plotting Results
#####################################################################################################

# results = data_results %>% dplyr::select(contains("perc"))
# label_vec = c("SDO", "BD", "AO", "DO", "kNN")

results = data_results %>% dplyr::select(contains("perc"))
names(results) = c("SDO", "BD", "AO", "DO", "kNN", "QDA", "SVM")

par(mfrow = c(1,1))
boxplot(results, main = "% Misclassification")

## standard boxplots
par(mfrow = c(1,5))

for (i in 1:7) {
  boxplot(results[[i]], ylim = c(0.05, 0.25))
  mtext(label_vec[i], side = 1, line = 1) # xlab
  mtext("% Misclassification", side = 2, line = 2.4) # ylab
}

par(mfrow = c(1,1))

title_text = paste("Misclassification % Using DistSpace Method (runs = ", n_sim, ")", sep = "", collapse = "")

title(main = title_text, line = 2.6)

## summary of the results
summary(results)

## distance-distance plot
ggplot(dist_bd, aes(x = distG0, y = distG1)) + 
  geom_point(aes(color = class, shape = class), size = 2.6) + 
  xlab("Distance to Unemployed") + ylab("Distance to Employed") + theme_bw() +
  scale_color_manual(name = "Group", labels = c("Unemployed", "Employed"), 
                     values = c("blue", "forestgreen")) + 
  scale_shape_manual(name = "Group", labels = c("Unemployed", "Employed"), values = c(15,17)) +
  theme(legend.position = c(0.15, 0.80))# + xlim(0, 5) + ylim(0,4)


## boxplots in one plot window
boxplot(
   data.frame(data_results$misclassPercSdo, data_results$misclassPercAo, data_results$misclassPercBd, data_results$misclassPercDo, data_results$misclassPercKnn), main = "% Misclassification", 
   names = c("SDO DistSpace","AO DistSpace", "BD DistSpace", "DO DistSpace", "kNN"))

