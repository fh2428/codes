rm(list = ls())
# setwd("C:/Users/frauke/Dropbox/2e/Robust statistics/Project")
setwd("/media/ornela/caa36dee-ee54-44e7-8fa8-872f75e66246/ornela/Dropbox/MSC Statistics/4th semester/RobustStatistics/project/code")
source('DO_code_clean.R')
source('classificationcode.R')
# library(geometry)
library(mrfDepthLight)
library(ggplot2)
library(class)
library(MASS)


n_obs = 500
groups = 2
n_sim = 200

sim_results = data.frame(missclassSdo = rep(NA, n_sim), missclassPercSdo = rep(NA, n_sim), Sdo_k = rep(NA, n_sim),
                         missclassBd = rep(NA, n_sim), missclassPercBd = rep(NA, n_sim), Bd_k = rep(NA, n_sim),
                         missclassAo = rep(NA, n_sim), missclassPercAo = rep(NA, n_sim), Ao_k = rep(NA, n_sim),
                         missclassDo = rep(NA, n_sim), missclassPercDo = rep(NA, n_sim), Do_k = rep(NA, n_sim),
                         missclassKnn = rep(NA, n_sim), missclassPercKnn = rep(NA, n_sim), Knn_k = rep(NA, n_sim))

for (i in 1:n_sim){
    
    print(paste('Simulation ', i))
    
    # contamination intensity 5, 10, 30
    epsilon <- 0.3 # 0 and 0.3 for the ggplot
    
    # generate data for each class
    g1 = mvrnorm(n = n_obs, mu = rep(0, 2), Sigma = matrix(c(1, 0, 0, 1), 2, 2)) 
    g2_1 = rexp(n = n_obs, rate = 1)#x1
    g2_2 = rexp(n = n_obs, rate = 1)#x2
    g2 = cbind(g2_1, g2_2)
    
    g1 = data.frame(x1 = g1[,1], x2 = g1[,2], class = 1)
    g2 = data.frame(x1 = g2_1, x2 = g2_2, class = 2)
    
    if( epsilon != 0)
    {
      # generate outliers for 0, 0.01, 0.05, 0.1
      outliers1 <- sample(1:n_obs, epsilon*n_obs - 10)
      outliers2 <- sample(1:n_obs, epsilon*n_obs - 10)
      out_mislabel <- sample(1:n_obs, 40)
      
      g1[outliers1, 1:2] <- cbind(g1[outliers1, 1] + 5, g1[outliers1, 2])
      g2[outliers2, 1:2] <- cbind(g2[outliers2, 1] - 1, g2[outliers2, 2] - 1)
 
      g1[out_mislabel[1:10], 3] = 2
      g2[out_mislabel[11:20], 3] = 1
    }  

    # combine classes in 1 data frame
    my_data = rbind(g1, g2)
    
    # p <- ggplot(data = my_data, aes(x = x1, y = x2)) + geom_point()
    # p <- p + aes(colour = factor(class), shape = factor(class)) + theme_bw() +
    #   scale_color_manual(name = "Group", labels = c(1,2), values = c("blue", "green")) +
    #   scale_shape_manual(name = "Group", labels = c(1,2), values = c(15,19))
    # p + ggtitle("30% Contamination") # pick the correct title before plot
    
    # divide into train (40%) and test set (60%)
    class1_ix = which(my_data$class == 1)
    class2_ix = which(my_data$class == 2)
    train_ix1 = sample(class1_ix, 0.4*length(class1_ix))
    train_ix2 = sample(class2_ix, 0.4*length(class2_ix))
    train_ix = c(train_ix1, train_ix2)
    
    
#################################################################################################
####################  SDO #######################################################################
    
    sdo1 <- outlyingness(my_data[class1_ix, c(1, 2)], my_data[, c(1, 2)])
    sdo2 <- outlyingness(my_data[class2_ix, c(1, 2)], my_data[, c(1, 2)])
    
    dist_sdo = data.frame(sdoG1 = sdo1$outlyingnessZ, sdoG2 = sdo2$outlyingnessZ)#, sdoG3 = sdo3$outlyingnessZ)
    
    dist_sdo$class = c(rep(1, 500), rep(2, 500))
    
    g = ggplot(data = dist_sdo, aes(x = sdoG1, y = sdoG2)) + geom_point()
    g = g + aes(colour = factor(class))
    g
    
    #knn 
    
    accuracy <- rep(0, 10)
    k <- 1:10
    
    for(x in k){
        prediction <- knn(dist_sdo[train_ix, c(1, 2)], dist_sdo[-train_ix, c(1, 2)],
                          dist_sdo[train_ix, 3], k = x)
        accuracy[x] <- mean(prediction == dist_sdo[-train_ix, 3])
    }
    # plot(k, accuracy, type = 'b')
    
    k_best = which.max(accuracy)
    
    sdo_class = knn(dist_sdo[train_ix, c(1, 2)], dist_sdo[-train_ix, c(1, 2)], dist_sdo[train_ix, 3],
                    k = k_best)
    # summary(sdo_class)
    
    sdo_results = table(dist_sdo[-train_ix, 3], sdo_class)
    
    # miss classifications
    missclassif = sdo_results[1, 2] + sdo_results[2, 1]
    # missclassif
    
    sim_results[i, 'missclassSdo'] = missclassif
    sim_results[i, 'missclassPercSdo'] = (missclassif)/(n_obs*groups - length(train_ix))    
    sim_results[i, 'Sdo_k'] = k_best
  

#################################################################################################
####################  AO  #######################################################################
    
    ao1 <- adjOutlyingness(my_data[class1_ix, c(1, 2)], my_data[, c(1, 2)])
    ao2 <- adjOutlyingness(my_data[class2_ix, c(1, 2)], my_data[, c(1, 2)])
    
    dist_ao = data.frame(aoG1 = ao1$outlyingnessZ, aoG2 = ao2$outlyingnessZ)#, sdoG3 = sdo3$outlyingnessZ)
    
    dist_ao$class = c(rep(1, 500), rep(2, 500))
    
    accuracy <- rep(0, 10)
    k <- 1:10
    
    for(x in k){
        prediction <- knn(dist_ao[train_ix, c(1, 2)], dist_ao[-train_ix, c(1, 2)],
                          dist_ao[train_ix, 3], k = x)
        accuracy[x] <- mean(prediction == dist_ao[-train_ix, 3])
    }
    
    k_best = which.max(accuracy)   
    
    ao_class = knn(dist_ao[train_ix, c(1, 2)], dist_ao[-train_ix, c(1, 2)], dist_ao[train_ix, 3],
                   k = k_best)

    ao_results = table(dist_ao[-train_ix, 3], ao_class)
    
    # miss classifications
    missclassif_ao = ao_results[1, 2] + ao_results[2, 1]
    sim_results[i, 'missclassAo'] = missclassif_ao
    sim_results[i, 'missclassPercAo'] = (missclassif_ao)/(n_obs*groups - length(train_ix))    
    sim_results[i, 'Ao_k'] = k_best
    
#################################################################################################
####################  bag distance  #######################################################################
    
    bd1 <- bagdistance(my_data[class1_ix, c(1, 2)], my_data[, c(1, 2)])
    bd2 <- bagdistance(my_data[class2_ix, c(1, 2)], my_data[, c(1, 2)])
    
    dist_bd = data.frame(bdG1 = bd1$bagdistance, bdG2 = bd2$bagdistance)#, sdoG3 = sdo3$outlyingnessZ)
    
    dist_bd$class = c(rep(1, 500), rep(2, 500))
    
    accuracy <- rep(0, 10)
    k <- 1:10
    
    for(x in k){
        prediction <- knn(dist_bd[train_ix, c(1, 2)], dist_bd[-train_ix, c(1, 2)],
                          dist_bd[train_ix, 3], k = x)
        accuracy[x] <- mean(prediction == dist_bd[-train_ix, 3])
    }
    
    k_best = which.max(accuracy)   
    
    bd_class = knn(dist_bd[train_ix, c(1, 2)], dist_bd[-train_ix, c(1, 2)], dist_bd[train_ix, 3],
                   k = k_best)

    bd_results = table(dist_bd[-train_ix, 3], bd_class)
    
    # miss classifications
    missclassif_bd = bd_results[1, 2] + bd_results[2, 1]
    sim_results[i, 'missclassBd'] = missclassif_bd
    sim_results[i, 'missclassPercBd'] = (missclassif_bd)/(n_obs*groups - length(train_ix))    
    sim_results[i, 'Bd_k'] = k_best
    
#################################################################################################
####################  DO   #######################################################################
  do1 <- DOclassif(data.matrix(my_data[class1_ix, c(1, 2)]), data.matrix(my_data[, c(1, 2)]))
  do2 <- DOclassif(data.matrix(my_data[class2_ix, c(1, 2)]), data.matrix(my_data[, c(1, 2)]))
  
  dist_do = data.frame(doG1 = do1, doG2 = do2)#, sdoG3 = sdo3$outlyingnessZ)
  
  dist_do$class = c(rep(1, 500), rep(2, 500))
  
  accuracy <- rep(0, 10)
  k <- 1:10
  
  for(x in k){
      prediction <- knn(dist_do[train_ix, c(1, 2)], dist_do[-train_ix, c(1, 2)],
                        dist_do[train_ix, 3], k = x)
      accuracy[x] <- mean(prediction == dist_do[-train_ix, 3])
  }
  
  k_best = which.max(accuracy)   
  
  do_class = knn(dist_do[train_ix, c(1, 2)], dist_do[-train_ix, c(1, 2)], dist_do[train_ix, 3],
                 k = k_best)
  
  do_results = table(dist_do[-train_ix, 3], do_class)
  
  # miss classifications
  missclassif_do = do_results[1, 2] + do_results[2, 1]
  sim_results[i, 'missclassDo'] = missclassif_do
  sim_results[i, 'missclassPercDo'] = (missclassif_do)/(n_obs*groups - length(train_ix))    
  sim_results[i, 'Do_k'] = k_best
    
  #################################################################################################
  ###################### KNN  #####################################################################
  accuracy <- rep(0, 10)
  k <- 1:10
  
  for(x in k){
    prediction <- knn(my_data[train_ix, c(1, 2)], my_data[-train_ix, c(1, 2)],
                      my_data[train_ix, 3], k = x)
    accuracy[x] <- mean(prediction == my_data[-train_ix, 3])
  }
  
  k_best = which.max(accuracy)   
  
  knn_class = knn(my_data[train_ix, c(1, 2)], my_data[-train_ix, c(1, 2)], my_data[train_ix, 3],
                 k = k_best)
  
  knn_results = table(my_data[-train_ix, 3], knn_class)
  
  # miss classifications
  missclassif_knn = knn_results[1, 2] + knn_results[2, 1]
  sim_results[i, 'missclassKnn'] = missclassif_knn
  sim_results[i, 'missclassPercKnn'] = (missclassif_knn)/(n_obs*groups - length(train_ix))    
  sim_results[i, 'Knn_k'] = k_best
  
}


#################################################################################################
results = sim_results %>% dplyr::select(contains("perc"))
names(results) = c("SDO", "BD", "AO", "DO", "kNN")

apply(results, 2, median)

par(mfrow = c(1,1))
boxplot(results, main = "% Misclassification")

boxplot(data.frame(sim_results$missclassPercSdo, sim_results$missclassPercAo, sim_results$missclassPercBd, sim_results$missclassPercDo, sim_results$missclassPercKnn),names=c("SDO DistSpace","AO DistSpace", "BD DistSpace", "DO DistSpace", "kNN"), main="% Missclassification")
adjbox(data.frame(sim_results$missclassPercSdo, sim_results$missclassPercAo, sim_results$missclassPercBd, sim_results$missclassPercDo, sim_results$missclassPercKnn),names=c("SDO DistSpace","AO DistSpace", "BD DistSpace", "DO DistSpace", "kNN"), main="% Missclassification")

contamin30 = results
boxplot(contamin30, main = "% Misclassification")
apply(contamin30, 2, median)

no_contamin = results
boxplot(no_contamin, main = "% Misclassification")

contamin10 = results
boxplot(contamin10, main = "% Misclassification")


