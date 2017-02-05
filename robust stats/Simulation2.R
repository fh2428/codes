rm(list = ls())
setwd("/media/ornela/caa36dee-ee54-44e7-8fa8-872f75e66246/ornela/Dropbox/MSC Statistics/4th semester/RobustStatistics/project/code")

source('DO_code_clean.R')
source('classificationcode.R')
library(mrfDepthLight)
library(ggplot2)
library(class)
library(MASS)


n_obs = 500
groups = 3
n_sim = 200

sim_results = data.frame(missclassSdo = rep(NA, n_sim), missclassPercSdo = rep(NA, n_sim), Sdo_k = rep(NA, n_sim),
                         missclassBd = rep(NA, n_sim), missclassPercBd = rep(NA, n_sim), Bd_k = rep(NA, n_sim),
                         missclassAo = rep(NA, n_sim), missclassPercAo = rep(NA, n_sim), Ao_k = rep(NA, n_sim),
                         missclassDo = rep(NA, n_sim), missclassPercDo = rep(NA, n_sim), Do_k = rep(NA, n_sim),
                         missclassKnn = rep(NA, n_sim), missclassPercKnn = rep(NA, n_sim), Knn_k = rep(NA, n_sim))

for (i in 1:n_sim){
    
    print(paste('Simulation ', i))
    
    # contamination intensity 5, 10, 30
    epsilon <- 0 # 0 and 0.3 for the ggplot
    
    # generate data for each class
    d1 = rgamma(n_obs, 2, 2)
    d2 = rgamma(n_obs, 3, 2)
    
    d = cbind(d1, d2)
    
    g1 = data.frame(x1 = d[,1], x2 = d[,2])
    g1$class = 1
    g2 = data.frame(x1 = -d[,1] + 0.5, x2 = d[,2])
    g2$class = 2
    g3 = data.frame(x1 = d[,1] + 2, x2 = d[,2] + 1)
    g3$class = 3
    
    if(epsilon != 0)
    {
      # generate outliers for 0, 0.01, 0.05, 0.1
      outliers1 <- sample(1:n_obs, round(epsilon*n_obs - 10))
      outliers2 <- sample(1:n_obs, round(epsilon*n_obs - 10))
      outliers3 <- sample(1:n_obs, round(epsilon*n_obs - 10))
      out_mislabel <- sample(1:n_obs, 30)
      
      g1[outliers1, 1:2] <- cbind(g1[outliers1, 1] + 5, g1[outliers1, 2] + 5) #contaminate x1 and x2
      g2[outliers2, 1:2] <- cbind(g2[outliers2, 1] + 5, g2[outliers2, 2] + 1)
      g3[outliers3, 1:2] <- cbind(g3[outliers3, 1] + 5, g3[outliers3, 2] + 5)

      g1[out_mislabel[1:10], 3] = sample(c(2, 3), 10, replace = T)
      g2[out_mislabel[11:20], 3] = sample(c(1, 3), 10, replace = T)
      g3[out_mislabel[21:30], 3] = sample(c(1, 2), 10, replace = T)
      
    }  

    # combine classes in 1 data frame
    my_data = rbind(g1, g2, g3)
    
    # q <- ggplot(data = my_data, aes(x = x1, y = x2)) + geom_point()
    # q <- q + aes(colour = factor(class), shape = factor(class)) + theme_bw() + 
    #   scale_color_manual(name = "Group", labels = c(1:3), values = c("blue", "green", "red")) + 
    #   scale_shape_manual(name = "Group", labels = c(1:3), values = c(15,17,19))
    # q + ggtitle("0% Contamination") # pick contamination title
    
    # divide into train (40%) and test set (60%)
    class1_ix = which(my_data$class == 1)
    class2_ix = which(my_data$class == 2)
    class3_ix = which(my_data$class == 3)
    train_ix1 = sample(class1_ix, 0.4*length(class1_ix))
    train_ix2 = sample(class2_ix, 0.4*length(class2_ix))
    train_ix3 = sample(class3_ix, 0.4*length(class3_ix))
    train_ix = c(train_ix1, train_ix2, train_ix3)
    
    
#################################################################################################
####################  SDO #######################################################################
    
    #SDO to group 1
    sdo11 <- outlyingness(my_data[class1_ix, c(1, 2)], my_data[class1_ix, c(1, 2)])
    sdo11 = data.frame(sdo = sdo11$outlyingnessZ)
    sdo11$class = 1
    
    sdo12 <- outlyingness(my_data[class1_ix, c(1, 2)], my_data[class2_ix, c(1, 2)])
    sdo12 = data.frame(sdo = sdo12$outlyingnessZ)
    sdo12$class = 2
    
    sdo13 <- outlyingness(my_data[class1_ix, c(1, 2)], my_data[class3_ix, c(1, 2)])
    sdo13 = data.frame(sdo = sdo13$outlyingnessZ)
    sdo13$class = 3
    
    sdo1 <- rbind(sdo11, sdo12, sdo13)
  
    #SDO to group 2
    sdo21 <- outlyingness(my_data[class2_ix, c(1, 2)], my_data[class1_ix, c(1, 2)])
    sdo21 = data.frame(sdo = sdo21$outlyingnessZ)
    sdo21$class = 1
    
    sdo22 <- outlyingness(my_data[class2_ix, c(1, 2)], my_data[class2_ix, c(1, 2)])
    sdo22 = data.frame(sdo = sdo22$outlyingnessZ)
    sdo22$class = 2
    
    sdo23 <- outlyingness(my_data[class2_ix, c(1, 2)], my_data[class3_ix, c(1, 2)])
    sdo23 = data.frame(sdo = sdo23$outlyingnessZ)
    sdo23$class = 3
    
    sdo2 <- rbind(sdo21, sdo22, sdo23)
    
    #SDO to group 3
    sdo31 <- outlyingness(my_data[class3_ix, c(1, 2)], my_data[class1_ix, c(1, 2)])
    sdo31 = data.frame(sdo = sdo31$outlyingnessZ)
    sdo31$class = 1
    
    sdo32 <- outlyingness(my_data[class3_ix, c(1, 2)], my_data[class2_ix, c(1, 2)])
    sdo32 = data.frame(sdo = sdo32$outlyingnessZ)
    sdo32$class = 2
    
    sdo33 <- outlyingness(my_data[class3_ix, c(1, 2)], my_data[class3_ix, c(1, 2)])
    sdo33 = data.frame(sdo = sdo33$outlyingnessZ)
    sdo33$class = 3
    
    sdo3 <- rbind(sdo31, sdo32, sdo33)
    
    dist_sdo = data.frame(sdoG1 = sdo1$sdo, sdoG2 = sdo2$sdo, sdoG3 = sdo3$sdo, class = sdo1$class)
  
    #knn 
    
    accuracy <- rep(0, 10)
    k <- 1:10
    
    for(x in k){
        prediction <- knn(dist_sdo[train_ix, 1:3], dist_sdo[-train_ix, 1:3],
                          dist_sdo[train_ix, 4], k = x)
        accuracy[x] <- mean(prediction == dist_sdo[-train_ix, 4])
    }
    # plot(k, accuracy, type = 'b')
    
    k_best = which.max(accuracy)
    
    sdo_class = knn(dist_sdo[train_ix, 1:3], dist_sdo[-train_ix, 1:3],
                    dist_sdo[train_ix, 4], k = k_best)
    
    sdo_results = table(dist_sdo[-train_ix, 4], sdo_class)
    
    # miss classifications
    missclassif = sum(sdo_results[1, - 1]) + sum(sdo_results[2, - 2]) + sum(sdo_results[3, - 3])
    # missclassif
    
    sim_results[i, 'missclassSdo'] = missclassif
    sim_results[i, 'missclassPercSdo'] = (missclassif)/(n_obs*groups - length(train_ix))    
    sim_results[i, 'Sdo_k'] = k_best
    
#################################################################################################
####################  Ao #######################################################################
    
    #AO to group 1
    ao11 <- adjOutlyingness(my_data[class1_ix, c(1, 2)], my_data[class1_ix, c(1, 2)])
    ao11 = data.frame(ao = ao11$outlyingnessZ)
    ao11$class = 1
    
    ao12 <- adjOutlyingness(my_data[class1_ix, c(1, 2)], my_data[class2_ix, c(1, 2)])
    ao12 = data.frame(ao = ao12$outlyingnessZ)
    ao12$class = 2
    
    ao13 <- adjOutlyingness(my_data[class1_ix, c(1, 2)], my_data[class3_ix, c(1, 2)])
    ao13 = data.frame(ao = ao13$outlyingnessZ)
    ao13$class = 3
    
    ao1 <- rbind(ao11, ao12, ao13)
    
    #AO to group 2
    ao21 <- adjOutlyingness(my_data[class2_ix, c(1, 2)], my_data[class1_ix, c(1, 2)])
    ao21 = data.frame(ao = ao21$outlyingnessZ)
    ao21$class = 1
    
    ao22 <- adjOutlyingness(my_data[class2_ix, c(1, 2)], my_data[class2_ix, c(1, 2)])
    ao22 = data.frame(ao = ao22$outlyingnessZ)
    ao22$class = 2
    
    ao23 <- adjOutlyingness(my_data[class2_ix, c(1, 2)], my_data[class3_ix, c(1, 2)])
    ao23 = data.frame(ao = ao23$outlyingnessZ)
    ao23$class = 3
    
    ao2 <- rbind(ao21, ao22, ao23)
    
    #AO to group 3
    ao31 <- adjOutlyingness(my_data[class3_ix, c(1, 2)], my_data[class1_ix, c(1, 2)])
    ao31 = data.frame(ao = ao31$outlyingnessZ)
    ao31$class = 1
    
    ao32 <- adjOutlyingness(my_data[class3_ix, c(1, 2)], my_data[class2_ix, c(1, 2)])
    ao32 = data.frame(ao = ao32$outlyingnessZ)
    ao32$class = 2
    
    ao33 <- adjOutlyingness(my_data[class3_ix, c(1, 2)], my_data[class3_ix, c(1, 2)])
    ao33 = data.frame(ao = ao33$outlyingnessZ)
    ao33$class = 3
    
    ao3 <- rbind(ao31, ao32, ao33)
    
    dist_ao = data.frame(aoG1 = ao1$ao, aoG2 = ao2$ao, aoG3 = ao3$ao, class = ao1$class)
    
    #knn 
    
    accuracy <- rep(0, 10)
    k <- 1:10
    
    for(x in k){
      prediction <- knn(dist_ao[train_ix, 1:3], dist_ao[-train_ix, 1:3],
                        dist_ao[train_ix, 4], k = x)
      accuracy[x] <- mean(prediction == dist_ao[-train_ix, 4])
    }
    # plot(k, accuracy, type = 'b')
    
    k_best = which.max(accuracy)
    
    ao_class = knn(dist_ao[train_ix, 1:3], dist_ao[-train_ix, 1:3],
                    dist_ao[train_ix, 4], k = k_best)
    
    ao_results = table(dist_ao[-train_ix, 4], ao_class)
    
    # miss classifications
    missclassif = sum(ao_results[1, - 1]) + sum(ao_results[2, - 2]) + sum(ao_results[3, - 3])
    # missclassif
    
    sim_results[i, 'missclassAo'] = missclassif
    sim_results[i, 'missclassPercAo'] = (missclassif)/(n_obs*groups - length(train_ix))    
    sim_results[i, 'Ao_k'] = k_best
    
#################################################################################################
####################  Bag Distance  #############################################################

    #BD to group 1
    bd11 <- bagdistance(my_data[class1_ix, c(1, 2)], my_data[class1_ix, c(1, 2)])
    bd11 = data.frame(bd = bd11$bagdistance)
    bd11$class = 1
    
    bd12 <- bagdistance(my_data[class1_ix, c(1, 2)], my_data[class2_ix, c(1, 2)])
    bd12 = data.frame(bd = bd12$bagdistance)
    bd12$class = 2
    
    bd13 <- bagdistance(my_data[class1_ix, c(1, 2)], my_data[class3_ix, c(1, 2)])
    bd13 = data.frame(bd = bd13$bagdistance)
    bd13$class = 3
    
    bd1 <- rbind(bd11, bd12, bd13)
    
    #BD to group 2
    bd21 <- bagdistance(my_data[class2_ix, c(1, 2)], my_data[class1_ix, c(1, 2)])
    bd21 = data.frame(bd = bd21$bagdistance)
    bd21$class = 1
    
    bd22 <- bagdistance(my_data[class2_ix, c(1, 2)], my_data[class2_ix, c(1, 2)])
    bd22 = data.frame(bd = bd22$bagdistance)
    bd22$class = 2
    
    bd23 <- bagdistance(my_data[class2_ix, c(1, 2)], my_data[class3_ix, c(1, 2)])
    bd23 = data.frame(bd = bd23$bagdistance)
    bd23$class = 3
    
    bd2 <- rbind(bd21, bd22, bd23)
    
    #BD to group 3
    bd31 <- bagdistance(my_data[class3_ix, c(1, 2)], my_data[class1_ix, c(1, 2)])
    bd31 = data.frame(bd = bd31$bagdistance)
    bd31$class = 1
    
    bd32 <- bagdistance(my_data[class3_ix, c(1, 2)], my_data[class2_ix, c(1, 2)])
    bd32 = data.frame(bd = bd32$bagdistance)
    bd32$class = 2
    
    bd33 <- bagdistance(my_data[class3_ix, c(1, 2)], my_data[class3_ix, c(1, 2)])
    bd33 = data.frame(bd = bd33$bagdistance)
    bd33$class = 3
    
    bd3 <- rbind(bd31, bd32, bd33)
    
    dist_bd = data.frame(bdG1 = bd1$bd, bdG2 = bd2$bd, bdG3 = bd3$bd, class = bd1$class)
    
    #knn 
    
    accuracy <- rep(0, 10)
    k <- 1:10
    
    for(x in k){
      prediction <- knn(dist_bd[train_ix, 1:3], dist_bd[-train_ix, 1:3],
                        dist_bd[train_ix, 4], k = x)
      accuracy[x] <- mean(prediction == dist_bd[-train_ix, 4])
    }
    # plot(k, accuracy, type = 'b')
    
    k_best = which.max(accuracy)
    
    bd_class = knn(dist_bd[train_ix, 1:3], dist_bd[-train_ix, 1:3],
                   dist_bd[train_ix, 4], k = k_best)
    
    bd_results = table(dist_bd[-train_ix, 4], bd_class)
    
    # miss classifications
    missclassif = sum(bd_results[1, - 1]) + sum(bd_results[2, - 2]) + sum(bd_results[3, - 3])
    # missclassif
    
    sim_results[i, 'missclassBd'] = missclassif
    sim_results[i, 'missclassPercBd'] = (missclassif)/(n_obs*groups - length(train_ix))    
    sim_results[i, 'Bd_k'] = k_best
    
    #################################################################################################
    ####################  DO  #######################################################################
    
    #DO to group 1
    do11 <- DOclassif(data.matrix(my_data[class1_ix, c(1, 2)]), data.matrix(my_data[class1_ix, c(1, 2)]))
    do11 = data.frame(do = do11)
    do11$class = 1
    
    do12 <- DOclassif(data.matrix(my_data[class1_ix, c(1, 2)]), data.matrix(my_data[class2_ix, c(1, 2)]))
    do12 = data.frame(do = do12)
    do12$class = 2
    
    do13 <- DOclassif(data.matrix(my_data[class1_ix, c(1, 2)]), data.matrix(my_data[class3_ix, c(1, 2)]))
    do13 = data.frame(do = do13)
    do13$class = 3
    
    do1 <- rbind(do11, do12, do13)
    
    #DO to group 2
    do21 <- DOclassif(data.matrix(my_data[class2_ix, c(1, 2)]), data.matrix(my_data[class1_ix, c(1, 2)]))
    do21 = data.frame(do = do21)
    do21$class = 1
    
    do22 <- DOclassif(data.matrix(my_data[class2_ix, c(1, 2)]), data.matrix(my_data[class2_ix, c(1, 2)]))
    do22 = data.frame(do = do22)
    do22$class = 2
    
    do23 <- DOclassif(data.matrix(my_data[class2_ix, c(1, 2)]), data.matrix(my_data[class3_ix, c(1, 2)]))
    do23 = data.frame(do = do23)
    do23$class = 3
    
    do2 <- rbind(do21, do22, do23)
    
    #DO to group 3
    do31 <- DOclassif(data.matrix(my_data[class3_ix, c(1, 2)]), data.matrix(my_data[class1_ix, c(1, 2)]))
    do31 = data.frame(do = do31)
    do31$class = 1
    
    do32 <- DOclassif(data.matrix(my_data[class3_ix, c(1, 2)]), data.matrix(my_data[class2_ix, c(1, 2)]))
    do32 = data.frame(do = do32)
    do32$class = 2
    
    do33 <- DOclassif(data.matrix(my_data[class3_ix, c(1, 2)]), data.matrix(my_data[class3_ix, c(1, 2)]))
    do33 = data.frame(do = do33)
    do33$class = 3
    
    do3 <- rbind(do31, do32, do33)
    
    dist_do = data.frame(doG1 = do1$do, doG2 = do2$do, doG3 = do3$do, class = do1$class)
    
    #knn 
    
    accuracy <- rep(0, 10)
    k <- 1:10
    
    for(x in k){
      prediction <- knn(dist_do[train_ix, 1:3], dist_do[-train_ix, 1:3],
                        dist_do[train_ix, 4], k = x)
      accuracy[x] <- mean(prediction == dist_do[-train_ix, 4])
    }
    # plot(k, accuracy, type = 'b')
    
    k_best = which.max(accuracy)
    
    do_class = knn(dist_do[train_ix, 1:3], dist_do[-train_ix, 1:3],
                   dist_do[train_ix, 4], k = k_best)
    
    do_results = table(dist_do[-train_ix, 4], do_class)
    
    # miss classifications
    missclassif = sum(do_results[1, - 1]) + sum(do_results[2, - 2]) + sum(do_results[3, - 3])
    # missclassif
    
    sim_results[i, 'missclassDo'] = missclassif
    sim_results[i, 'missclassPercDo'] = (missclassif)/(n_obs*groups - length(train_ix))    
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
  missclassif_knn = sum(knn_results[1, - 1]) + sum(knn_results[2, - 2]) + sum(knn_results[3, - 3])
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
