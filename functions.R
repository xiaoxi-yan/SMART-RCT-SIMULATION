## INSTALL PACKAGES
#install.packages(c("doParallel","ggplot2","dplyr","latex2exp","reshape2","gridExtra"))
library(doParallel)
registerDoParallel()
library(dplyr); library(ggplot2);
library(latex2exp);library(reshape2);library(gridExtra)
options(scipen=999)

## SMART DATA GENERATION FUNCTION
smart_gen <- function(seed, n, threshold, 
                      Baseline_HBA1c_mean, Baseline_HBA1c_sd,
                      MeanNR1, SDNR1,
                      MeanNR2, SDNR2,
                      MeanR1, SDR1,
                      MeanR2, SDR2,
                      pRespondApp, pRespondNurse, pRespondN_A,  
                      cApp, cNurse,cN_A,
                      switch){
  set.seed(seed)
  # generate the baseline HBA1c
  Baseline_HBA1c <- rnorm(n,Baseline_HBA1c_mean, Baseline_HBA1c_sd)
  Baseline_HBA1c[Baseline_HBA1c<7.8] <- 7.8
  Baseline_HBA1c[Baseline_HBA1c>13] <- 13
  # Stage 1, randomise to treatment1 = {Nurse, App},  Nurse = 0 , App = 1 and receptiveness.
  nApp = floor(n/2) # number of participants in App at stage 1
  nNurse = n - nApp # number of participants in Nurse at stage 1
  mat <- matrix(0,nrow = n,ncol = 2) #indicator to store indicators
  mat[sample(n,nApp),1] <- 1 # randomly allocate to App. 
  mat[mat[,1]==1,2] <- rbinom(nApp, 1, pRespondApp)  # 1 for receptive to app, 0 for not receptive.
  mat[mat[,1]==0,2] <- rbinom(nNurse, 1, pRespondNurse)  # 1 for receptive to nurse, 0 for not receptive.
  AppRecept1_id = which(mat[,1]==1 & mat[,2]==1)
  NurseRecept1_id = which(mat[,1]==0 & mat[,2]==1)
  AppNRecept1_id = which(mat[,1]==1 & mat[,2]==0)
  NurseNRecept1_id = which(mat[,1]==0 & mat[,2]==0)
  
  # Stage 1, intermediate results
  change1_HBA1c <- matrix(0,nrow = n,ncol = 1)
  change1_HBA1c[c(AppRecept1_id,NurseRecept1_id),1] <- rnorm(length(c(AppRecept1_id,NurseRecept1_id)) , MeanR1, SDR1)
  change1_HBA1c[c(AppNRecept1_id,NurseNRecept1_id),1] <- rnorm(length(c(AppNRecept1_id,NurseNRecept1_id)), MeanNR1, SDNR1)
  
  HBA1c_6wks =  Baseline_HBA1c  + change1_HBA1c
  #HBA1c_6wks[HBA1c_6wks<6] <- 6
  # Stage 1, Judged as Responsive
  Response1 <- 1*( (HBA1c_6wks - Baseline_HBA1c) < threshold)
  mat <- cbind(mat,Response1)
  Response1_id = which(Response1==1)
  NResponse1_id = which(Response1==0)
  AppRecept1R_id = AppRecept1_id[AppRecept1_id  %in% Response1_id]
  AppRecept1NR_id = AppRecept1_id[AppRecept1_id  %in% NResponse1_id]
  AppNRecept1R_id = AppNRecept1_id[AppNRecept1_id  %in% Response1_id]
  AppNRecept1NR_id = AppNRecept1_id[AppNRecept1_id  %in% NResponse1_id]
  NurseRecept1R_id = NurseRecept1_id[NurseRecept1_id  %in% Response1_id]
  NurseRecept1NR_id = NurseRecept1_id[NurseRecept1_id  %in% NResponse1_id]
  NurseNRecept1R_id = NurseNRecept1_id[NurseNRecept1_id  %in% Response1_id]
  NurseNRecept1NR_id = NurseNRecept1_id[NurseNRecept1_id  %in% NResponse1_id]
  
  # Stage 2, for responder, stay with trt1, for non-responder, rerandomised to trt2: {Nurse = 0, App = 1, N_A  =2}
  mat <- cbind(mat, mat[,1]) #add a column for second reallocation, using trt1 as base.
  AppRecept1NR_to_Nurse_id <- sample(AppRecept1NR_id,floor(length(AppRecept1NR_id)/2))
  AppRecept1NR_to_N_A_id <-   AppRecept1NR_id[!AppRecept1NR_id %in% AppRecept1NR_to_Nurse_id]
  AppNRecept1NR_to_Nurse_id <- sample(AppNRecept1NR_id,floor(length(AppNRecept1NR_id)/2))  
  AppNRecept1NR_to_N_A_id <- AppNRecept1NR_id[!AppNRecept1NR_id %in% AppNRecept1NR_to_Nurse_id]
  
  NurseRecept1NR_to_App_id <- sample(NurseRecept1NR_id,floor(length(NurseRecept1NR_id)/2))
  NurseRecept1NR_to_N_A_id <-   NurseRecept1NR_id[!NurseRecept1NR_id %in% NurseRecept1NR_to_App_id]
  NurseNRecept1NR_to_App_id <- sample(NurseNRecept1NR_id,floor(length(NurseNRecept1NR_id)/2))  
  NurseNRecept1NR_to_N_A_id <- NurseNRecept1NR_id[!NurseNRecept1NR_id %in% NurseNRecept1NR_to_App_id]
  
  mat[c(AppRecept1NR_to_Nurse_id,AppNRecept1NR_to_Nurse_id) ,4] <- 0
  mat[c(AppRecept1NR_to_N_A_id,AppNRecept1NR_to_N_A_id),4] <- 2
  mat[c(NurseRecept1NR_to_App_id,NurseNRecept1NR_to_App_id),4] <- 1
  mat[c(NurseRecept1NR_to_N_A_id,NurseNRecept1NR_to_N_A_id),4] <- 2
  
  #Stage 2, receptiveness to new trt
  mat <- cbind(mat, mat[,2]) # create a new column for receptiveness at stage 2 based off receptiveness at stage 1 (responders' receptiveness remains the same)
  mat[AppRecept1NR_to_Nurse_id,5] <- rbinom(length(AppRecept1NR_to_Nurse_id),1, pRespondNurse)
  mat[AppRecept1NR_to_N_A_id,5] <-  rbinom(length(AppRecept1NR_to_N_A_id),1, pRespondN_A)
  mat[AppNRecept1NR_to_Nurse_id,5] <- rbinom(length(AppNRecept1NR_to_Nurse_id),1, pRespondNurse)
  mat[AppNRecept1NR_to_N_A_id ,5] <- rbinom(length(AppNRecept1NR_to_N_A_id),1, pRespondN_A)
  
  mat[NurseRecept1NR_to_App_id,5] <- rbinom(length(NurseRecept1NR_to_App_id),1, pRespondApp)
  mat[NurseRecept1NR_to_N_A_id,5] <-  rbinom(length(NurseRecept1NR_to_N_A_id),1, pRespondN_A)
  mat[NurseNRecept1NR_to_App_id,5] <- rbinom(length(NurseNRecept1NR_to_App_id),1, pRespondApp)
  mat[NurseNRecept1NR_to_N_A_id ,5] <- rbinom(length(NurseNRecept1NR_to_N_A_id),1, pRespondN_A)
  
  change2_HBA1c <- matrix(0,nrow = n,ncol = 1)
  change2_HBA1c[which(mat[,5] ==1),1] <- rnorm(length(which(mat[,5] ==1)) , MeanR2, SDR2) # receptive group
  change2_HBA1c[which(mat[,5] ==0),1] <- rnorm(length(which(mat[,5] ==0)) , MeanNR2, SDNR2) # non-receptive group
  
  Final_HBA1c = HBA1c_6wks + change2_HBA1c
  Final_HBA1c[Final_HBA1c<6] <- 6 # turn this off to check threshold
  
  trt1 <- mat[,1]; receptive1<-mat[,2]; Responder1 <- mat[,3]; 
  trt2 <- mat[,4]; receptive2<- mat[,5]
  cost <- (mat[,1]==1)*cApp + (mat[,1]==0)*cNurse +
    (mat[,4]==0)*cNurse + (mat[,4]==1)*cApp + (mat[,4]==2)*cN_A +
    (mat[,1] != mat[,4])*switch   
  grp <- matrix(0,nrow = n,ncol=1)
  grp[trt1==0 & trt2==0,1] <- 1    # Nurse -> R1 -> Nurse under AI1 and 2
  grp[trt1==0 & trt2==1,1] <- 2    # Nurse -> NR1 -> App under AI1
  grp[trt1==0 & trt2==2,1] <- 3    # Nurse -> NR1 -> N_A under AI2
  grp[trt1==1 & trt2==1,1] <- 4    # App -> R1 -> App under AI3 and 4
  grp[trt1==1 & trt2==0,1] <- 5    # App -> R1 -> Nurse under AI3
  grp[trt1==1 & trt2==2,1] <- 6    # App -> R1 -> N_A  under AI4
  
  return( data.frame( Baseline_HBA1c = Baseline_HBA1c,trt1 = trt1, receptive1 = receptive1, 
                 HBA1c_6wks = HBA1c_6wks[,1], Responder1, 
                 trt2 = trt2, receptive2=receptive2, 
                 Final_HBA1c = Final_HBA1c[,1],cost =cost, grp = grp[,1]))
}

## RCT DATA GENERATION FUNCTION
rct_gen <- function(seed, n, threshold,
                      Baseline_HBA1c_mean, Baseline_HBA1c_sd,
                      MeanNR1, SDNR1,
                      MeanNR2, SDNR2,
                      MeanR1, SDR1,
                      MeanR2, SDR2,
                      pRespondApp, pRespondNurse, pRespondN_A,  
                      cApp, cNurse,cN_A,
                      switch){
  set.seed(seed)
  
  # generate the baseline HBA1c
  Baseline_HBA1c <- rnorm(n,Baseline_HBA1c_mean, Baseline_HBA1c_sd)
  Baseline_HBA1c[Baseline_HBA1c<7.8] <- 7.8
  Baseline_HBA1c[Baseline_HBA1c>13] <- 13
  # Randomise to dtr1, dtr2, dtr3, dtr4
  nAi1 = floor(floor(n/2)/2)
  nAi2 = floor(n/2) - nAi1
  nAi3 = floor( (n - (nAi1 + nAi2)) /2)
  nAi4 = (n - (nAi1 + nAi2)) - nAi3
  
  mat <- matrix(0,nrow = n,ncol = 6)  #indicator to store indicators
  colnames(mat) <- c("Ai","trt1","receptive1","Responder1","trt2","receptive2")
  mat[,"Ai"] <- sample(c(rep(1, nAi1),rep(2, nAi2),rep(3, nAi3),rep(4, nAi4)),n,replace=F) # randomly allocate treatment strategy
  ### ids of the Ais
  Ai1_id <- which(mat[,"Ai"]==1)
  Ai2_id <- which(mat[,"Ai"]==2)
  Ai3_id <- which(mat[,"Ai"]==3)
  Ai4_id <- which(mat[,"Ai"]==4)
  mat[c(Ai3_id,Ai4_id),"trt1"] <- 1    #indicator for App as treatment1
  
  mat[Ai1_id, "receptive1"] <- rbinom(nAi1, 1, pRespondNurse)  # 1 for receptive to nurse, 0 for not receptive.
  mat[Ai2_id, "receptive1"] <- rbinom(nAi2, 1, pRespondNurse)  # 1 for receptive to nurse, 0 for not receptive.
  mat[Ai3_id, "receptive1"] <- rbinom(nAi3, 1, pRespondApp)    # 1 for receptive to app, 0 for not receptive.
  mat[Ai4_id, "receptive1"] <- rbinom(nAi4, 1, pRespondApp)    # 1 for receptive to app, 0 for not receptive.
  
  # Stage 1, intermediate results
  change1_HBA1c <- matrix(0,nrow = n,ncol = 1)
  change1_HBA1c[mat[,"receptive1"]==1,1] <- rnorm(sum(mat[,"receptive1"]==1) , MeanR1, SDR1)  # receptive to treatment 1
  change1_HBA1c[mat[,"receptive1"]==0,1] <- rnorm(sum(mat[,"receptive1"]==0), MeanNR1, SDNR1) # non-receptive to treatment 1
  HBA1c_6wks =  Baseline_HBA1c  + change1_HBA1c
  #HBA1c_6wks[HBA1c_6wks<6] <- 6# turn this off to check threshold
  # Stage 1, Judged as Responsive
  Response1 <- 1*( (HBA1c_6wks - Baseline_HBA1c) < threshold) 
  mat[ ,"Responder1"]  <- Response1
  
  ### Stage 2, for responder, stay with trt1, for non-responder, trt2 according to dtr {Nurse = 0, App = 1, N_A  =2}
  mat[Ai1_id,"trt2"] <- 0^Response1[Ai1_id] * 1^(1-Response1[Ai1_id])    #indicator of treatment 2 according to dtr, DTR1 R -> Nurse, NR -> App
  mat[Ai2_id,"trt2"] <- 0^Response1[Ai2_id] * 2^(1-Response1[Ai2_id]) 
  mat[Ai3_id,"trt2"] <- 1^Response1[Ai3_id] * 0^(1-Response1[Ai3_id])   
  mat[Ai4_id,"trt2"] <- 1^Response1[Ai4_id] * 2^(1-Response1[Ai4_id]) 
  
  #Stage 2, receptiveness to new trt if available
  mat[,"receptive2"] <- mat[,"receptive1"]               #for receptiveness at stage 2 based off receptiveness at stage 1 (responders' receptiveness remains the same)
  Ai1NR_id <- which(mat[,"Ai"]==1 & mat[,"Responder1"] == 0)
  Ai2NR_id <- which(mat[,"Ai"]==2 & mat[,"Responder1"] == 0)
  Ai3NR_id <- which(mat[,"Ai"]==3 & mat[,"Responder1"] == 0)
  Ai4NR_id <- which(mat[,"Ai"]==4 & mat[,"Responder1"] == 0)
  
  mat[Ai1NR_id,"receptive2"] <- rbinom(length(Ai1NR_id), 1, pRespondApp)       # Ai1 non-responders receptiveness to new treatment = app
  mat[Ai2NR_id,"receptive2"] <- rbinom(length(Ai2NR_id), 1, pRespondN_A)       # Ai1 non-responders receptiveness to new treatment = app
  mat[Ai3NR_id,"receptive2"] <- rbinom(length(Ai3NR_id), 1, pRespondNurse)     # Ai1 non-responders receptiveness to new treatment = app
  mat[Ai4NR_id,"receptive2"] <- rbinom(length(Ai4NR_id), 1, pRespondN_A)       # Ai1 non-responders receptiveness to new treatment = app
  
  change2_HBA1c <- matrix(0,nrow = n,ncol = 1)
  change2_HBA1c[mat[,"receptive2"] ==1,1] <- rnorm(sum(mat[,"receptive2"] ==1) , MeanR2, SDR2) # receptive group
  change2_HBA1c[mat[,"receptive2"] ==0,1] <- rnorm(sum(mat[,"receptive2"] ==0), MeanNR2, SDNR2) # non-receptive group
  
  Final_HBA1c = HBA1c_6wks+ change2_HBA1c
  Final_HBA1c[Final_HBA1c<6] <- 6
  
  Ai<- mat[,"Ai"]; receptive1<-mat[,"receptive1"]; Responder1 <- mat[,"Responder1"]; 
  receptive2<- mat[,"receptive2"]
  cost<- (mat[,"trt1"]==1)*cApp + (mat[,"trt1"]==0)*cNurse +
    (mat[,"trt2"]==0)*cNurse + (mat[,"trt2"]==1)*cApp + (mat[,"trt2"]==2)*cN_A +
    (mat[,"trt1"] != mat[,"trt2"])*switch   

  return( data.frame( Baseline_HBA1c = Baseline_HBA1c, receptive1 = receptive1, 
                 HBA1c_6wks = HBA1c_6wks[,1], Responder1, 
                 receptive2=receptive2, 
                 Final_HBA1c = Final_HBA1c[,1],cost =cost,Ai= Ai))
}

#IPW mean for SMART DTR
fun_mean.dt<-function(y_dt, R, p.trt1, p.trt2.NR){  
  # assume probability of responder p.trt2.R = 1
  # y_dt the vector of outcomes for the dtr of format (A,A^R(B)^(1-R))
  # R  the vector of responder indicator for the dtr.
  Yi = y_dt
  Wi=1/(p.trt1*(1*R + p.trt2.NR *(1-R)))
  y.dt.bar = sum(Yi*Wi)/sum(Wi)
  return(y.dt.bar)
}




