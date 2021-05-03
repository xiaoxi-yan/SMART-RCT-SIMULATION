source("functions.R")
if(!dir.exists("output"))
  dir.create("output")
dir1 = "output/"

#### 
### 
# set.seed(12345)
# x = rnorm(10000, 9.73,1.37)
# x <-ifelse(x<7.8,7.8,x)
# x <-ifelse(x>13,13,x)
# hist(x, breaks= 150, main ="Histogram of baseline HbA1c", xlab= "baseline HbA1c", xaxt='n',xlim = c(7.8,13))
# axis(side=1, at=seq(7.8,13,0.5))
# 
# abline(v=mean(x), col = "red")

######## INPUT VARIABLES ######################
#HBA1c value parameters
Baseline_HBA1c_mean = 9.73  #Y_0 
Baseline_HBA1c_sd =   1.37  

MeanNR1 = 0; SDNR1 = 0.71  #mean and sd of change in HBA1c at week 6 for non-receptive subjects
MeanNR2 = 0; SDNR2 = 0.77  #mean and sd of change in HBA1c at week 12 for non-receptive subjects

MeanR1 = -1.53 ; SDR1 = 0.71 #mean and sd of change in HBA1c at week 6 for receptive subjects
MeanR2 = -0.94; SDR2 = 0.77  #mean and sd af change in HBA1c at week 12 for receptive subjects

# noting that receptiveness =/= responder by design.
# design parameters
threshold = -0.5           ##\delta, subjects with change in HbA1c at 6 weeks less than threshold is a responder for stage 2. 


###### receptiveness rate to treatments P(R_C_N=1),P(R_C_A=1),P(R_C_A+N=1)
pRespondApp = 0.51; pRespondNurse = 0.69;
# A+N receptiveness probability given not receptive to App
P1 = (1-pRespondApp)+ pRespondNurse -  (1-pRespondApp)*pRespondNurse
# A+N receptiveness probability given not receptive to Nurse
P2 = (1-pRespondNurse)+ pRespondApp -  (1-pRespondNurse)*pRespondApp
##Average
pRespondN_A = round((P1+P2)/2,2)  
# cost 
exchange_rate = 1.267
cApp = (75+75+48)/exchange_rate
cNurse = (75+94+48)/exchange_rate
cN_A = (75+94+48)/exchange_rate
switch = 50/exchange_rate
###
nsimul = 10000


#####################################
##
if(!dir.exists(paste0(dir1,"mean")))
  dir.create(paste0(dir1,"mean"))
if(!dir.exists(paste0(dir1,"best_AI")))
  dir.create(paste0(dir1,"best_AI"))

# sample size, assuming equal ranodmisation 

n_list  = c(50,100,200,300,400,500,600,700,800,900,1000,1100,1300,1500,1700)

### iterate for each n sample sizes
for(j in 1: length(n_list)){
  n = n_list[j];
  ################## VARIANCE/MEAN/BEST SMART OUTPUT FILES  ##############
  smart_output <-foreach(i=1:nsimul, .combine = rbind ) %dopar% {
    seed = 1237974 + i + threshold*nsimul + n*nsimul
    smart_dat <- smart_gen(seed, n, threshold,Baseline_HBA1c_mean, Baseline_HBA1c_sd,
                           MeanNR1, SDNR1,MeanNR2, SDNR2,MeanR1, SDR1,MeanR2, SDR2,
                           pRespondApp, pRespondNurse, pRespondN_A,
                           cApp, cNurse,cN_A,switch)
    
    ### overall
    HBA1c_12 = mean(smart_dat[,"Final_HBA1c"])
    Cost_subject= mean(smart_dat[,"cost"])
    temp = smart_dat[, c("trt1","Responder1")]
    Rsp_A = sum(temp[temp[,1]==1,2])/sum(temp[,1]==1)
    Rsp_N = sum(temp[temp[,1]==0,2])/sum(temp[,1]==0)
    
    ### AI
    ngrp1 <- sum(smart_dat[,"grp"]==1)    # Nurse -> R1 -> Nurse under AI1 and 2
    ngrp2 <- sum(smart_dat[,"grp"]==2)      # Nurse -> NR1 -> App under AI1
    ngrp3 <- sum(smart_dat[,"grp"]==3)     # Nurse -> NR1 -> N_A under AI2
    ngrp4 <- sum(smart_dat[,"grp"]==4)     # App -> R1 -> App under AI3 and 4
    ngrp5 <- sum(smart_dat[,"grp"]==5)    # App -> R1 -> Nurse under AI3
    ngrp6 <- sum(smart_dat[,"grp"]==6)     # App -> R1 -> N_A  under AI4
    
    ai1_id<- which(smart_dat[,"grp"] %in% c(1,2))
    ai2_id<-  which(smart_dat[,"grp"] %in% c(1,3))
    ai3_id<-  which(smart_dat[,"grp"] %in% c(4,5))
    ai4_id<- which(smart_dat[,"grp"] %in% c(4,6))
    
    p.trt1_Nurse = (ngrp1+ngrp2+ngrp3)/nrow(smart_dat)
    p.trt1_App = 1- p.trt1_Nurse
    
    Ai1_HBA1c_12_a <-  with(smart_dat[ai1_id,],fun_mean.dt(Final_HBA1c , Responder1, p.trt1_Nurse, ngrp2/(ngrp2+ngrp3)))
    Ai2_HBA1c_12_a <-  with(smart_dat[ai2_id,],fun_mean.dt(Final_HBA1c , Responder1, p.trt1_Nurse, ngrp3/(ngrp2+ngrp3)))
    Ai3_HBA1c_12_a <-  with(smart_dat[ai3_id,],fun_mean.dt(Final_HBA1c , Responder1, p.trt1_App, ngrp5/(ngrp5+ngrp6)))
    Ai4_HBA1c_12_a <-  with(smart_dat[ai4_id,],fun_mean.dt(Final_HBA1c , Responder1, p.trt1_App, ngrp6/(ngrp5+ngrp6)))
    
    
    return(cbind(HBA1c_12, Cost_subject ,Rsp_N,Rsp_A,
                 Ai1_HBA1c_12=Ai1_HBA1c_12_a,Ai2_HBA1c_12=Ai2_HBA1c_12_a,Ai3_HBA1c_12=Ai3_HBA1c_12_a,Ai4_HBA1c_12=Ai4_HBA1c_12_a))
  }
  
  output1 <- rbind(colMeans(smart_output,na.rm=T), apply(smart_output, 2, function(x) sd(x,na.rm=T)))
  names(output1) <- c("MC Mean", "MC SD")
  
  write.csv(output1, paste0(dir1,"mean/smart_",n,".csv"), row.names=FALSE)
  
  best_smart_AI<- unlist(apply(smart_output[,5:8], 1, function(x) which(x== min(x))))
  write.csv(best_smart_AI, paste0(dir1,"best_AI/smart_",n,".csv"), row.names=FALSE)
  
  ################ MEAN/BEST AI RCT OUTPUT FILES  ##############
  rct_output <-foreach(i=1:nsimul, .combine = rbind ) %dopar% {
    seed = 1244534 + i + threshold*nsimul + n*nsimul
    rct_dat <- rct_gen(seed, n, threshold,Baseline_HBA1c_mean, Baseline_HBA1c_sd,
                       MeanNR1, SDNR1,MeanNR2, SDNR2,MeanR1, SDR1,MeanR2, SDR2,
                       pRespondApp, pRespondNurse, pRespondN_A,
                       cApp, cNurse,cN_A,switch)
    
    ### overall
    HBA1c_12 = mean(rct_dat[,"Final_HBA1c"])
    Cost_subject= mean(rct_dat[,"cost"])
    temp = rct_dat[, c("Responder1","Ai")]
    Rsp_Ai1 = with(rct_dat[rct_dat$Ai==1,], sum(Responder1)/length(Responder1))
    Rsp_Ai2 = with(rct_dat[rct_dat$Ai==2,], sum(Responder1)/length(Responder1))
    Rsp_Ai3 = with(rct_dat[rct_dat$Ai==3,], sum(Responder1)/length(Responder1))
    Rsp_Ai4 = with(rct_dat[rct_dat$Ai==4,], sum(Responder1)/length(Responder1))
    
    ### AI
    Ai1_HBA1c_12 <- mean(rct_dat[rct_dat$Ai==1,"Final_HBA1c"],na.rm=T)
    Ai2_HBA1c_12 <-  mean(rct_dat[rct_dat$Ai==2,"Final_HBA1c"],na.rm=T)
    Ai3_HBA1c_12 <- mean(rct_dat[rct_dat$Ai==3,"Final_HBA1c"],na.rm=T)
    Ai4_HBA1c_12 <-  mean(rct_dat[rct_dat$Ai==4,"Final_HBA1c"],na.rm=T)
    
    return(cbind(HBA1c_12, Cost_subject ,Rsp_Ai1,Rsp_Ai2,Rsp_Ai3,Rsp_Ai4,
                 Ai1_HBA1c_12,Ai2_HBA1c_12,Ai3_HBA1c_12,Ai4_HBA1c_12))
  }
  
  output2 <- rbind(colMeans(rct_output[,1:10],na.rm=T),
                   apply(rct_output[,1:10], 2, function(x) sd(x,na.rm=T)))
  
  names(output2) <- c("MC Mean", "MC SD")
  write.csv(output2, paste0(dir1,"mean/rct_",n,".csv"), row.names=FALSE)
  best_rct_AI<- unlist(apply(rct_output[,7:10], 1, function(x) which(x== min(x))))
  write.csv(best_rct_AI, paste0(dir1,"best_AI/rct_",n,".csv"), row.names=FALSE)
  
  remove(list=c("smart_output","rct_output","output1","output2","best_smart_AI","best_rct_AI",
                "n"))
}

######### END SIMULATION

########## TABLE 2 #################################
### output for Table 2 
smart_100 = read.csv(paste0(dir1,"mean/smart_100.csv"))
rct_100 = read.csv(paste0(dir1,"mean/rct_100.csv"))
smart_AI_100 = read.csv(paste0(dir1,"best_AI/smart_100.csv"))
rct_AI_100 = read.csv(paste0(dir1,"best_AI/rct_100.csv"))

table2<- cbind(t(smart_100[,c(1,2,5,6,7,8)]),t(rct_100[,c(1,2,7,8,9,10)]))
pOpt_smart =  matrix(prop.table(table(smart_AI_100)),ncol=1)
pOpt_rct = matrix(prop.table(table(rct_AI_100)), ncol =1)
table2 <- rbind(table2,cbind(pOpt_smart,NA, pOpt_rct,NA))
colnames(table2) <- c("SMART_MC_mean","SMART_MC_sd", "RCT_MC_mean","SMART_MC_sd")
rownames(table2) <- c("Y","Cost(USD)","Y_AI1","Y_AI2","Y_AI3","Y_AI4","pOptAI1","pOptAI2","pOptAI3","pOptAI4")
write.csv(table2, paste0(dir1,"table2.csv"),row.names=T)


########### FIGURE 3 ###########################
file_list <- list.files(path=paste0(dir1,"mean/"))
smart_set <- file_list[grep('smart', file_list)]
rct_set <- file_list[grep('rct', file_list)]
#SMART
smart_data <-data.frame(n = character(), mean= numeric())
for (i in 1:length(smart_set)){
  temp_data <- read.csv(paste0(dir1,"mean/",smart_set[i]), stringsAsFactors = F)
  smart_data <- rbind(smart_data,cbind(unlist(strsplit(smart_set[[i]], ".csv")),temp_data[1,]))
}
smart_data$n = unlist(lapply(as.character(smart_data[,1]), function(x) strsplit(x,"_")[[1]][2]))
smart_data$n <- as.numeric(as.character(smart_data$n ))
names(smart_data)[1]<-"file"

#RCT
rct_data <-data.frame(n = character(), mean= numeric())
for (i in 1:length(rct_set)){
  temp_data <- read.csv(paste0(dir1,"mean/",rct_set[i]), stringsAsFactors = F)
  rct_data <- rbind(rct_data,cbind(unlist(strsplit(rct_set[[i]], ".csv")),temp_data[1,]))
}
rct_data$n = unlist(lapply(as.character(rct_data[,1]), function(x) strsplit(x,"_")[[1]][2]))
rct_data$n <- as.numeric(as.character(rct_data$n ))
names(rct_data)[1]<-"file" 

plot_dat <- cbind(rbind(smart_data[,-c(4:5)], rct_data[,-c(4:7)]), type = factor(c(rep("SMART",nrow(smart_data)), rep("RCT",nrow(rct_data))),levels=c("SMART","RCT")))
overall_mean <- ggplot(plot_dat, aes(n, HBA1c_12)) + geom_line(aes(linetype = type)) + ggtitle(" Overall mean HbA1c") + ylim(8.21,8.25) +ylab(expression(tilde(y)))+
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5,face="bold",size=14))
overall_mean_cost <- ggplot(plot_dat, aes(n, Cost_subject)) + geom_line(aes(linetype = type)) + ggtitle("Overall mean per subject cost (USD)")  +ylim(343,344) +ylab("per subject cost (USD)")+
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5,face="bold",size=14))

pp <- gridExtra::grid.arrange(overall_mean, overall_mean_cost)

ggsave(plot=pp, paste0(dir1,"fig3.png"),width = 12.4, height=9.6, unit = "in")


##### Figure 4 ######
smart_sd_data <-data.frame(n = character(), sd= numeric())
for (i in 1:length(smart_set)){
  temp_data <- read.csv(paste0(dir1,"mean/",smart_set[i]), stringsAsFactors = F)
  smart_sd_data <- rbind(smart_sd_data,cbind(unlist(strsplit(smart_set[[i]], ".csv")),temp_data[2,]))
}
smart_sd_data$n = unlist(lapply(as.character(smart_sd_data[,1]), function(x) strsplit(x,"_")[[1]][2]))
smart_sd_data$n <- as.numeric(as.character(smart_sd_data$n ))
names(smart_sd_data)[1]<-"file"

#RCT
rct_sd_data <-data.frame(n = character(), sd= numeric())
for (i in 1:length(rct_set)){
  temp_data <- read.csv(paste0(dir1,"mean/",rct_set[i]), stringsAsFactors = F)
  rct_sd_data<- rbind(rct_sd_data,cbind(unlist(strsplit(rct_set[[i]], ".csv")),temp_data[2,]))
}
rct_sd_data$n = unlist(lapply(as.character(rct_sd_data[,1]), function(x) strsplit(x,"_")[[1]][2]))
rct_sd_data$n <- as.numeric(as.character(rct_sd_data$n ))
names(rct_sd_data)[1]<-"file" 

plot_sd_dat <- cbind(rbind(smart_sd_data[,-c(4:5)], rct_sd_data[,-c(4:7)]), type = factor(c(rep("SMART",nrow(smart_sd_data)), rep("RCT",nrow(rct_sd_data))),levels=c("SMART","RCT")))
names(plot_sd_dat)[4:7] <- c("AI1","AI2","AI3","AI4")
plot_sd_dat_ai <- reshape2::melt(plot_sd_dat[,4:9], id = c("n", "type"))
AI_sd <- ggplot(plot_sd_dat_ai, aes(n, value)) + geom_line(aes(linetype = type)) +  facet_wrap(. ~ variable,ncol= 2)+
  labs(title=expression( AI[j]~standard~devation~of~HbA1c)) + 
  ylab(TeX("$s_\\tilde{y}$"))+
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5,face="bold",size=14))

ggsave(plot=AI_sd, paste0(dir1,"fig4.png"),width = 12.4, height=9.6, unit = "in")


######################## Figure 5#######
file_list <- list.files(path=paste0(dir1,"best_AI/"))
smart_set <- file_list[grep('smart', file_list)]
rct_set <- file_list[grep('rct', file_list)]
nsimul=10000
#SMART
smart_data <-data.frame(iter=1:nsimul)
for (i in 1:length(smart_set)){
  temp_data <- read.csv(paste0(dir1,"best_AI/",smart_set[i]), stringsAsFactors = F) 
  colnames(temp_data) <- unlist(strsplit(smart_set[[i]], ".csv"))
  if(nrow(temp_data)<nsimul){
    temp_data<- data.frame(c(temp_data[,1], rep(NA,nsimul-nrow(temp_data))))
    colnames(temp_data) <- unlist(strsplit(smart_set[[i]], ".csv"))
  }
  smart_data <- cbind(smart_data,temp_data)
}
smart_data <- smart_data[,-1]
smart_best <- data.frame(AI1  = numeric(0),AI2  = numeric(0),AI3  = numeric(0),AI4  = numeric(0),n=integer(0))
for(i in 1: ncol(smart_data)){
  dd <-table(factor(smart_data[,i],levels =c(1,2,3,4)))
  smart_best[i,1]<- dd[1]/sum(dd) *100
  smart_best[i,2]<- dd[2]/sum(dd) *100
  smart_best[i,3]<- dd[3]/sum(dd) *100
  smart_best[i,4]<- dd[4]/sum(dd) *100
}
smart_n <- as.numeric(unlist(lapply(colnames(smart_data), function(x) strsplit(x,"_")[[1]][2])))
smart_best$n<- smart_n 
smart_best<-reshape2::melt(smart_best,value.name="pct",id.vars="n",variable = "AI")

#RCT
rct_data <-data.frame(iter=1:nsimul)
for (i in 1:length(rct_set)){
  temp_data <- read.csv(paste0(dir1,"best_AI/",rct_set[i]), stringsAsFactors = F) 
  colnames(temp_data) <- unlist(strsplit(rct_set[[i]], ".csv"))
  if(nrow(temp_data)<nsimul){
    temp_data<- data.frame(c(temp_data[,1], rep(NA,nsimul-nrow(temp_data))))
    colnames(temp_data) <- unlist(strsplit(rct_set[[i]], ".csv"))
  }
  rct_data <- cbind(rct_data,temp_data)
}
rct_data <- rct_data[,-1]
rct_best <- data.frame(AI1  = numeric(0),AI2  = numeric(0),AI3  = numeric(0),AI4  = numeric(0),n=integer(0))
for(i in 1: ncol(rct_data)){
  dd <-table(factor(rct_data[,i],levels =c(1,2,3,4)))
  rct_best[i,1]<- dd[1]/sum(dd) *100
  rct_best[i,2]<- dd[2]/sum(dd) *100
  rct_best[i,3]<- dd[3]/sum(dd) *100
  rct_best[i,4]<- dd[4]/sum(dd) *100
}
rct_n <- as.numeric(unlist(lapply(colnames(rct_data), function(x) strsplit(x,"_")[[1]][2])))
rct_best$n<- rct_n 
rct_best<-reshape2::melt(rct_best,value.name="pct",id.vars="n",variable = "AI")

plot_dat <- cbind(rbind(smart_best, rct_best),type = factor(c(rep("SMART",nrow(smart_best)), rep("RCT",nrow(rct_best))),levels=c("SMART","RCT")))
plot_dat <- plot_dat[plot_dat$n >50,]
plot_dat<- plot_dat %>%filter(!is.na(AI))
ggplot(plot_dat, aes(n,  pct)) + geom_line(aes(linetype = type))+ facet_wrap(.~AI,ncol = 2) +
  scale_x_continuous(name="sample size, n", breaks=seq(0,1700,250)) +
  scale_y_continuous(name="percentage(%)", breaks=c(0,10,30,50,70,90),limits=c(0, 95))  +
  #geom_text(data=annotation, aes( x=x, y=y, label=label), , size=2 ,  fontface="bold" )
  labs(title=expression(Percentage~of~times~AI[j]~identified~as~ best))+
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=14),
        plot.title = element_text(hjust = 0.5,face="bold",size=14))
ggsave(paste0(dir1,"fig5.png"),width = 12.4, height=9.6, unit = "in")


