source("functions.R")
if(!dir.exists("output"))
  dir.create("output")
dir1 = "output/"
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

####### SENSITIVITY ANALYSIS ###############
#####SENSITIVITY OUTPUT FILES ####  ##############
n_list  =  c(20,50,100,300)
threshold_list = seq(-2.5,1.9,0.2)
if(!dir.exists(paste0(dir1,"sensitivity")))
    dir.create(paste0(dir1,"sensitivity"))
dir2 = paste0(dir1,"sensitivity/")
for(j in 1: length(n_list)){
  # sample size, assuming equal ranodmisation 
  n = n_list[j]; 
  if(!dir.exists(paste0(dir2,"threshold_",n)))
    dir.create(paste0(dir2, "threshold_",n))
  for(k in 1:length(threshold_list)){
    threshold = threshold_list[k]
    ################## THRESHOLD SMART OUTPUT FILES  ##############
    smart_threshold  <-foreach(i=1:nsimul, .combine = rbind ) %dopar% {
    seed = 1237974 + i + threshold*nsimul + n*nsimul
    smart_dat <- smart_gen(seed, n, threshold,Baseline_HBA1c_mean, Baseline_HBA1c_sd,
                         MeanNR1, SDNR1,MeanNR2, SDNR2,MeanR1, SDR1,MeanR2, SDR2,
                         pRespondApp, pRespondNurse, pRespondN_A,  
                         cApp, cNurse,cN_A,switch)
    ### overall
    HBA1c_12 = mean(smart_dat[,"Final_HBA1c"])
    return(HBA1c_12)
  }
  write.csv(mean(smart_threshold,na.rm=T), paste0(dir2,"threshold_",n,"/smart_",threshold,".csv"), row.names=FALSE)

    ################## THRESHOLD RCT OUTPUT FILES  ##############
    rct_threshold <-foreach(i=1:nsimul, .combine = rbind ) %dopar% {
    seed = 1244534 + i + threshold*nsimul + n*nsimul
    rct_dat <- rct_gen(seed, n, threshold,Baseline_HBA1c_mean, Baseline_HBA1c_sd,
                     MeanNR1, SDNR1,MeanNR2, SDNR2,MeanR1, SDR1,MeanR2, SDR2,
                     pRespondApp, pRespondNurse, pRespondN_A,  
                     cApp, cNurse,cN_A,switch)
  
    ### overall
   HBA1c_12 = mean(rct_dat[,"Final_HBA1c"])
   return(HBA1c_12)
  }

  write.csv(mean(rct_threshold,na.rm=T), paste0(dir2,"threshold_",n,"/rct_",threshold,".csv"), row.names=FALSE)
    remove(list=c("smart_threshold","rct_threshold","threshold"))
  }
  remove(list=c("n"))
}


####### FIGURE 6
#### threshold sensitivty #######################
n_list = c(20,50,100,300)
plot_dat2<- data.frame(V1=factor(), V2=factor(),threshold=numeric(),HBA1c=numeric(),type=factor(),n=numeric())
for(j in 1:length(n_list)){
  n <- n_list[j]
  folder = paste0(dir2,"threshold_",n,"/")
  file_list <- list.files(path=folder)
  smart_set <- file_list[grep('smart', file_list)]
  rct_set <- file_list[grep('rct', file_list)]
  
  ##############
  #SMART
  ##############
  smart_data <-data.frame(threshold = character(), HBA1c = numeric())
  for (i in 1:length(smart_set)){
    temp_data <- read.csv(paste0(folder,smart_set[i]), stringsAsFactors = F)
    smart_data <- rbind(smart_data,cbind(unlist(strsplit(smart_set[[i]], ".csv")),temp_data[1,1]))
  }
  smart_data$threshold = unlist(lapply(as.character(smart_data$V1), function(x) strsplit(x,"_")[[1]][2]))
  smart_data$threshold <- as.numeric(as.character(smart_data$threshold ))
  smart_data$HBA1c <- as.numeric(as.character(smart_data$V2))
  #########
  #RCT
  #########
  rct_data <-data.frame(threshold = character(), HBA1c = numeric())
  for (i in 1:length(rct_set)){
    temp_data <- read.csv(paste0(folder,rct_set[i]), stringsAsFactors = F)
    rct_data <- rbind(rct_data,cbind(unlist(strsplit(rct_set[[i]], ".csv")),temp_data[1,1]))
  }
  rct_data$threshold = unlist(lapply(as.character(rct_data$V1), function(x) strsplit(x,"_")[[1]][2]))
  rct_data$threshold <- as.numeric(as.character(rct_data$threshold ))
  rct_data$HBA1c <- as.numeric(as.character(rct_data$V2))
  plot_dat <- cbind(rbind(smart_data, rct_data), type = factor(c(rep("SMART",nrow(smart_data)), rep("RCT",nrow(rct_data))),levels=c("SMART","RCT")))
  plot_dat$n <- n
  plot_dat2<- rbind(plot_dat2,plot_dat)
}
plot_dat2<-plot_dat2%>% arrange(n) %>% mutate(n2 = paste0("n = ",n)) %>%
  mutate(n2=factor(n2, levels=unique(n2))) 
ggplot(plot_dat2[plot_dat2$n<=300,], aes(threshold, HBA1c)) + geom_line(aes(linetype = type)) +
  scale_x_continuous(name="HbA1c reduction threshold", breaks=seq(-2.5,1.9,0.5)) +
  scale_y_continuous(name=expression(tilde(y)), breaks=seq(8.25,8.5,0.05)) +
  facet_wrap(~ n2, nrow = 2)+
  theme(axis.text=element_text(size=10.5),
        axis.title=element_text(size=14),
        # axis.text.x = element_text(angle = 90),
        plot.title = element_text(hjust = 0.5,face="bold",size=14))

ggsave(paste0(dir1,"fig6.png"),width = 12.4, height=9.6, unit = "in")



