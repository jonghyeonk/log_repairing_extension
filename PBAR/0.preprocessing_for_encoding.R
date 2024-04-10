

library(stringdist)

setwd("~/log_repairing_extension/PBAR/normaldata")

fn= list.files()
fn

rate = c('0.10', '0.25', '0.50', '1.00')
for(i in 1:length(fn)){
  print(fn[i])
  setwd("~/log_repairing_extension/PBAR/normaldata")
  
  dat =read.csv(fn[i],T)

  x.act = dat$Activity
  letter.list = c(letters, LETTERS, 1:9)
  act.list.save = c()
  for(j in 1:length(rate)){
    setwd("~/log_repairing_extension/PBAR/anomaly_v2")
    dat2 =read.csv(paste( strsplit( fn[i], ".csv")[[1]], "_", rate[j], ".csv", sep='') ,T)
    
    x.act2 = dat2$Activity
    
    act.list = unique( x.act2 )
    
    act.list = c(act.list.save,  act.list[!is.element(act.list, act.list.save )])
    
    if(length(act.list) > length(letter.list)){
      print("Over size problem: act.length > letters ")
    }
    
    x.act2 = apply( data.frame(x.act2), 1, FUN= function(x){ letter.list[which(act.list==x)]} )  
    dat2$Activity = x.act2
    setwd("~/log_repairing_extension/PBAR/encoded_anomaly")
    write.csv(dat2, paste( strsplit( fn[i], ".csv")[[1]], "_", rate[j], ".csv", sep=''), row.names = FALSE)
    
    act.list.save = act.list
  }
  
  x.act = apply( data.frame(x.act), 1, FUN= function(x){ letter.list[which(act.list.save==x)]} )  

  dat$Activity = x.act
  names(dat)[which(names(dat)=='Timestamp' )] = 'Complete.Timestamp'
  setwd("~/log_repairing_extension/PBAR/encoded_normal")
  write.csv(dat, fn[i], row.names = FALSE)
}


#### For case study PNUH

setwd("~/log_repairing_extension/PBAR/dat_PNUH")

library(readr)

rate = c('0.10', '0.25', '0.50', '0.75', '1.00')
dat =read_csv("PNUH_with_leverage.csv",T , locale=locale('ko',encoding='euc-kr'))

setwd("~/log_repairing_extension/PBAR/normaldata")

dat =read.csv(fn[i],T)

dat = dat[,c("caseid", 'Activity', 'timestamp', 'h_diag2', 'label')]
x.act = dat$Activity

dat[which(dat$Activity == '진료및검사접수예약'), 'Activity'] = 'Reserve_Receipt'
dat[which(dat$Activity == '진료시작'), 'Activity'] = 'Start_Treatment'
dat[which(dat$Activity == '진료종료'), 'Activity'] = 'Finish_Treatment'
dat[which(dat$Activity == '수납'), 'Activity'] = 'Payment'
dat[which(dat$Activity == '처방전발행'), 'Activity'] = 'Print_Prescript'
dat[which(dat$Activity == '진료접수'), 'Activity'] = 'Receipt_Treatment'
dat[which(dat$Activity == '처치'), 'Activity'] = 'Posterior_Treatment'
dat[which(dat$Activity == '검사예약'), 'Activity'] = 'Reserve_Examination'
dat[which(dat$Activity == '검사시작'), 'Activity'] = 'Start_Examination'
dat[which(dat$Activity == '검사접수'), 'Activity'] = 'Receipt_Examination'
dat[which(dat$Activity == '검사완료'), 'Activity'] = 'Finish_Examination'
dat[which(dat$Activity == '원내약수령'), 'Activity'] = 'Receive_Prescript'
dat[which(dat$Activity == '증명서발급'), 'Activity'] = 'Print_Certificate'
dat[which(dat$Activity == '전원의뢰서'), 'Activity'] = 'Hospital_Transfer'
dat[which(dat$Activity == '수술방퇴실'), 'Activity'] = 'Exit_Operation'
dat[which(dat$Activity == '수술방입실'), 'Activity'] = 'Enter_Operation'
dat[which(dat$Activity == '마취시작'), 'Activity'] = 'Start_Anesthesia'
dat[which(dat$Activity == '수술종료'), 'Activity'] = 'Finish_Operation'
dat[which(dat$Activity == '수술시작'), 'Activity'] = 'Start_Operation'
dat[which(dat$Activity == '마취종료'), 'Activity'] = 'Finish_Anesthesia'


dat= dat[order(dat$caseid, dat$timestamp),]

dat$caseid = as.factor(dat$caseid)
levels(dat$caseid) = paste0("anony", 1:length(levels(dat$caseid)))

order = aggregate(dat$timestamp ,by= list(dat$caseid), order)

dat$order = unlist(order$x)
names(dat)[1] = c('Case')
names(dat)[6] = c('Timestamp')
dat = dat[, c("Case", "Activity", "Timestamp", "h_diag2" )]
write.csv(dat, "PNUH.csv", row.names = F)

setwd("~/log_repairing_extension/PBAR/dat_PNUH")
dat = read.csv("PNUH.csv", T)
normal = dat[which(dat$h_diag2 < 0.1), ]
normal_trace = unique( normal[,c('Case', 'h_diag2')] )

normal1 = normal_trace[order(normal_trace$h_diag2),'Case'][1:500]

normal2 = normal[is.element(normal$Case, normal1),]

anomaly = dat[which(dat$h_diag2>0.1  ),]

dat1 = rbind(normal2,  anomaly)


setwd("~/log_repairing_extension/PBAR/encoded_anomaly")
write.csv(anomaly, "PNUH_anomaly.csv" , row.names = FALSE)

