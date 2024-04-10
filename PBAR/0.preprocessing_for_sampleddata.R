


setwd("~/log_repairing_extension/PBAR/anomaly_v2")

fn= list.files()
fn

for(i in 1:length(fn)){
  print(fn[i])

  dat =read.csv(fn[i],T)
  dat = dat[order(dat$Case, dat$Timestamp, dat$Activity),]
  write.csv(dat, fn[i], row.names = F)
  
  set.seed(1004) # for reprodicibility
  normal = dat[which(dat$type_res_trace ==''),]
  anomal = dat[which(dat$type_res_trace !=''),]
  
  
  sample50 =  sample(unique(normal$Case), size=0.5*length(unique(normal$Case)), replace=F)
  sample25 =  sample(sample50, size=0.5*length(sample50), replace=F)
  sample10 =  sample(sample25, size=0.4*length(sample25), replace=F)
  
  normal10 = normal[which(is.element(normal$Case, sample10)),]
  normal25 = normal[which(is.element(normal$Case, sample25)),]
  normal50 = normal[which(is.element(normal$Case, sample50)),]
  
  dat10 = rbind(normal10, anomal)
  dat25 = rbind(normal25, anomal)
  dat50 = rbind(normal50, anomal)
  
  write.csv(dat10, paste( unlist(strsplit(fn[i], split = '1.00'))[1] , '0.10', ".csv", sep = ''), row.names = F)
  write.csv(dat25, paste( unlist(strsplit(fn[i], split = '1.00'))[1] , '0.25', ".csv", sep = ''), row.names = F)
  write.csv(dat50, paste( unlist(strsplit(fn[i], split = '1.00'))[1] , '0.50', ".csv", sep = ''), row.names = F)
  
}

