



  options(warn=-1)
  library(visNetwork)
  library(stringdist)
  library(DiagrammeR)
  library(data.table)
  library(dplyr)
  library(caret)
  library(stringr)
  library(foreach)
  library(doParallel)
  library(iterators)
  library(MASS)
  
  no_cores <- detectCores() - 1 
  cl <- makeCluster(no_cores)  
  registerDoParallel(cl)  
  
  setwd("~/log_repairing_extension/PBAR")
  source('DiscoverNBG.R')
  source('PBAR_effi.R')

  {
    setwd("~/log_repairing_extension/PBAR/encoded_normal")
    dat = read.csv("PNUH_10000.csv" ,T)
    dat = dat[,c('Case.ID', 'Activity')]
    names(dat)[1] ='Case'
    dat$order = 1
    
    for(i in 2:nrow(dat)){
      if(dat$Case[i] == dat$Case[i-1] ){
        dat$order[i] = dat$order[i-1] + 1
      }
    }
    dat[1:15,]
    dat[which(dat$Activity == "EVENT 17 START"), 'Activity'] = "Start"
    dat[which(dat$Activity == "EVENT 32 END"),'Activity' ] = "End"
    dat[which(dat$Activity == "Print_Prescription"),'Activity' ] = "Print_Prescript"
    
    trace_v2 = aggregate(dat$Activity, by=list(dat$Case), FUN=function(x){paste(x, collapse = '>>')})
    
    variant_count = table(trace_v2[,2])
    variant_length=  str_count(names(variant_count), pattern = '>>')+1
    
    v_id = paste( "v_", 1:length(variant_length) ,sep='')
    v_id = as.vector(rep(v_id, variant_length))
    trace_freq = as.numeric(rep(unlist(variant_count), variant_length))
    one = rep(1, length(v_id))
    cumone = ave(one, v_id , FUN=cumsum)
    variant_frame = data.frame(cbind(v_id,  Activity=unlist(strsplit(names(variant_count), split='>>')), order=cumone ,trace_freq = as.numeric(trace_freq) ))
    variant_frame$order = as.numeric(variant_frame$order)
  
  }
  unlist(lapply( unlist(trace_v2$x), FUN = function(x){ grep("Receipt_Examination>>Payment>>Receipt_Treatment>>Start_Treatment>>Finish_Treatment>>Posterior_Treatment>>End",x )  }))
  {# Preprocessing for anomalous traces
    
    setwd("~/log_repairing_extension/PBAR/encoded_anomaly")
    dat2 = read.csv("PNUH_anomaly.csv",T)
    anomaly= dat2
    names(anomaly)[3] = 'order'
    dat2= dat2[,c("Case", "Timestamp", "Activity","h_diag2")]
    dat2$h_diag2 = round(dat2$h_diag2,9)
    dat2 = dat2[order(dat2$Case),]
    dat2$order = ave(rep(1, nrow(dat2)), by= dat2$Case, FUN= cumsum)
    dat_sub = unique(dat2[,c("Case","h_diag2")])
    dat_sub_start = dat_sub
    dat_sub_end = dat_sub
    dat_sub_start$Activity = "Start"
    dat_sub_end$Activity = "End"
    
    dat_sub_start$Timestamp = aggregate(dat2$Timestamp , by= list(dat2$Case), FUN= function(x){x[1]})[,2]
    dat_sub_end$Timestamp = aggregate(dat2$Timestamp , by= list(dat2$Case), FUN= function(x){x[length(x)]})[,2]
    dat_sub_start$order = 0
    dat_sub_end$order = aggregate(dat2$Activity , by= list(dat2$Case), FUN= function(x){length(x)+1 })[,2]
    
    dat_sub_start = dat_sub_start[,names(dat2)]
    dat_sub_end = dat_sub_end[,names(dat2)]
    
    dat2= rbind(dat2, dat_sub_start , dat_sub_end)
    dat2$order = dat2$order +1
    dat2 = dat2[order(dat2$Case,dat2$order),]
    
    dat2= dat2[,c("Case", "Activity", "order", "h_diag2")]
    dat2$Case = as.factor(dat2$Case)
    trace_v1 = aggregate(dat2$Activity, by=list(dat2$Case), FUN = function(x){ paste(x, collapse = ">>")  } )
    dat2 = data.frame(dat2[1:(nrow(dat2)-1),1],dat2[1:(nrow(dat2)-1),3],
                      dat2[1:(nrow(dat2)-1),2], dat2[2:nrow(dat2),2])
    names(dat2) = c('Case', 'level', 'from', 'to')
    dat2 = dat2[which(dat2$from != "End"),]

  }
  
  

  start = Sys.time()


  print('Start to discover NBGs')
  {#Develop Tree in each activity perspective  
    NBGs = NBG(dat, alpha= 0, beta= 0 )  # alpha: weight (likelihood) , beta: frequency 
  }

  print('Start to repair logs by PBAR')
  {#Implement Reconstruction
    input = anomaly[ ,1:3]
    reconst2 = PBAR(input, NBGs, dat, dat2, variant_frame, loopmax=15, method.repair = "Prob")
  }
  end = Sys.time()

  # Evaluation
  reconst3 = reconst2[,c(2,4)]
  names(reconst3)[1:2] = c('Case', 'res')
  reconst3$repaired = lapply(reconst3$res, '[', 1)
  reconst3$pattern = lapply(reconst3$res, '[', 2) 
  reconst3$pattern_base = lapply(reconst3$res, '[', 3) 
  reconst3 = reconst3[,c(1,3,5,4)]
  reconst4 = merge(reconst3,unique(anomaly[,c(1,4)]), by= c("Case"), all.x= 1)
  
  labelpadding = function(x){
    paste(sort(unlist(lapply( unlist(x), FUN= function(x){
      gsub("[^a-zA-Z]", "", x)})))
      ,collapse = '+')
  }
  
  reconst4$pattern =  unlist(lapply( reconst4$pattern_base, FUN= function(x){labelpadding(x)}))
  
  reconst4[which(reconst4$pattern !='normal'),  ]
  
  see = reconst4[which(reconst4$pattern =='normal'),  ]
  see = reconst4
  see= see[order(see$h_diag2 , decreasing = 1),]
  see$repaired = unlist(see$repaired)
  see = see[,c(1,2,4,5)]
  target = trace_v1[which(is.element(trace_v1$Group.1, see$Case )), ]
  see$trace = unlist(target[match(see$Case, target$Group.1), 'x'])
  see$trace = unlist(lapply(see$trace, FUN= function(x){ strsplit(x, split="Start>>")[[1]][2]}))
  see$trace = unlist(lapply(see$trace, FUN= function(x){ strsplit(x, split=">>End")}))
  
  setwd("~/log_repairing_extension")
  write.csv(see, "result_PNUH.csv", row.names = F)





