

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
no_cores <- detectCores() -1
cl <- makeCluster(no_cores)  
registerDoParallel(cl) 
print(paste0("Cores = ",detectCores()))



{
  setwd("~/log_repairing_extension/PBAR")
  source('DiscoverNBG.R')
  source('PBAR_effi.R')
  data_all = c('Small', 'Medium', 'Large', 'Huge' , 'Wide',
           'credit-card', 'mccloud','hospital_billing', 'Road_Traffic',
           'b12', 'b17')
  
  for(data in data_all){
    
    setwd("~/log_repairing_extension/PBAR/encoded_normal")
    test =  read.csv( paste(data, ".csv", sep=''),T)
    head(test)
    names(test)[1] = c('Case.ID') 
    rate = c('0.10', '0.25', '0.50', '1.00')
    result = as.numeric()
    for(f in 1:length(rate)){ # change
    setwd("~/log_repairing_extension/PBAR/encoded_anomaly")
    dat = read.csv(paste(data, "_", rate[f], ".csv", sep='' ),T)
    head(dat)
    print(paste(data,"_", rate[f], ".csv", sep = ''))
    dat_save2 = dat
    {# Preprocessing for clean traces
      
      #For substitute the label of activity to simple letter
  
      names(dat)[which(names(dat)=="type_res_trace")] ="label"
      dat= dat[,c("Case", "Timestamp", "Activity","label")]
      dat = dat[order(dat$Case),]
      dat$order = ave(rep(1, nrow(dat)), by= dat$Case, FUN= cumsum)
      dat_sub = unique(dat[,c("Case","label")])
      dat_sub_start = dat_sub
      dat_sub_end = dat_sub
      dat_sub_start$Activity = "Start"
      dat_sub_end$Activity = "End"
      
      dat_sub_start$Timestamp = aggregate(dat$Timestamp , by= list(dat$Case), FUN= function(x){x[1]})[,2]
      dat_sub_end$Timestamp = aggregate(dat$Timestamp , by= list(dat$Case), FUN= function(x){x[length(x)]})[,2]
      dat_sub_start$order = 0
      dat_sub_end$order = aggregate(dat$Activity , by= list(dat$Case), FUN= function(x){length(x)+1 })[,2]
      
      dat_sub_start = dat_sub_start[,names(dat)]
      dat_sub_end = dat_sub_end[,names(dat)]
      
      dat= rbind(dat, dat_sub_start , dat_sub_end)
      dat$order = dat$order +1
      dat = dat[order(dat$Case,dat$order),]
      dat= dat[,c("Case", "Activity", "order", "label")]
      dat$Case = as.factor(dat$Case)
      
      dat_save = dat
      dat = dat[which(dat$label==''),]  
      
      trace_v2 = aggregate(dat$Activity, by=list(dat$Case), FUN=function(x){paste(x, collapse = '>')})
      
      variant_count = table(trace_v2[,2])
      variant_length=  str_count(names(variant_count), pattern = '>')+1
      
      v_id = paste( "v_", 1:length(variant_length) ,sep='')
      v_id = as.vector(rep(v_id, variant_length))
      trace_freq = as.numeric(rep(unlist(variant_count), variant_length))
      one = rep(1, length(v_id))
      cumone = ave(one, v_id , FUN=cumsum)
      variant_frame = data.frame(cbind(v_id,  Activity=unlist(strsplit(names(variant_count), split='>')), order=cumone ,trace_freq = as.numeric(trace_freq) ))
      variant_frame$order = as.numeric(variant_frame$order)
    }
    
    
    {# Preprocessing for anomalous traces
      anomaly= dat_save
      anomaly = anomaly[which(!is.element(anomaly$Case,names(table(anomaly$Case)[which(table(as.character(anomaly$Case))<3)]))),] #change
      anomaly.c = unique(anomaly$Case)
      
      anomaly2 = dat_save[which(is.element(dat_save$Case, anomaly.c)),]
      
      one = rep(1, nrow(anomaly2))
      cumone = ave(one, by= as.factor(anomaly2[,1]), FUN= cumsum)
      dat2 = data.frame(anomaly2[1:(nrow(anomaly2)-1),1],anomaly2[1:(nrow(anomaly2)-1),3],
                        anomaly2[1:(nrow(anomaly2)-1),2], anomaly2[2:nrow(anomaly2),2])
      del = cumone[1:(nrow(anomaly2)-1)] - cumone[2:nrow(anomaly2)]
      dat2 = dat2[which(del<0),]
      names(dat2) = c('Case', 'level', 'from', 'to')
    }
    
    
  
    start = Sys.time()
  
    print('Start to discover NBGs')
    {#Develop Tree in each activity perspective  
      NBGs = NBG(dat, alpha= 0, beta= 0 )  # alpha: weight (likelihood) , beta: frequency 
    }
    

    print('Start to repair logs by PBAR')
    {#Implement Reconstruction
      input = anomaly[ ,1:3]
      reconst2 = PBAR(input, NBGs, dat, dat2, variant_frame, loopmax=15, method.repair = "Max")
    }
    end = Sys.time()
    
    {# Evaluation
    reconst3 = reconst2[,c(2,4)]
    names(reconst3)[1:2] = c('Case', 'res')
    reconst3$repaired = lapply(reconst3$res, '[[', 1)
    reconst3$pattern = lapply(reconst3$res, '[[', 2) 
    reconst3$pattern_base = lapply(reconst3$res, '[[', 3) 
    reconst3 = reconst3[,c(1,3,5,4)]
    reconst4 = merge(reconst3,unique(anomaly[,c(1,4)]), by= c("Case"), all.x= 1)
    
    labelpadding = function(x){
      paste(sort(unlist(lapply( unlist(strsplit(x, split ="," )), FUN= function(x){
        gsub("[^a-zA-Z]", "", x)})))
        ,collapse = '+')
    }
    
    reconst4$pattern =  unlist(lapply( reconst4$pattern, FUN= function(x){labelpadding(x)}))
    reconst4$label =  unlist(lapply( reconst4$label, FUN= function(x){labelpadding(x)}))
    
    reconst4 = reconst4[order(reconst4$Case),]
    reconst4[which(reconst4$label == ''), 'label'] = 'normal'
    
    x = reconst4
    
    test2 = test[which(is.element(test$Case.ID, x$Case)),]
    y = aggregate(test2$Activity , by= list(test2$Case.ID), FUN= function(x){paste(x, collapse = '>>')})
  
    x2 = x[which(as.character(x$repaired)==as.character(y$x)), ]
    accuracy_pattern =1- sum(x2$pattern!=x2$label)/nrow(x2)
    
    
    
    print( paste("Accuracy_pattern = ", accuracy_pattern))
    accuracy =1- sum(as.character(x$repaired)!=as.character(y$x))/nrow(x)
    
    
    loc_skip = which(is.element(y$Group.1,reconst4[which(reconst4$label=='skip'),'Case'] ))
    accuracy_skip =1- sum( (as.character(x$repaired)!=as.character(y$x))[loc_skip] )/length(loc_skip)
    loc_insert = which(is.element(y$Group.1,reconst4[which(reconst4$label=='insert'),'Case'] ))
    accuracy_insert =1- sum( (as.character(x$repaired)!=as.character(y$x))[loc_insert] )/length(loc_insert)
    loc_moved = which(is.element(y$Group.1,reconst4[which(reconst4$label=='moved'),'Case'] ))
    accuracy_moved =1- sum( (as.character(x$repaired)!=as.character(y$x))[loc_moved] )/length(loc_moved)
    loc_rework = which(is.element(y$Group.1,reconst4[which(reconst4$label=='rework'),'Case'] ))
    accuracy_rework =1- sum( (as.character(x$repaired)!=as.character(y$x))[loc_rework] )/length(loc_rework)
    loc_replace = which(is.element(y$Group.1,reconst4[which(reconst4$label=='replace'),'Case'] ))
    accuracy_replace =1- sum( (as.character(x$repaired)!=as.character(y$x))[loc_replace] )/length(loc_replace)
    loc_normal = which(is.element(y$Group.1,reconst4[which(reconst4$label=='normal'),'Case'] ))
    accuracy_normal =1- sum( (as.character(x$repaired)!=as.character(y$x))[loc_normal] )/length(loc_normal)
    
    print(  paste("Reconstuction accuracy = ", ceiling(accuracy*10000)/10000, sep='') )
    print(  paste("Reconstuction accuracy.skip = ", ceiling(accuracy_skip*10000)/10000, sep='') )
    print(  paste("Reconstuction accuracy.insert = ", ceiling(accuracy_insert*10000)/10000, sep='') )
    print(  paste("Reconstuction accuracy.rework = ", ceiling(accuracy_rework*10000)/10000, sep='') )
    print(  paste("Reconstuction accuracy.replace = ", ceiling(accuracy_replace*10000)/10000, sep='') )
    print(  paste("Reconstuction accuracy.moved = ", ceiling(accuracy_moved*10000)/10000, sep='') )
    print(  paste("Reconstuction accuracy.normal = ", ceiling(accuracy_normal*10000)/10000, sep='') )
    }
    
    
    ## performance on anomaly
    reconst5 = reconst4[which(reconst4$label != 'normal'),]
    
    x = reconst5
    test2 = test[which(is.element(test$Case.ID, x$Case)),]
    y = aggregate(test2$Activity , by= list(test2$Case.ID), FUN= function(x){paste(x, collapse = '>>')})
    accuracy_anomaly =1- sum(as.character(x$repaired)!=as.character(y$x))/nrow(x)    
    
    ## performance on single pattern
    x = reconst5[which( !unlist(lapply(reconst5$label, FUN= function(x){
      grepl('+',x, fixed = TRUE)
    }))) ,]
    
    test2 = test[which(is.element(test$Case.ID, x$Case)),]
    y = aggregate(test2$Activity , by= list(test2$Case.ID), FUN= function(x){paste(x, collapse = '>>')})
  
    # accuracy_pattern_single =1- sum(x$pattern!=x$label)/nrow(x)
    accuracy_single =1- sum(as.character(x$repaired)!=as.character(y$x))/nrow(x)
    x2 = x[which(as.character(x$repaired)==as.character(y$x)), ]
    accuracy_pattern_single =1- sum(x2$pattern!=x2$label)/nrow(x2)
    
    ## performance on multiple patterns
    x = reconst5[which( unlist(lapply(reconst5$label, FUN= function(x){
      grepl('+',x, fixed = TRUE)
    }))) ,]
    
    case_multiple = x$Case
    
    test2 = test[which(is.element(test$Case.ID, x$Case)),]
    y = aggregate(test2$Activity , by= list(test2$Case.ID), FUN= function(x){paste(x, collapse = '>>')})
    
    # accuracy_pattern_multiple =1- sum(x$pattern!=x$label)/nrow(x)
    accuracy_multiple =1- sum(as.character(x$repaired)!=as.character(y$x))/nrow(x)
    

      
    x2 = x[which(as.character(x$repaired)==as.character(y$x)), ]
    accuracy_pattern_multiple =1- sum(x2$pattern!=x2$label)/nrow(x2)
    
    ###########
    repaired_trace = lapply(reconst2$result, FUN= function(x){  unlist(strsplit(x[[1]][[1]], split='>>')) }  )
    repaired_case = rep(reconst2$Group.1.x, unlist(lapply(repaired_trace, FUN= length)))
    repaired = data.frame(cbind(Case = as.character(repaired_case),  Activity= unlist(repaired_trace) ))

    x= reconst4
    test2 = test[which(is.element(test$Case.ID, x$Case)),]
    
    x.act = repaired$Activity
    y.act = test2$Activity
    z.act = dat_save2$Activity
    
    act.list = unique( c(x.act,y.act,z.act) )
    letter.list = c(letters, LETTERS, 1:9)
    
    if(length(act.list) > length(letter.list)){
      print("Over size problem: act.length > letters ")
    }
    
    
    x.act = apply( data.frame(x.act), 1, FUN= function(x){ letter.list[which(act.list==x)]} )  
    y.act = apply( data.frame(y.act), 1, FUN= function(x){ letter.list[which(act.list==x)]} )  
    z.act = apply( data.frame(z.act), 1, FUN= function(x){ letter.list[which(act.list==x)]} )  
    
    if(data == "credit-card" | data == 'mccloud'){
      repaired$Case = as.numeric((repaired$Case))
      dat_save2$Case = as.numeric((dat_save2$Case))
    }
    
    if(data == 'b12'){
      repaired$Case = as.numeric((repaired$Case))
      dat_save2$Case = as.numeric((dat_save2$Case))
    }
    
    str.x = aggregate(x.act , by= list(repaired$Case), FUN= function(x){paste(x, collapse = '')})
    str.y = aggregate(y.act , by= list(test2$Case.ID), FUN= function(x){paste(x, collapse = '')})
    str.z = aggregate(z.act , by= list(dat_save2$Case), FUN= function(x){paste(x, collapse = '')})
    
    print( paste( "edit dist(before) = " ,mean(stringdist(str.z$x, str.y$x, method = "lv" )) , collapse = '') ) # error by edit distant (Levenshtein dist)
    print( paste( "edit dist(after) = " ,mean(stringdist(str.x$x, str.y$x, method = "lv")) , collapse = '') ) # error by edit distant (Levenshtein dist)
  
    
    loc_multiple = which( is.element(str.x$Group.1 , case_multiple ))
    
    save = c( data, rate[f] , ceiling(accuracy_pattern*10000)/10000,  
              ceiling(accuracy_pattern_single*10000)/10000, ceiling(accuracy_pattern_multiple*10000)/10000,
              ceiling(accuracy_skip*10000)/10000,  ceiling(accuracy_insert*10000)/10000, 
              ceiling(accuracy_rework*10000)/10000, ceiling(accuracy_replace*10000)/10000,
              ceiling(accuracy_moved*10000)/10000, ceiling(accuracy_normal*10000)/10000,
              ceiling(accuracy_anomaly*10000)/10000, ceiling(accuracy*10000)/10000,
              ceiling(accuracy_single*10000)/10000, ceiling(accuracy_multiple*10000)/10000,
              mean(stringdist(str.z$x, str.y$x, method = "lv")),
              mean(stringdist(str.x$x, str.y$x, method = "lv")), 
              mean(stringdist(str.z$x[loc_normal], str.y$x[loc_normal], method = "lv")),
              mean(stringdist(str.x$x[loc_normal], str.y$x[loc_normal], method = "lv")),
              mean(stringdist(str.z$x[-loc_normal], str.y$x[-loc_normal], method = "lv")),
              mean(stringdist(str.x$x[-loc_normal], str.y$x[-loc_normal], method = "lv")), 
              
              mean(stringdist(str.x$x[loc_skip], str.y$x[loc_skip], method = "lv")),
              mean(stringdist(str.x$x[loc_insert], str.y$x[loc_insert], method = "lv")),
              mean(stringdist(str.x$x[loc_rework], str.y$x[loc_rework], method = "lv")),
              mean(stringdist(str.x$x[loc_replace], str.y$x[loc_replace], method = "lv")),
              mean(stringdist(str.x$x[loc_moved], str.y$x[loc_moved], method = "lv")),
              mean(stringdist(str.x$x[loc_multiple], str.y$x[loc_multiple], method = "lv")),
              
              difftime(end,start, units= 'mins'))
    
    result= rbind(result, save)
    }
    
    result= data.frame(result)
    names(result) = c("Data", "Rate","PatternACC", "PatternACC(single)", "PatternACC(multiple)",
                      'p.skip','p.insert','p.rework','p.replace','p.moved', 'p.normal', 'p.anomaly',
                      "ReconACC", "ReconACC(single)", "ReconACC(multiple)", 
                      "ReconERR.before", "ReconERR.after", 
                      "ReconERR.before.normal", "ReconERR.after.normal",
                      "ReconERR.before.anomaly", "ReconERR.after.anomaly",
                      'e.skip','e.insert','e.rework','e.replace','e.moved',"e.multiple",
                      "time")
    
    setwd("~/log_repairing_extension/PBAR/result_max")
    
    write.csv(result, paste(data, ".csv", sep=''), row.names = F)
}
}


