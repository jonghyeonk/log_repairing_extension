

## explainability 

data_all = c('Small', 'Medium', 'Wide', 'Large', 'Huge' , 
             'credit-card', 'mccloud','hospital_billing', 'Road_Traffic', 
             'b12', 'b17')
result1 = as.numeric()
for(data in data_all){
  setwd("C:/Users/whd1g/OneDrive/바탕 화면/볼차노/PBAR_extension/log_repairing_extension.v2/result_max_v2")
  pbar =  read.csv( paste(data, ".csv", sep=''),T)
  head(pbar)
  pbar = pbar[which(pbar$Rate == 1.0 ),]
  

  result1= rbind(result1,
                 c(round(as.numeric(pbar$PatternACC.single.),2),
                   round(as.numeric(pbar$PatternACC.multiple.),2)
                 ))

  
}


result1= data.frame(t(result1))
names(result1) = c( 'Small', 'Medium', 'Wide', 'Large', 'Huge' , 
                    'Credit', 'Pub','Hospital Billing', 'Road Traffic', 
                    'BPIC 2012', 'BPIC 2017')

result1
setwd("C:/Users/whd1g/OneDrive/바탕 화면/볼차노/PBAR_extension/log_repairing_extension.v2")
write.csv(result1, "eval3.csv", row.names = F)


## time 

data_all = c('Small', 'Medium', 'Wide', 'Large', 'Huge' , 
             'credit-card', 'mccloud','hospital_billing', 'Road_Traffic', 
             'b12', 'b17')
result1 = as.numeric()
for(data in data_all){
  setwd("C:/Users/whd1g/OneDrive/바탕 화면/볼차노/PBAR_extension/log_repairing_extension.v2/result_max_v2")
  pbar =  read.csv( paste(data, ".csv", sep=''),T)
  head(pbar)
  pbar = pbar[which(pbar$Rate == 1.0 ),]
  
  
  result1= rbind(result1,
                 c(round(as.numeric(pbar$time),4)
                 ))
  
}


result1= data.frame(t(result1))
names(result1) = c( 'Small', 'Medium', 'Wide', 'Large', 'Huge' , 
                    'Credit', 'Pub','Hospital Billing', 'Road Traffic', 
                    'BPIC 2012', 'BPIC 2017')

result1
setwd("C:/Users/whd1g/OneDrive/바탕 화면/볼차노/PBAR_extension/log_repairing_extension.v2")
write.csv(result1, "eval4.csv", row.names = F)

##############################################################################


#table3
data_all = c('Small', 'Medium', 'Wide', 'Large', 'Huge' , 
             'credit-card', 'mccloud','hospital_billing', 'Road_Traffic', 
             'b12', 'b17')
result1 = as.numeric()
for(data in data_all){
  setwd("C:/Users/ADMIN/Desktop/볼차노/PBAR_extension/log_repairing_extension.v2/result_max_v2")
  pbar =  read.csv( paste(data, ".csv", sep=''),T)
  head(pbar)
  pbar = pbar[which(pbar$Rate == 1.0 ),]
  
  td = read.csv( paste(data, "_align_trd.csv", sep=''),T)
  td = td[which(td$Rate == 1.0 ),]
  
  ed = read.csv( paste(data, "_align_edit.csv", sep=''),T)
  ed = ed[which(ed$Rate == 1.0 ),]
  
  da = read.csv( paste(data, "_deepalign.csv", sep=''),T)
  da = da[which(da$Rate == 1.0 ),]  
  
  
  
  result1= rbind(result1,
                 c(round(as.numeric(pbar$ReconACC),3) ,
                                                 round(as.numeric(td$ReconACC),3),
                                                 round(as.numeric(ed$ReconACC),3), 
                                                 round(as.numeric(da$ReconACC),3), 
                   
                  round(as.numeric(pbar$p.normal),3), 
                                     round(as.numeric(td$p.normal),3), 
                                     round(as.numeric(ed$p.normal),3),
                                     round(as.numeric(da$p.normal),3), 
                   
                   round(as.numeric(pbar$p.anomaly),3) ,
                                     round(as.numeric(td$p.anomaly),3) ,
                                     round(as.numeric(ed$p.anomaly),3) ,
                                     round(as.numeric(da$p.anomaly),3)

                 ))
  
  
}


result1
round(apply(result1, 2, mean),3)

################################################################################

#table2

skip = as.numeric()
insert = as.numeric()
rework = as.numeric()
replace = as.numeric()
move = as.numeric()
multiple = as.numeric()
result2 = as.numeric()
for(data in data_all){
  setwd("C:/Users/whd1g/OneDrive/바탕 화면/볼차노/PBAR_extension/log_repairing_extension.v2/result_max_v2")
  pbar =  read.csv( paste(data, ".csv", sep=''),T)
  head(pbar)
  pbar = pbar[which(pbar$Rate == 1.0 ),]
  
  td = read.csv( paste(data, "_align_trd.csv", sep=''),T)
  td = td[which(td$Rate == 1.0 ),]
  
  ed = read.csv( paste(data, "_align_edit.csv", sep=''),T)
  ed = ed[which(ed$Rate == 1.0 ),]
  
  da = read.csv( paste(data, "_deepalign.csv", sep=''),T)
  da = da[which(da$Rate == 1.0 ),]  
  
  skip= cbind(skip, c( format(round(as.numeric(pbar$p.skip),2), nsmall=2),
                       format(round(as.numeric(td$p.skip),2), nsmall=2),
                       format(round(as.numeric(ed$p.skip),2), nsmall=2),
                       format(round(as.numeric(da$p.skip),2), nsmall=2)))
  
  insert= cbind(insert, c( format(round(as.numeric(pbar$p.insert),2), nsmall=2),
                           format(round(as.numeric(td$p.insert),2), nsmall=2),
                           format(round(as.numeric(ed$p.insert),2), nsmall=2),
                           format(round(as.numeric(da$p.insert),2), nsmall=2)))
  
  rework= cbind(rework, c( format(round(as.numeric(pbar$p.rework),2), nsmall=2),
                           format(round(as.numeric(td$p.rework),2), nsmall=2),
                           format(round(as.numeric(ed$p.rework),2), nsmall=2),
                           format(round(as.numeric(da$p.rework),2), nsmall=2)))
  
  replace= cbind(replace, c( format(round(as.numeric(pbar$p.replace),2), nsmall=2),
                             format(round(as.numeric(td$p.replace),2), nsmall=2),
                             format(round(as.numeric(ed$p.replace),2), nsmall=2),
                             format(round(as.numeric(da$p.replace),2), nsmall=2)))
  
  move= cbind(move, c( format(round(as.numeric(pbar$p.move),2), nsmall=2),
                       format(round(as.numeric(td$p.move),2), nsmall=2),
                       format(round(as.numeric(ed$p.move),2), nsmall=2),
                       format(round(as.numeric(da$p.move),2), nsmall=2)))
  
  multiple= cbind(multiple, c( format(round(as.numeric(pbar$ReconACC.multiple.),2), nsmall=2),
                               format(round(as.numeric(td$ReconACC.multiple.),2), nsmall=2),
                               format(round(as.numeric(ed$ReconACC.multiple.),2), nsmall=2),
                               format(round(as.numeric(da$ReconACC.multiple.),2), nsmall=2)))
  
  
}



result2 = rbind( skip, insert, rework, replace, move, multiple)
result2= data.frame(result2)

names(result2) = c( 'Small', 'Medium', 'Wide', 'Large', 'Huge' , 
                    'Credit', 'Pub','Hospital Billing', 'Road Traffic', 
                    'BPIC 2012', 'BPIC 2017')

result2
setwd("C:/Users/whd1g/OneDrive/바탕 화면/볼차노/PBAR_extension/log_repairing_extension.v2")
write.csv(result2, "eval2.csv", row.names = F)



##############################################################################


#table3 - similarity
data_all = c('Small', 'Medium', 'Wide', 'Large', 'Huge' , 
             'credit-card', 'mccloud','hospital_billing', 'Road_Traffic', 
             'b12', 'b17')
result1 = as.numeric()
for(data in data_all){
  setwd("C:/Users/ADMIN/Desktop/볼차노/PBAR_extension/log_repairing_extension.v2/result_max_v3")
  pbar =  read.csv( paste(data, ".csv", sep=''),T)
  head(pbar)
  pbar = pbar[which(pbar$Rate == 1.0 ),]
  
  td = read.csv( paste(data, "_align_trd.csv", sep=''),T)
  td = td[which(td$Rate == 1.0 ),]
  
  ed = read.csv( paste(data, "_align_edit.csv", sep=''),T)
  ed = ed[which(ed$Rate == 1.0 ),]
  
  da = read.csv( paste(data, "_deepalign.csv", sep=''),T)
  da = da[which(da$Rate == 1.0 ),]  
  
  
  
  result1= rbind(result1,
                 c(round(as.numeric(pbar$ReconERR.after),3) ,
                   round(as.numeric(td$ReconERR.after),3) ,
                   round(as.numeric(ed$ReconERR.after),3) ,
                   round(as.numeric(da$ReconERR.after),3) ,
                   
                   round(as.numeric(pbar$ReconERR.after.normal),3) ,
                   round(as.numeric(td$ReconERR.after.normal),3) ,
                   round(as.numeric(ed$ReconERR.after.normal),3) ,
                   round(as.numeric(da$ReconERR.after.normal),3) ,
                   
                   round(as.numeric(pbar$ReconERR.after.anomaly),3) ,
                   round(as.numeric(td$ReconERR.after.anomaly),3) ,
                   round(as.numeric(ed$ReconERR.after.anomaly),3) ,
                   round(as.numeric(da$ReconERR.after.anomaly),3)
                   
                 ))
  
  
}


result1
round(apply(result1, 2, mean),3)


setwd("C:/Users/ADMIN/Desktop/볼차노/PBAR_extension/log_repairing_extension.v2")
write.csv(result1, "eval_error.csv", row.names = F)

part1 = result1[,1:4]
part2 = result1[,5:8]
part3 = result1[,9:12]

ACC1 = apply(part1, 1, FUN= function(x){paste(x, collapse = " / ")})
ACC2 = apply(part2, 1, FUN= function(x){paste(x, collapse = " / ")})
ACC3 = apply(part3, 1, FUN= function(x){paste(x, collapse = " / ")})

ave = round(apply(result1, 2, mean), 3)

result2 = data.frame("Event logs" = c('Small', 'Medium', 'Wide', 'Large', 'Huge', ' Credit', 'Pub', 
                           'Hospital B.', 'Road Traffic', 'BPIC 2012', 'BPIC 2017'))

ACC1 = unlist(lapply(ACC1,FUN= function(x){paste0('(',x, ')')}))
ACC2 = unlist(lapply(ACC2,FUN= function(x){paste0('(',x, ')')}))
ACC3 = unlist(lapply(ACC3,FUN= function(x){paste0('(',x, ')')}))

result2$SIM1 = ACC1
result2$SIM2 = ACC2
result2$SIM3 = ACC3

result2

setwd("C:/Users/ADMIN/Desktop/볼차노/PBAR_extension/log_repairing_extension.v2")
write.csv(result2, "eval_error2.csv", row.names = F)


###################


################################################################################

#table2

skip = as.numeric()
insert = as.numeric()
rework = as.numeric()
replace = as.numeric()
move = as.numeric()
multiple = as.numeric()
result2 = as.numeric()
for(data in data_all){
  setwd("C:/Users/ADMIN/Desktop/볼차노/PBAR_extension/log_repairing_extension.v2/result_max_v3")
  pbar =  read.csv( paste(data, ".csv", sep=''),T)
  head(pbar)
  pbar = pbar[which(pbar$Rate == 1.0 ),]
  
  td = read.csv( paste(data, "_align_trd.csv", sep=''),T)
  td = td[which(td$Rate == 1.0 ),]
  
  ed = read.csv( paste(data, "_align_edit.csv", sep=''),T)
  ed = ed[which(ed$Rate == 1.0 ),]
  
  da = read.csv( paste(data, "_deepalign.csv", sep=''),T)
  da = da[which(da$Rate == 1.0 ),]  
  

  skip= cbind(skip, c( format(round(as.numeric(pbar$e.skip),2), nsmall=2),
                       format(round(as.numeric(td$e.skip),2), nsmall=2),
                       format(round(as.numeric(ed$e.skip),2), nsmall=2),
                       format(round(as.numeric(da$e.skip),2), nsmall=2)))
  
  insert= cbind(insert, c( format(round(as.numeric(pbar$e.insert),2), nsmall=2),
                           format(round(as.numeric(td$e.insert),2), nsmall=2),
                           format(round(as.numeric(ed$e.insert),2), nsmall=2),
                           format(round(as.numeric(da$e.insert),2), nsmall=2)))
  
  rework= cbind(rework, c( format(round(as.numeric(pbar$e.rework),2), nsmall=2),
                           format(round(as.numeric(td$e.rework),2), nsmall=2),
                           format(round(as.numeric(ed$e.rework),2), nsmall=2),
                           format(round(as.numeric(da$e.rework),2), nsmall=2)))
  
  replace= cbind(replace, c( format(round(as.numeric(pbar$e.replace),2), nsmall=2),
                             format(round(as.numeric(td$e.replace),2), nsmall=2),
                             format(round(as.numeric(ed$e.replace),2), nsmall=2),
                             format(round(as.numeric(da$e.replace),2), nsmall=2)))
  
  move= cbind(move, c( format(round(as.numeric(pbar$e.move),2), nsmall=2),
                       format(round(as.numeric(td$e.move),2), nsmall=2),
                       format(round(as.numeric(ed$e.move),2), nsmall=2),
                       format(round(as.numeric(da$e.move),2), nsmall=2)))
  
  multiple= cbind(multiple, c( format(round(as.numeric(pbar$e.multiple),2), nsmall=2),
                               format(round(as.numeric(td$e.multiple),2), nsmall=2),
                               format(round(as.numeric(ed$e.multiple),2), nsmall=2),
                               format(round(as.numeric(da$e.multiple),2), nsmall=2)))
  
  
}



result2 = rbind( skip, insert, rework, replace, move, multiple)
result2= data.frame(result2)

names(result2) = c( 'Small', 'Medium', 'Wide', 'Large', 'Huge' , 
                    'Credit', 'Pub','Hospital Billing', 'Road Traffic', 
                    'BPIC 2012', 'BPIC 2017')

result2
setwd("C:/Users/ADMIN/Desktop/볼차노/PBAR_extension/log_repairing_extension.v2")
write.csv(result2, "eval_error3.csv", row.names = F)

