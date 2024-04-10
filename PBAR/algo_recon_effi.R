source('VotingMatrix.R')
library(dplyr)


recon_rework = function(repair, NBGs, actlist, place_rework, longerfirst, k , e2_set, actset, num_place){
  actset = NBGs[[1]]
  model.nodes = NBGs[[2]]
  model.edges = NBGs[[3]]
  base.list = NBGs[[4]]
  longerfirst = c(place_rework, longerfirst[-which(is.element(longerfirst, place_rework))])
  error = e2_set[[longerfirst[k]]]
  actlist_repaired = actlist[-(error+1)]
  repair = data.frame( cbind (Case= repair$Case[1], level= 1:(length(actlist_repaired)-1),
                              from= actlist_repaired[-length(actlist_repaired)] , to= actlist_repaired[-1]) )
  vote = voting_matrix(repair, actset, model.nodes, model.edges, base.list)
  
  # number of anomalous places should be reduced one by one
  if(length(which(apply(vote[[1]],2,sum)>1)) < num_place){
    return( list(predict_pattern = 'rework', 
                 predict_pattern_base = 'rework', 
                 k = 1,
                 actlist_repaired = actlist_repaired,
                 vote= vote,
                 repair = repair)  )
  }else{
    return( list(predict_pattern = 'failed_rework', 
                 predict_pattern_base = 'failed_rework',
                 k = k+1,
                 actlist_repaired = actlist_repaired,
                 vote= vote,
                 repair = repair)  )
  }
}


recon_insert_A = function(repair, NBGs, actlist, longerfirst, k, error, 
                          len_each, actset, num_place){
  actset = NBGs[[1]]
  model.nodes = NBGs[[2]]
  model.edges = NBGs[[3]]
  base.list = NBGs[[4]]
  actlist_repaired = actlist[-(error+1)[1:(len_each[longerfirst[k]]-1)] ]
  repair = data.frame( cbind (Case= as.character(repair$Case[1]), level= 1:(length(actlist_repaired)-1),
                              from= actlist_repaired[-length(actlist_repaired)] , to= actlist_repaired[-1]) )
  
  vote = voting_matrix(repair, actset, model.nodes, model.edges, base.list)
  err_insert = which(apply(vote[[1]],2,sum)>1)
  
  if(length(err_insert) <= num_place){
    return( list(predict_pattern = 'insert', 
                 predict_pattern_base = 'insert_A', 
                 k = 1,
                 actlist_repaired = actlist_repaired,
                 vote= vote,
                 repair = repair,
                 deleted_seq= actlist[(error+1)[1:(len_each[longerfirst[k]]-1)] ],
                 deleted_loc =  error[1]
                 ))
  }else{
    return( list(predict_pattern = 'failed_insert', 
                 predict_pattern_base = 'failed_insert',
                 k = k+1,
                 actlist_repaired = actlist_repaired,
                 vote= vote,
                 repair = repair,
                 deleted_seq= actlist[(error+1)[1:(len_each[longerfirst[k]]-1)] ],
                 deleted_loc =  error[1]
                 ))
  }
}




recon_insert_B = function(repair, NBGs, actlist, longerfirst, j, k, 
                          error, len_each, actset, num_place, replace_change,
                          Indirect_replace, replace_loc){
  ###
  actset = NBGs[[1]]
  model.nodes = NBGs[[2]]
  model.edges = NBGs[[3]]
  base.list = NBGs[[4]]
  actlist_repaired = actlist[-(error + replace_loc)]  
  repair = data.frame( cbind (Case= repair$Case[1], level= 1:(length(actlist_repaired)-1),
                              from= actlist_repaired[-length(actlist_repaired)] , to= actlist_repaired[-1]) )
  vote = voting_matrix(repair, actset, model.nodes, model.edges, base.list)
  
  if(length(which(apply(vote[[1]],2,sum)>1)) <= num_place){
    if(j==1){ 
      replace_change = replace_change +1 
    }else if(replace_change>0){
      replace_change = replace_change +1 
    }else{  # when j >1 but replace pattern checked in first order.
      Indirect_replace = TRUE
    }
    return( list(predict_pattern = 'insert', 
                 predict_pattern_base = 'insert_B', 
                 k = 1,
                 actlist_repaired = actlist_repaired,
                 vote= vote,
                 repair = repair,
                 deleted_seq= actlist[(error+ replace_loc)],
                 deleted_loc= error[1]+replace_loc,
                 replace_change= replace_change,
                 Indirect_replace = Indirect_replace))
  }else{
    return( list(predict_pattern = 'failed_insert', 
                 predict_pattern_base = 'failed_insert',
                 k = k+1,
                 actlist_repaired = actlist_repaired,
                 vote= vote,
                 repair = repair,
                 deleted_seq= actlist[(error+1)[1:(len_each[longerfirst[k]]-1)] ],
                 deleted_loc = error[1],
                 replace_change= replace_change,
                 Indirect_replace = Indirect_replace) )
  }
}




sliding_skip = function(actlist, variant_frame, e2_set, error){
  multi_grid =  c(0, unlist(e2_set), 10000)
  loc = which(multi_grid== error)
  if(loc == 2 ){
    prior = actlist[(multi_grid[loc-1]):multi_grid[loc]]
  }else{
    prior = actlist[(multi_grid[loc-1]+1):multi_grid[loc]]
  }
  post = actlist[(multi_grid[loc]+1):multi_grid[loc+1]]
  post = post[which(!is.na(post))]
  
  if(length(actlist) == 2){
    post = actlist[2]
  }
  
  variant_frame_filter = aggregate( variant_frame$order, by= list(variant_frame$v_id), FUN= function(x){
    max(as.numeric(x))
  })
  variant_frame_filter = variant_frame_filter[which(variant_frame_filter$x <= length(actlist) +2 &
                                                      variant_frame_filter$x > length(prior) + length(post)), 'Group.1']
  # +1: skip / +2:moved by 1 step
  variant_frame2 = variant_frame[which(is.element(variant_frame$v_id, variant_frame_filter) ),]
  
  if(length(variant_frame_filter) == 0){
    sliding_check = data.frame()
  }else{
    sliding_check = aggregate(variant_frame2$Activity, by=list(variant_frame2$v_id), FUN= function(x){  #check prior-sequence
      s =((length(prior)))
      st=1
      loop_finish = TRUE
      while(loop_finish){
        if(sum(x[st:(st+s-1)] != prior, na.rm =T) ==0 ){
          loop_finish = FALSE
          for(t in (st+s+1):(st+s+2)){
            tt = x[t:(t+(length(post)-1))]
            tt = tt[which(!is.na(tt))]
            if( length(tt>0)  & sum(tt != post, na.rm =T) ==0){
              inject_sub = x[(st+s):(t-1)]
              return( list(inject_sub))
            } 
          }
        }
        st = st+1
      }
    })
    names(sliding_check) = c('v_id', 'inject')
    
    check1 = unlist(lapply(sliding_check$inject, FUN= is.null))
    if(sum(check1)!=0 ){
      sliding_check= sliding_check[-which(check1),]
    }
    check2 = unlist( lapply(sliding_check$inject, FUN= function(x){
      is.na(x[[1]])}))
    if(  sum(check2) != 0  ){
      sliding_check = sliding_check[-which(check2), ]
    }
  }
  return(list(sliding_check = sliding_check,
              variant_frame2 = variant_frame2))
}



recon_skip = function(sliding_check, NBGs, actlist, repair, variant_frame2,
                      j, k, error, actset, num_place, 
                      replace_change, flag_adjust_moved,
                      inserted_seq, inserted_loc, method.repair){
  actset = NBGs[[1]]
  model.nodes = NBGs[[2]]
  model.edges = NBGs[[3]]
  base.list = NBGs[[4]]
  sliding_check = left_join(sliding_check, unique(variant_frame2[,c('v_id', 'trace_freq')]), by = 'v_id')
  sliding_check$inject = unlist(lapply(sliding_check$inject, FUN= function(x){paste(unlist(x), collapse = ">>")}))
  sliding_check$trace_freq = as.numeric(sliding_check$trace_freq)
  sliding_check = aggregate(sliding_check$trace_freq, by = list(sliding_check$inject), FUN= sum ) 
  
  priority1 = (unlist(lapply(strsplit(sliding_check$Group.1,split = '>>'), FUN= function(x){length(x)}))==1)
  if(sum(priority1) >0){
    sliding_check= sliding_check[which(priority1),]
  }
  
  
  if(sum(priority1) == 0 & (replace_change>0 | 
                            flag_adjust_moved ==TRUE |
                            (j==1 & length(actlist) > 3 ) )  ){ 
    skip_refuse = TRUE
    return( list(skip_refuse = skip_refuse
    ))
  }else{
    skip_refuse = FALSE
    if(method.repair =="Prob"){
      added = unlist(strsplit(sliding_check[which(sliding_check$x == max(sliding_check$x)), 'Group.1'], ">>" ))
    }else{
      dist= unlist(lapply(  strsplit(sliding_check$Group.1,split = '>>') , length ))
      sim = 1/(1+ dist)
      prob = sliding_check$x / sum(sliding_check$x)
      rank = sim * prob
      added = unlist(strsplit(sliding_check[which(rank == max(rank)), 'Group.1'], ">>" ))
    }
    actlist_repaired = c(actlist[1:error],
                         added,
                         actlist[(error+1):length(actlist)])
    
    inserted_seq[[length(inserted_seq)+1]] = added
    inserted_loc = c(inserted_loc, error)
    
    repair = data.frame( cbind (Case= as.character(repair$Case[1]), level= 1:(length(actlist_repaired)-1),
                                from= actlist_repaired[-length(actlist_repaired)] , to= actlist_repaired[-1]) )
    vote = voting_matrix(repair, actset, model.nodes, model.edges, base.list)
    if(length(which(apply(vote[[1]],2,sum)>1)) <= num_place){
      return( list(predict_pattern = 'skip', 
                   predict_pattern_base = 'skip', 
                   k = 1,
                   actlist_repaired= actlist_repaired,
                   skip_refuse = skip_refuse,
                   vote= vote,
                   repair = repair,
                   inserted_seq = inserted_seq,
                   inserted_loc = inserted_loc
      ))
    }else{
      return( list(predict_pattern = 'failed_skip', 
                   predict_pattern_base = 'failed_skip', 
                   k = k+1,
                   actlist_repaired= actlist_repaired,
                   skip_refuse = skip_refuse,
                   vote= vote,
                   repair = repair,
                   inserted_seq = inserted_seq,
                   inserted_loc = inserted_loc
      ))
    }
  }
}

recon_moved_replace = function(inserted_seq, inserted_loc, 
                               deleted_seq, deleted_loc,j, PBARs){
  
  inserted_seq_filter = inserted_seq[length(inserted_seq)]
  inserted_seq_filter = lapply(inserted_seq_filter, FUN= sort)
  deleted_seq_filter = deleted_seq[which( lapply(deleted_seq, length)<3)]
  deleted_seq_filter = lapply(deleted_seq_filter, FUN= sort)            
  check_seq = deleted_seq_filter[which(is.element(deleted_seq_filter, inserted_seq_filter))]
  
  inserted_loc_filter = inserted_loc[length(inserted_loc)]
  deleted_loc_filter = deleted_loc[which( lapply(deleted_seq, length)==1)]
  check_loc = deleted_loc_filter[which(is.element(deleted_loc_filter,  c( inserted_loc_filter, inserted_loc_filter+1) ))]
  if(length(check_seq)>0){
    jj=1
    while( !is.element(list(sort(PBARs[[jj]]$deleted_seq)) , check_seq[1]) ){
      jj= jj+1
    }
    
    predict_pattern[j] <<- 'moved'
    predict_pattern <<- predict_pattern[-jj]
    deleted_seq <<- deleted_seq[-which(is.element(deleted_seq, check_seq[1]))[1]]

  }else if(length(check_loc)>0){
    jj=1
    while( !is.element(list(sort(PBARs[[jj]]$deleted_loc)) , check_loc[1]) ){
      jj= jj+1
    }
    predict_pattern[j] <<- 'replace'
    predict_pattern <<- predict_pattern[-jj]
    deleted_loc <<- deleted_loc[-which(is.element(deleted_loc, check_loc[1]))[1]]
    replace_change <<- 0
  }
}
