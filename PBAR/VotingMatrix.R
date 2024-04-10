
# Geneweight token score based on tree model
voting_matrix = function(obs_case, actset, model.nodes, model.edges, base.list){
  actlist = c( as.character(obs_case[,3]),'End')
  obs_case$level = as.numeric(obs_case$level)

  nodes_cut.list=model.nodes[which(is.element(actset, actlist))]
  edges_cut.list=model.edges[which(is.element(actset, actlist))]

  base.df = data.frame(actset[which(is.element(actset, actlist))],  base.list[which(is.element(actset, actlist))] )
  obs_case1 = obs_case
  token= as.numeric()
  
  if(length(actset[is.element(actset, actlist)])==length(unique(actlist))){
    for(j in 1:length(actlist)){
      obs_case1 = obs_case[,c(3,4,2)]
      if(actlist[j] ==actlist[length(actlist)] ){
        obs_case1[,3] = obs_case1[,3] + base.df[which(base.df[,1] ==actlist[j]),2] - obs_case1[nrow(obs_case1),3] - 1 
      }else{
        obs_case1[,3] = obs_case1[,3] + base.df[which(base.df[,1] == actlist[j]),2] - obs_case1[j,3]
      }
      
      obs_case2 = paste(obs_case1[,1] , obs_case1[,2] , sep ='>>')
      obs_case1 = paste(obs_case1[,1] , obs_case1[,2], obs_case1[,3] ,sep='-')


      nodes_cut = nodes_cut.list[[which(base.df[,1] ==actlist[j])]]
      edges_cut.eng= edges_cut.list[[which(base.df[,1] ==actlist[j])]]


      names(edges_cut.eng)[1] = "id"
      df <- left_join(edges_cut.eng, nodes_cut[,1:2], by = "id"  )
      names(df)[1:2] = c("FROM", "id")
      df2 = left_join(df, nodes_cut[,1:2], by = "id"  )
      edges_cut.eng = df2[,c(6,7,3,4,5)]
      names(edges_cut.eng) = c("from","to","value","label","level")
      edges_cut.eng = edges_cut.eng[,c(1,2,3,5)]
      # previous
      edges_cut.eng = paste(edges_cut.eng[,1],edges_cut.eng[,2],edges_cut.eng[,4], sep='-' )
      token= rbind(token,  !is.element(obs_case1, edges_cut.eng))
      
    }
  }else{
    for(j in which(is.element(actlist, actset))){
      obs_case1 = obs_case[,c(3,4,2)]
      if(actlist[j] ==actlist[length(actlist)] ){
        obs_case1[,3] = obs_case1[,3] +  base.df[which(base.df[,1] ==actlist[j]),2] - obs_case1[nrow(obs_case1),3] - 1 
      }else{
        obs_case1[,3] = obs_case1[,3] +  base.df[which(base.df[,1] ==actlist[j]),2] - obs_case1[j,3]
      }
      
      obs_case2 = paste(obs_case1[,1] , obs_case1[,2] , sep ='>>')
      obs_case1 = paste(obs_case1[,1] , obs_case1[,2], obs_case1[,3] ,sep='-')
      nodes_cut = nodes_cut.list[[which(base.df[,1] ==actlist[j])]] 
      edges_cut.eng= edges_cut.list[[which(base.df[,1] ==actlist[j])]]
      names(edges_cut.eng)[1] = "id"
      df <- left_join(edges_cut.eng, nodes_cut[,1:2], by = "id"  )
      names(df)[1:2] = c("FROM", "id")
      df2 = left_join(df, nodes_cut[,1:2], by = "id"  )
      
      edges_cut.eng = df2[,c(6,7,3,4,5)]
      names(edges_cut.eng) = c("from","to","value","label","level")
      edges_cut.eng = edges_cut.eng[,c(1,2,5)]
      edges_cut.eng = paste(edges_cut.eng[,1],edges_cut.eng[,2],edges_cut.eng[,3], sep='-' )
      token= rbind(token,  !is.element(obs_case1, edges_cut.eng))
    }
    for(g in 1:sum(!is.element(actlist, actset[is.element(actset, actlist)]))){
      loc = which(!is.element(actlist, actset[is.element(actset, actlist)]))
      token= rbind(token[1:(loc[g]-1),],rep(1, ncol(token)), token[loc[g]:nrow(token),])
    }
  }
  for(i in 1:nrow(token)){
    token.loc = which(token[i,]==1)
    aft = min(token.loc[token.loc >=i])
    pre = max(token.loc[token.loc<i])
    if(aft==Inf){aft=numeric()}
    if(pre==-Inf){pre=numeric()}
    token.loc2= c(aft, pre)
    token[i,-token.loc2]=0
  }
  return(list(token, obs_case2))
}




fill_partial = function(vote, e1, e2, error_save){
  e2_min = e2[which(diff(e2) > 1) ]
  e2_max = e2[which(diff(e2) > 1)+1 ]
  e1_diff1 = which(diff(e1) != 1)
  e1_in = list()
  if(length(e1_diff1)== 0 ){
    e1_in[[1]] = e1
  }else{
    e1_diff2 = unique(c(1, e1_diff1, length(e1) ))
    for(i in 1:(length(e1_diff2)-1)){
      e1_in[[i]] = e1[e1_diff2[i]:e1_diff2[i+1] ]
    }
  }
  
  
  for(i in 1:length(e1_in)){
    vote_adj = vote[[1]]
    e1_temp = e1_in[[i]]
    e1_min = min(e1_temp) -1
    e1_max = max(e1_temp)
    e2_temp = e2[which(e2 >= e1_min & e2 <= e1_max)]
    e2_min = max(-5, e2_temp[which(diff(e2_temp) > 1) ] )  # max: to prevent empty vector
    e2_max = max(-1, e2_temp[which(diff(e2_temp) > 1)+1 ])

    if( e2_max - e2_min < 3 &  e2_max - e2_min >0 ){
      if(sum(vote_adj[e2_min:(e2_max+1) , (e2_min+1):(e2_max-1) ]) == 0 &
         sum(vote_adj[e2_min:(e2_max+1) , (e2_min):(e2_max) ]) == 2*(e2_max -e2_min+1 ) &
         sum(is.element(e2, error_save)) == 0 ){# to prevent to crash with deep survey
        vote_adj[e1_temp , (min(e1_temp)):(max(e1_temp)-1) ] =1
        
        e2 <<- which(apply(vote_adj,2,sum)>1)
        flag_adjust <<- TRUE
        flag_adjust_moved <<- TRUE
      }
      
    } 
  }
  
}

