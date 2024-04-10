




NBG = function(clean_data, alpha= 0, beta= 0){
  dat= clean_data
  actset  = unique(dat$Activity)
  model.edges =list()
  model.nodes = list()
  base.list = as.numeric()
  visNBGs = list()
  
  apply_NBG = function(actset,dat, alpha= 0, beta= 0){
    actsel = actset
    case_p = unique(dat[which(dat$Activity == actsel),'Case'])
    dat_p = dat[which(is.element(dat$Case, case_p)),]
    one = rep(1, nrow(dat_p))
    cumone = ave(one, by= as.factor(dat_p[,1]), FUN= cumsum)
    dat_p2 = data.frame(dat_p[1:(nrow(dat_p)-1),1],dat_p[1:(nrow(dat_p)-1),3],
                        dat_p[1:(nrow(dat_p)-1),2], dat_p[2:nrow(dat_p),2])
    del = cumone[1:(nrow(dat_p)-1)] - cumone[2:nrow(dat_p)]
    dat_p3 = dat_p2[which(del<0),]
    dat_p3$BA = rep(1, nrow(dat_p3))
    
    loc = aggregate( as.character(dat_p3[,3]), by=list(as.character(dat_p3[,1])), 
                     FUN= function(x) which(x==as.character(actsel)))
    loc2 = aggregate( as.character(dat_p3[,4]), by=list(as.character(dat_p3[,1])), 
                      FUN= function(x) which(x==as.character(actsel)))
    
    if( sum(class(loc[,2])!="integer") != 0 & sum(class(loc2[,2])!='integer') != 0 ){
      
      if( sum(class(loc[,2])== c('matrix', 'array')) ==2 ){
        case_p1 = data.frame(NA,NA)
        case_p2 = data.frame(NA,NA)
        names(case_p1) = c("Case",'loc')
        names(case_p2) = c("Case",'loc')
        case_p3 = (loc[,1])
        leng_p3 = apply(loc[,2], 1,FUN= length)
        case_p3 = rep(case_p3, as.numeric(leng_p3))
        case_p3 = data.frame( Case =case_p3 , loc =as.vector(t(loc[,2])) )
      }else{
        case_p1 = data.frame( Case = unique((loc[,1])[ which(  as.numeric(summary(loc[,2])[,1]) ==0)]) , 
                              loc = unlist(((loc2[,2])[ which(  as.numeric(summary(loc[,2])[,1]) ==0 & as.numeric(summary(loc2[,2])[,1]) ==1  )]))+1) 
        case_p2 = data.frame( Case = ((loc[,1])[ which( as.numeric(summary(loc[,2])[,1]) ==1)]) 
                              , loc = unlist(((loc[,2])[ which(  as.numeric(summary(loc[,2])[,1]) ==1)]))) 
        case_p3 = (loc[,1])[ which( as.numeric(summary(loc[,2])[,1]) >1)] 
        leng_p3 = (summary(loc[,2])[,1])[which( as.numeric(summary(loc[,2])[,1]) >1)]
        case_p3 = rep(case_p3, as.numeric(leng_p3))
        case_p3 = data.frame( Case =case_p3 , loc = unlist((loc[,2])[which( as.numeric(summary(loc[,2])[,1]) >1)]))  
      }
    }else if(class(loc[,2])!="integer" & class(loc2[,2])=='integer'){
      case_p1 = data.frame(NA,NA)
      case_p1 = case_p1[-1,]
      names(case_p1) = c("Case",'loc')
      if(sum( as.numeric(summary(loc[,2])[,1]) ==0 )>0){
        case_p1 = data.frame( Case = unique((loc[,1])[ which(  as.numeric(summary(loc[,2])[,1]) ==0)]) , 
                              loc = unlist(((loc2[,2])[ which(  as.numeric(summary(loc[,2])[,1]) ==0 )]))+1) 
      }
      case_p2 = data.frame( Case = ((loc[,1])[ which( as.numeric(summary(loc[,2])[,1]) ==1)]) 
                            , loc = unlist(((loc[,2])[ which(  as.numeric(summary(loc[,2])[,1]) ==1)]))) 
      case_p3 = (loc[,1])[ which( as.numeric(summary(loc[,2])[,1]) >1)] 
      leng_p3 = (summary(loc[,2])[,1])[which( as.numeric(summary(loc[,2])[,1]) >1)]
      case_p3 = rep(case_p3, as.numeric(leng_p3))
      case_p3 = data.frame( Case =case_p3 , loc = unlist((loc[,2])[which( as.numeric(summary(loc[,2])[,1]) >1)])) 
    }else if(class(loc[,2])=="integer" & class(loc2[,2])!='integer'){
      case_p1 = data.frame(NA,NA)
      case_p1 = case_p1[-1,]
      names(case_p1) = c("Case",'loc')
      case_p2 = data.frame( Case = loc[,1], loc = loc[,2])
      case_p3 = data.frame(NA,NA)
      case_p3 = case_p3[-1,]
      names(case_p3) = c("Case",'loc')
    }else{
      case_p1 = data.frame(NA,NA)
      case_p1 = case_p1[-1,]
      names(case_p1) = c("Case",'loc')
      case_p2 = data.frame( Case =loc[,1], loc = loc[,2])
      case_p3 = data.frame(NA,NA)
      case_p3 = case_p3[-1,]
      names(case_p3) = c("Case",'loc')
    }
    
    case_pp = rbind(case_p1, case_p2, case_p3) 
    names(dat_p3) = c('Case', 'level', "from", "to", 'BA')
    dat_p3 = merge( dat_p3, case_pp, by='Case', all.y=T)
    dat_p3 = dat_p3[which(!is.na(dat_p3$Case)),]
    dat_p3$max = ave(dat_p3[,2] , by=dat_p3[,1] , FUN=length )
    
    if(length(unique(dat_p3[,2]))>1){
      base = max(dat_p3[,2])
    }else{
      base = unique(dat_p3[,2])
    }
    dat_p3$level = dat_p3$level + base - dat_p3$loc
    TF = (dat_p3[,2] >= base )
    dat_p3$TF = TF
    dat_pre = dat_p3[which(dat_p3$TF == 0),]
    dat_aft =  dat_p3[which(dat_p3$TF == 1),]
    dat_pre2 = dat_pre[,2:4]
    dat_aft2 = dat_aft[,2:4]
    dat_pre2$freq = ave( rep(1, nrow(dat_pre2)) , by = list(dat_pre2[,1],dat_pre2[,2], dat_pre2[,3]), FUN=sum)
    dat_aft2$freq = ave( rep(1, nrow(dat_aft2)) , by = list(dat_aft2[,1],dat_aft2[,2], dat_aft2[,3]), FUN=sum)
    
    ######## previous tree
    if(nrow(dat_pre)>0){  
      dat_sel = unique(dat_pre2)
      names(dat_sel) = c("level", "from", "to", "freq")
      dat_sel.1 = dat_sel[,1:2]
      dat_sel.2 = dat_sel[,c(1,3)]
      names(dat_sel.2)[2]="from"
      dat_sel.2[,1] = as.factor(as.numeric(dat_sel.2[,1]) +1)
      dat_sel2 =rbind(dat_sel.1, dat_sel.2)
      df = aggregate(dat_sel2[,2], by=list(dat_sel2[,1]), FUN=function(x) unique(as.character(x)) )
      
      if(class(df[,2]) =="list" ){
        from = unlist(df[,2])
        level =rep(df[,1], as.numeric(summary(df[,2])[,1]))
      }else if(class(df[,2]) =="matrix"){
        from = as.vector(df[,2] )
        level =rep(df[,1], apply(df[,2],1,length))
      }else{
        from = unlist(df[,2])
        level =df[,1] 
      }
      
      id = 1:length(from)
      nodes <- data.frame(id,from, level = level)
      nodes= unique(nodes)
      names(dat_sel)[1:3] = c('level', "from","to")
      dat_sel$level = as.factor(dat_sel$level )
      nodes$level = as.factor(nodes$level )
      nodes$id = as.factor(nodes$id )
      dat_sel_p = dat_sel
      dat_sel_p[,1] = as.numeric(as.character(dat_sel_p[,1]))
      dat_sel_p[,1] = dat_sel_p[,1]+1 
      dat_sel$level2 = dat_sel_p[,1]
      tt = merge(dat_sel, nodes, by=c("from",'level'), all.x=T)
      names(nodes) = c("id2","to","level2")
      ttt = merge(tt, nodes, by=c("to",'level2'), all.x=T)
      names(nodes) = c("id","from","level")
      
      edges_pre <- data.frame(
        from = ttt$id,
        to = ttt$id2,
        value = ttt$freq,
        label= as.character(ttt$freq)
      )
      nodes_pre = nodes[which(is.element(nodes[,1],  unique(as.vector(as.matrix(edges_pre[,1:2])))) ),]
      edges_pre4 = merge(edges_pre, nodes_pre[,c(1,3)] , by.x = "from", by.y = "id", all.x=T)
      edges_pre4$total =ave( as.numeric(as.character(edges_pre4$label)) , by= paste(edges_pre4$level, edges_pre4$to), FUN= sum)
      edges_pre4$weight = edges_pre4$value/edges_pre4$total 
      id_save= id
    }
    
    if(nrow(dat_aft)>0){
      dat_sel = unique(dat_aft2)
      names(dat_sel) = c("level", "from", "to", "freq")
      dat_sel.1 = dat_sel[,1:2]
      dat_sel.2 = dat_sel[,c(1,3)]
      names(dat_sel.2)[2]="from"
      dat_sel.2[,1] = as.factor(as.numeric(dat_sel.2[,1]) +1)
      dat_sel2 =rbind(dat_sel.1, dat_sel.2)
      df = aggregate(dat_sel2[,2], by=list(dat_sel2[,1]), FUN=function(x) unique(as.character(x)) )
      from = unlist(df[,2])
      if(class(df[,2]) =="list"){
        level =rep(df[,1], as.numeric(summary(df[,2])[,1]))}else{level =df[,1] }
      if(nrow(dat_pre)>0){
        id = (max(id_save)+1):(max(id_save)+length(from))
      }else{
        id = 1:length(from)
      }
      nodes <- data.frame(id,from, level = level)
      nodes= unique(nodes)
      names(dat_sel)[1:3] = c('level', "from","to")
      dat_sel$level = as.factor(dat_sel$level )
      nodes$level = as.factor(nodes$level )
      nodes$id = as.factor(nodes$id )
      
      dat_sel_p = dat_sel
      dat_sel_p[,1] = as.numeric(as.character(dat_sel_p[,1]))
      dat_sel_p[,1] = dat_sel_p[,1]+1 
      dat_sel$level2 = dat_sel_p[,1]
      tt = merge(dat_sel, nodes, by=c("from",'level'), all.x=T)
      names(nodes) = c("id2","to","level2")
      ttt = merge(tt, nodes, by=c("to",'level2'), all.x=T)
      names(nodes) = c("id","from","level")
      
      edges_aft <- data.frame(
        from = ttt$id,
        to = ttt$id2,
        value = ttt$freq,
        label= as.character(ttt$freq)
      )
      
      nodes_aft = nodes[which(is.element(nodes[,1],  unique(as.vector(as.matrix(edges_aft[,1:2])))) ),]
      edges_aft4 = merge(edges_aft, nodes_aft[,c(1,3)] , by.x = "from", by.y = "id", all.x=T)
      edges_aft4$total =ave( as.numeric(as.character(edges_aft4$label)) , by= paste(edges_aft4$level, edges_aft4$from), FUN= sum)
      edges_aft4$weight = edges_aft4$value/edges_aft4$total 
    }
    
    if(nrow(dat_pre)==0){
      edges = edges_aft4
      nodes =  nodes_aft
      edges_save=  edges_aft
    }else if(nrow(dat_aft)==0){
      edges= edges_pre4
      nodes = nodes_pre
      edges_save= edges_pre
    }else{
      edges = rbind(edges_pre4, edges_aft4)
      nodes = rbind(nodes_pre, nodes_aft)
      edges_save= rbind(edges_pre, edges_aft)
    }
    
    edges_cut = edges[which( edges$weight > alpha & edges$value > beta),1:5]
    nodes_cut = nodes[which(is.element(nodes[,1],  unique(as.vector(as.matrix(edges_cut[,1:2])))) ),]
    ###For visualization
    show1 =visNetwork(nodes_cut, edges_cut, width = "100%",height = "800px") %>%  visNodes(shape = "square") %>%
      visEdges(smooth = T) %>%
      visEdges(shadow = TRUE,
               arrows =list(to = list(enabled = TRUE, scaleFactor = 2)),
               color = list(color = "black", highlight = "red")) %>%
      visHierarchicalLayout(levelSeparation = 150)
    
    result = list(edges_cut,nodes_cut, base, show1)
    setNames(result,c("e","n","b", "vis"))
    return( result )
  }
  
  
  
  vtree =foreach(i=actset ,.packages = c("visNetwork", "dplyr" )) %dopar% apply_NBG(i ,dat, alpha, beta) 
  
  # vtree = lapply(actset, FUN = function(x){apply_NBG(x,dat, alpha, beta)})

  model.edges = lapply(vtree, '[[', 1)
  model.nodes = lapply(vtree, '[[', 2)
  base.list = unlist(lapply(vtree, '[[', 3))
  visNBGs = lapply(vtree, '[[', 4)
  return( list( actset, model.nodes, model.edges, base.list, visNBGs) )
}



