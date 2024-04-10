

# Update e2_set
update_e2_set = function(e2){
  e2_set = list()
  n = 1
  set = e2[1]
  if(length(e2)>1){
    for(i in e2[2:length(e2)]){
      if(i == set[length(set)]+1){
        set = c(set, i)
      }else{
        e2_set[[n]] = set
        set = i
        n = n+1
      }
    }
    e2_set[[n]] = set
  }else{
    if(length(e2)==1){
      e2_set[[1]] = set
    }
  }
  return(e2_set)
}

