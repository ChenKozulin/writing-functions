Can_Con <-
function(cleaf,CO2,RLWC,StoDen,Rnet,vdp,Tair){
  
  res=data.frame(cleaf,CO2,RLWC,StoDen,Rnet,vdp,Tair)
  CanCond = (Cleaf*StoDen)*CO2*Rnet*vpd*Tair*RLWC
  cond=ifelse(Tair < 0, 0, cond)
  
  return(cond)
}
