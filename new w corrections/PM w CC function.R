#' Penman Montieth Model with canopy conductance 
#' 
#' THis function computer evapotranspiration based on Penman Montieth Model 
#' including canopy conductance 
#' Canopy conductance paramters:
#' 
#' @param Cleaf (mmol m⁻² s⁻¹) water vapor exiting through the canopy  
#' @param CO2 % of carbon dioxide (CO2) in the environment 
#' @param RLWC (root/leaf water content)
#' @param StoDen (No. of stomata/mm^2 leaf) stomatal density 
#' @author Chen
#' @return canopy_conductance (mm/day)

CanCon=function(cleaf,CO2,RLWC,StoDen,Rnet,vdp,Tair) {
  
  res=data.frame(cbind(cleaf,CO2,RLWC,StoDen,Rnet,vdp,Tair))
  CanCond = (cleaf*StoDen)*CO2*Rnet*vdp*Tair*RLWC
  
  return (CanCond)
  
}

