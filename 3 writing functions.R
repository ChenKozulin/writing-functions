#' Penman Montieth Model with canopy conductance 
#' 
#' THis function computer evapotranspiration based on Penman Montieth Model 
#' including canopy conductance 
#' Canopy conductance paramters:
#' 
#' @param Cleaf (mmol m⁻² s⁻¹) water vapor exiting through the canopy  
#' @param Carbon % of carbon dioxide (CO2) in the environment 
#' @param RLWC (root/leaf water content)
#' @param StoDen (No. of stomata/mm^2 leaf) stomatal density 
#' @author Chen
#' @return canopy_conductance (mm/day)

# Determine leaf conductance (mmol m⁻² s⁻¹)
cleaf<- c(17.577,17.841,20.208,25.942,26.274,27.051,27.792,28.1,29.008,29.276)
#Determine % of CO2 in the environment
Carbon <- c(16.937,17.893,18.817,19.765,20.745,20.683,18.680,17.647,16.609)
#Determine LRWC (no units)
LRWC<-c(69.70,68.54,67.94,67.7,67.29,67.06,66.86,66.73,66.57)
#Determine StoDen (mm⁻²)
STDN<-c(25.95,26.74,27.69,28.79,29.19,30.33,31.99,32.16,32.38)

Canop_Conductance=function (cleaf,Carbon,LRWC,STDN,Rnet,vdp,Tair){
  conductance= (cleaf*Carbon*LRWC*STDN*Rnet*vdp*Tair)
  result=data.farme(cleaf,Carbon,LRWC,STDN)
  
  return(conductance)
  return(plot(conductance,result$Carbon))
}

