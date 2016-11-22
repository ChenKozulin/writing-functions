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

# Determine leaf conductance (mmol m⁻² s⁻¹)
cleaf<- c(17.577,17.841,20.208,25.942,26.274,27.051,27.792,28.1,29.008,29.276)


#Determine % of CO2 in the environment
CO2 <- sample(16:20, 1)

#Determine RLWC (no units)
RLWC<-runif(1,0.0, 1.0)

#Determine StoDen (mm⁻²)
StoDen<-sample(1:10, 1)

#Determine Net Radiation
Rnet<-c(139.472,139.236,138.366,138.065,136.442,128.438,124.898,124.549,123.385,122.546)


#Determine Vapor Pressure Differance (VDP)
vdp<-c(0.14,0.61,1.21,1.24,1.54,1.61,1.63,1.85,2.39,2.52)

#Determine Tair
Tair<-c(34.45,33.56,32.17,32.09,31.99,31.90,30.31,29.42,29.01,28.64)

Can_Con=function(cleaf,CO2,RLWC,StoDen,Rnet,vdp,Tair){
  
  res=data.frame(cleaf,CO2,RLWC,StoDen,Rnet,vdp,Tair)
  CanCond = (cleaf*StoDen)*CO2*Rnet*vdp*Tair*RLWC
  
  return (CanCond)
  
}

