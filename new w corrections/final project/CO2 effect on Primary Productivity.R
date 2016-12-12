#' CO2 effect on Primary Productivity
#' 
#' The function computes estimations of Primary productivity 
#' under changing CO2 levels, based on canopy transpiration and LAI data. 
#' 
#' @param LAI Leaf Area Index of vegitation cover: leaf area / ground area (m^2/m^2),
#' @param Pmax Maximum Primary Production 
#' @param k    Growth Coefficient

PP=function(LAI,aCO2,Pmax,k){
  if (aCO2>16){
    k=k*2
    else k=k
  } 
  P=Pmax*(1-(exp(-k*LAI)))
  
  return(P,plot(LAI1,P1), plot(LAI2,P2))
}