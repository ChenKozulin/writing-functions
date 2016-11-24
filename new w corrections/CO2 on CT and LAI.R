#' Effect of Leaf Area Index (LAI) calculations on Primary Productivity Assesment 
#' 
#' The function compares Primary Productivity estimations, 
#' based on two way of calculating LAI
#'   
#' Paramters:
#'  @param LAI1 Leaf Area Index of vegitation cover: leaf area / ground area (m^2/m^2),
#'              as calculated by direct methods (averaged leaf area/ground trap area)
#'  @param LAI2 LAI as calculated by measuring the difference between light levels 
#'              above the canopy and at ground level.  
#'  @param Pmax Maximum Primary Production 
#'  @param k    Growth Coefficient
#'  
#'  @author Chen
#'  @return PrimaryProduct

Primary= function(LAI1,LAI2,Pmax,k){
  dat=res=data.frame(cbind(LAI1,LAI2,Pmax,k))
  P1=Pmax*(1-(exp(-k*LAI1)))
  P2=Pmax*(1-(exp(-k*LAI2)))
  
  return(P1,P2, plot(LAI1,P1), plot(LAI2,P2))
           
} 