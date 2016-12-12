#' #'The function examines the effect of Leaf Area Index (LAI) 
#' calculations on Primary Productivity Assesment under different atmospheric CO2 levels 
#' 
#' 
#' Paramters:
#'  @param LAI1a Leaf Area Index of vegitation cover: leaf area / ground area (m^2/m^2),
#'              as calculated by direct methods (averaged leaf area/ground trap area)
#'              from ambient CO2 environment
#'              
#'  @param LAI1e Leaf Area Index of vegitation cover: leaf area / ground area (m^2/m^2),
#'              as calculated by direct methods (averaged leaf area/ground trap area)
#'              from elevated CO2 environment
#'              
#'  @param LAI2a LAI as calculated by measuring the difference between light levels 
#'              above the canopy and at ground level.
#'              from ambient CO2 environment
#'              
#'  @param LAI2e LAI as calculated by measuring the difference between light levels 
#'              above the canopy and at ground level.
#'              from elevated CO2 environment
#'                          
#'  @param Pmax Maximum Primary Production 
#'  
#'  @param k    Growth Coefficient
#'   
#'              
#'  
#'  @author Chen
#'  @return PrimaryProduct

Primary= function(LAI1a,LAI1e,LAI2a,LAI2e,Pmax,k){
  dat=res=data.frame(cbind(LAI1a,LAI1e,LAI2a,LAI2e,Pmax,k))
  P1a=Pmax*(1-(exp(-k*LAI1a)))
  P1e=Pmax*(1-(exp(-k*LAI1e)))
  
  P2a=Pmax*(1-(exp(-k*LAI2a)))
  P2e=Pmax*(1-(exp(-k*LAI2e)))
  
  return(P1a,P1e,P2a,P2e, plot(LAI1a,P1a), plot(LAI2a,P2a),plot(LAI1e,P1e), plot(LAI2e,P2e) )
  
} 