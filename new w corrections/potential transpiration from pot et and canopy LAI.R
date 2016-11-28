#' Estimation of potential Transpiration from potential evapotranspiration 
#' and canopy leaf area index 
#' 
#' The function computes the expected canopy transpiration from data of potential evapotranspiration 
#' and canopy leaf area index
#' 
#'Novák and Havrila (2005) Transpiration of Plants: A Review of Calculation Methods. Geophysical Research Abstracts, Vol. 7, 07181
#'SRef-ID: 1607-7962/gra/EGU05-A-07181
#'   
#'  note that the use of the PM model to assess transpiration is 
#'  limited by the poor ability to calculate canopy resistance, 
#'  which is influenced by a variety environmental parameters.
#'  Instead, they propose to use a semi-emperical model developed by Novak (1995)
#'  According to this model, potential transpiration (Etp)can be calculated 
#'  as a function of potential evapotranspiration (Ep) and canopy leaf area index (ω0).
#'  
#'      Paramters
#'      @param Ep   potential evapotranspiration
#'      @param beta canopy leaf area index coefﬁcient (β = 0.463 is valid for majority of agricultural canopies; 0.45<β <0.55.)
#'      @param omega0  canopy leaf area index 
#'      
#'      @author Chen
#'      @return Etp potential transpiration 
#'      
PotTransp=function(Ep,beta,omega0){
  Etp=Ep*exp(1-exp(-beta*omega0))
  return (Etp, plot(omega0,Etp))
  return(list(PotEt=Ep,canopyLAI=omega0,Pot_transpiration=Etp))
}