#'Transpiration form Yield
#'
#'The function computes the expected canopy transpiration from biomass yeild.
#'According to: Novak, V., & Van Genuchten, M. T. (2008). Using the transpiration 
#'regime to estimate biomass production. Soil science, 173(6), 401-407.
#'yeild could be calculated by Y=kt*Et-Y0, from which Et=(Y+Y0)/kt  
#'where Y is dry biomass produced per unit area (t/ha)
#'kt is the crop water use efficiency (Hillel and Guron,1973). 
#'Et is the transpiration total over the growing season of the particular canopy (cm)
#'Y0 denotes the intersection of the equation with the vertical axis.
#'
#'Paramters
#'@param Y       dry biomass produced per unit area (t/ha)
#'@param kt      crop water use efficiency (t/ha/cm)
#'@param Y0      intersection of the equation with the vertical axis
#'
#'@author Chen
#'
#'@return Et (canopy transpiration)

TranspirationYeild= function(Y,kt,Y0){
  data=data.frame(cbind(Y,kt,Y0))
  Et=(Y+Y0)/kt
  return(Et, plot(Et,Y))
}
  
