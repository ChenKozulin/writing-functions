#' Penman Montieth Model with canopy conductance 
#' 
   #' THis function computer evapotranspiration based on Penman Montieth Model 
   #' including canopy conductance 
   
   #'Penman Montieth Model paramters: 
   #' @param        Tair    (deg C) air temperature
 #' @param    vpd     (Pa)    vapour pressure deficit
 #' @param     Rnet    (j/m2/day)      net radiation
 #' @param     gs      (s/mm)  surface conductance
 #' @param     ga      (s/mm)  aerodynamic conductance
 #' @param     CP  =      1010.0 (J/kg*K) specific heat of air
 #' @param    Pair = 101325 (Pa)

 #'Canopy conductance paramters:
 #' 
 #' @param Cleaf (mmol m⁻² s⁻¹) water vapor exiting through the canopy  
 #' @param CO2 % of carbon dioxide (CO2) in the environment 
 #' @param RLWC (root/leaf water content)
 #' @param StoDen (No. of stomata/mm^2 leaf) stomatal density 
 #' @author Chen
 #' @return Evapotranspiration (mm/day)
 #' @return canopy_conductance (mm/day)
 

 
   
   penman_montieth =
    +     function(Tair, vpd, Rnet, gs,ga, dayl, CP=1010, Pair=101325) 
      {
      +         
        +         #       Internal Variables
        +         #
        +         #       rho     (kg/m3)         density of air
        +         #       CP      (K/jg/degC)     specific heat of air
        +         #       lhvap   (J/kg)          latent heat of vapourization H20
        +         #       s       (Pa/degC)       slope of sat vpd vs T curve
        +         #       rs      (s/m)   surface resistance
        +         #       ra      (s/m)   aerodynamic resistance
        +         
        +         
        +         # convert Rnet to daytime value in j/m2/s
        +         Rnet = Rnet / (60*60*dayl)
        +         
          +         # convert conductance to resistance and change units
          +         rs = 1000.0/gs
          +         ra = 1000.0/ga
          +         
            +         # Assign tk (Kelvins)
            +         tk = Tair + 273.15
            +         
              +         #       Density of air (rho) as a fn. of air temp.
              +         rho = 1.292 * ( 0.00428 * Tair )
              +         
                +         #       Latent heat of vapourization as a fn. of Tair.
                +         lhvap = 2.5023e6 - 2430.54 * Tair
                +         
                  +         #       Temperature offsets for slope estimates
                  +         dt = 0.2
                  +         t1 = Tair + dt
                  +         t2 = Tair - dt
                  +         
                    +         #       Saturation vapour pressures at t1 and t2(Pa)
                    +         pvs1 = 610.7 * exp(17.38 * t1 / ( 239.0 + t1))
                    +         pvs2 = 610.7 * exp(17.38 * t2 / ( 239.0 + t2))
                    +         
                      +         #       Slope of pvs vs T curve at Tair (Pa/deg C)
                      +         s = ( pvs1 -   pvs2 ) / ( t1 -  t2 )
                      +         
                        +         #       Calculate gamma
                        +         gamma = CP * Pair / ( lhvap )
                        +         
                          +         
                          +         #       Evaporation in W/m2
                          +         et = ((s*Rnet) + (rho*CP*vpd/ra)) / (gamma*(1.0 + rs/ra) +s)
                          +         
                            +         
                            +         #       mH20/s = W/m2 * 1kgH20/lhvap J * 1m3H20/1000kGH20
                            +         ewater = ( et/ ( lhvap * 1000 ))
                            +         
                              +         # mmH20/day
                              +         ewater.day = ewater * dayl*60*60 * 1000
                              +         
                                +         # return from your function
                                +         ewater.day
                              
                              canopy_conductance= function(cleaf, co2, Rnet, vdp, Tair, RLWC,StoDen){
                                
                                +         
                                  +         # Determine leaf conductance
                                  cleaf<-runif(1,0.0, 1.0)
                                  #Determine % of CO2 in the environment
                                  CO2 <- sample(16:20, 1)
                                  #Determine RLWC
                                  RLWC<-runif(1,0.0, 1.0)
                                  #Determine StoDen
                                  StoDen<-sample(1:10, 1)
                                  CanCond = (Cleaf*StoDen)*CO2*Rnet*vpd*Tair*RLWC
                                  
                                  
                              }
                              +     }