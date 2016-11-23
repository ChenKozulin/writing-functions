canopy_conductance <-
function(cleaf, co2, Rnet, vdp, Tair, RLWC,StoDen, CanCond){
                                
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
                                  return(CanCond)
                              }
