cc <-
function(cleaf, co2, Rnet, vdp, Tair, RLWC,StoDen) {
                                CanCond = (Cleaf*StoDen)*CO2*Rnet*vpd*Tair*RLWC
                                cond=ifelse(Tair < 0, 0, cond)
                                
                                return(cond)
                              }
