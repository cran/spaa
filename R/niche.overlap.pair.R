niche.overlap.pair <-
function(vectA, vectB, method = c("levins","schoener","petraitis","pianka","czech","morisita") ){ 
      match.arg(method )
	  nij <- vectA
	  pij <- nij/sum(nij)
      
	  nkj <- vectB
	  pkj <- nkj/sum(nkj)
	  if (method == "levins"){
	      upper <- sum(pij * pkj)
	      lower <- sum(pij^2)
	      result <- upper/lower
	     }
	       else {
	            if(method == "schoener"){
	       	      result <- 1 - sum(abs(pij - pkj))/2
	               } 
	     		  else{
	                    if(method == "petraitis"){
                            nij <- nij[(vectA > 0) & (vectB > 0)]
	                        pij <- nij/sum(nij)
	                        nkj <- nkj[(vectA > 0) & (vectB > 0)]
	                        pkj <- nkj/sum(nkj)
	                        Eik <- sum(pij*log(pkj))-sum(pij*log(pij))
	                        result <- exp(Eik)
	     			       } 
	     			        else {if(method == "pianka"){
	     	                	      result <- sum(pij*pkj)/sqrt((sum(pij^2)) * (sum(pkj^2)))
	     	                       } 
	     			               else{ if(method == "czech"){
	     	  	                           result <- 1 - (sum(abs(pij - pkj)))/2
	     	                              }
	     	                               else { if(method =="morisita"){
	     	  	                                     result <- 2*sum(pij*pkj)/((sum(pij^2)) + (sum(pkj^2)))
	                                                  }
	     	                                     }
	     	                            
	     								}
	                      
	                             }  
                         
	     				}
         
	     		}
return(round(result,3))
}

