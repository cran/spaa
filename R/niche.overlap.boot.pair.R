niche.overlap.boot.pair <-
function (vectorA, vectorB, method = c("levins","schoener","petraitis","pianka","czech","morisita"), times = 999, quant = c(0.025, 0.975)) 
{
    if(!length(vectorA)==length(vectorB)){
	  stop("Length of the input vectors not equal")
	}
	match.arg(method)
    booted <- rep(NA, times)
    for (i in 1:times) {
	    if(method == "levins"){
		    obs <- niche.overlap.pair(vectorA, vectorB, method ="levins")
			booted[i] <- niche.overlap.pair(sample(vectorA, replace = TRUE),sample(vectorB, replace = TRUE), method ="levins")
		    }
    	    else {
		        if(method == "schoener"){
     		      obs <- niche.overlap.pair(vectorA, vectorB, method ="schoener")
				  booted[i] <- niche.overlap.pair(sample(vectorA, replace = TRUE),sample(vectorB, replace = TRUE), method ="schoener")
		          } 
			     else{
    	             if(method == "petraitis") {
		              obs <- niche.overlap.pair(vectorA, vectorB, method ="petraitis")
					  booted[i] <- niche.overlap.pair(sample(vectorA, replace = TRUE),sample(vectorB, replace = TRUE), method ="petraitis")
                      }
		              else{
    	                 if(method == "pianka"){
		                   obs <- niche.overlap.pair(vectorA, vectorB, method ="pianka")
						   booted[i] <- niche.overlap.pair(sample(vectorA, replace = TRUE),sample(vectorB, replace = TRUE), method ="pianka")
                           }
                            else{		  
    	                     if(method == "czech"){       
		                        obs <- niche.overlap.pair(vectorA, vectorB, method ="czech")
								booted[i] <- niche.overlap.pair(sample(vectorA, replace = TRUE),sample(vectorB, replace = TRUE), method = "czech")
							   }
                                else{		  
    	                           if(method == "morisita"){    
		                                   obs <- niche.overlap.pair(vectorA, vectorB, method ="morisita")
										   booted[i] <- niche.overlap.pair(sample(vectorA, replace = TRUE),sample(vectorB, replace = TRUE), method = "morisita")
                                           }							  
    	                              }
		                        
								}
		                  
						  }
		        
				   }
		    
			}
		
     }
    result <- c(obs, mean(booted),sd(booted), quantile.default(booted, quant, na.rm = TRUE), times)
	names(result) <- c("Observed","Boot mean","Boot std","Boot CI1", "Boot CI2", "times")
	return(round(result,3))
}

