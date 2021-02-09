# Not used

estimateCE <- function(datatset, covariateList, treatment, outcome, ignore=FALSE){
  
  #stratify data into different levels of covariates. Levels that do not occur should be dropped
  
  strata <- split(datatset, datatset[covariateList], drop = TRUE) 
  
  strataSize = nrow(datatset)
  
  effect <- 0
  
  accumSize <- 0
  
  for (stratum in strata){
    
	##treatment group
    treatmentSet <- dplyr::filter (stratum, !!as.name(treatment ) == 1)
    treatmentSetY <- dplyr::filter (stratum, !!as.name(treatment ) == 1, !!as.name(outcome ) == 1)
    
    treatNbr <- nrow(treatmentSet)
    
	## We may change this, depending on what we want
    if(treatNbr == 0){
      treatmentMean <- 0
      if(ignore){
        next
      }
      
    }
    else
	
      treatmentMean <- nrow(treatmentSetY)/treatNbr
    
    ##control group
    controlSet <- dplyr::filter (stratum, !!as.name(treatment ) == 0) 
    controlSetY <- dplyr::filter (stratum, !!as.name(treatment ) == 0, !!as.name(outcome ) == 1)
    
    contrNbr <- nrow(controlSet)
    
    if(contrNbr == 0 ){
      
      controlMean <- 0
      
      if(ignore){
        next
      }
      
    }

    else
      controlMean <-  nrow(controlSetY)/contrNbr
    
    stratumSize = nrow(stratum)
    accumSize = accumSize + stratumSize
    
    effect<- effect + (treatmentMean - controlMean)* stratumSize
    
  }

  effect <- effect/accumSize
  
  return (effect)
  
}