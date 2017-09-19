PropRound <- function( vec, total ){
  
  if( total > 0 & total  %% 1 != 0 ){
   
    stop("total must be a positive integer.")
     
  }
  
  reorder <- match( 1:length(vec), order(vec, decreasing = TRUE) )
  
  vec <- vec[order(vec, decreasing = TRUE)]
  
  vecnm1 <- c( total, rep( 0, length(vec) - 1 ) )
  
  scorenm1 <- PropRoundScore(vec, vecnm1)
  
  vecn <- vecnm1
  
  vecn[which.min(scorenm1)] <- vecn[which.min(scorenm1)] + 1
  
  vecn[which.max(scorenm1)] <- vecn[which.max(scorenm1)] - 1
  
  scoren <- PropRoundScore( vec, vecn )
  
  # i <- 0
  
  while( sum( scoren^2 ) < sum( scorenm1^2 ) ){
   
    scorenm1 <- scoren
    
    vecnm1 <- vecn
    
    vecn[which.min(scorenm1)] <- vecn[which.min(scorenm1)] + 1
    
    vecn[which.max(scorenm1)] <- vecn[which.max(scorenm1)] - 1
    
    scoren <- PropRoundScore( vec, vecn )
    
    # i <- i + 1
    # 
    # cat( paste0( i, "\n" ) )
    
  }
  
  out <- vecnm1[reorder]
    
  return( out )
  
}

PropRoundScore <- function(initialVec, iteratVec){
 
  score <- 
    ( ( iteratVec / mean(iteratVec) ) - ( initialVec / mean(initialVec) ) )
  
  return(score)
   
}