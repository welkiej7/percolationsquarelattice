##Hoshen - Kopelman Algorithm. 

hoshen_kopelman <- function(square_matrix_size,prob, seed = NULL, plot = TRUE){
  ## Pre-Amble
  require(tidyverse)
  require(plot.matrix)
  require(pals)
  if(!is.null(seed)){
    set.seed(seed)
  }
  #Condition Check
  if(!is.numeric(prob) | ( prob > 1 | prob < 0 )){
    stop(paste("Probability must be numeric between 0 and 1"))
  }
  if(!is.numeric(square_matrix_size)){
    stop(paste("Square matrix size must be numeric."))
  }
  #Start
  percolation_matrix <- matrix(NA_real_, nrow = square_matrix_size, 
                               ncol = square_matrix_size)
  for(i in 1:nrow(percolation_matrix)){
    percolation_matrix[i,] <- rbinom(square_matrix_size,1,prob)
  }
  # Visualize the Initial Matrix 
  initial_matrix <- percolation_matrix
  # Variation of Hoshen Kopelman
  k = 1
  for(row in 1:nrow(percolation_matrix)){
    for(col in 1:ncol(percolation_matrix)){
      if(col == 1){
        if(percolation_matrix[row,col] != 0){
          percolation_matrix[row,col] <- k
          k <- k + 1
        } else {
          next()
        }
      } else {
        if(percolation_matrix[row,col] == 0){
          next()
        } else if(percolation_matrix[row,col] != 0 & percolation_matrix[row,col-1] == 0){
          percolation_matrix[row,col] <- k
          k <- k + 1
        } else if(percolation_matrix[row,col] !=0 & percolation_matrix[row,col -1] != 0){
          percolation_matrix[row,col] <- percolation_matrix[row,col-1]
        } else {
          next()
        }
      }
    }
  }
  for(row in 1:nrow(percolation_matrix)){
    for(col in 1:ncol(percolation_matrix)){
      if(row == 1){
        next()
      } else {
        if(percolation_matrix[row,col] != 0 & percolation_matrix[row-1,col] != 0){
          percolation_matrix[row,col] -> percolation_matrix[row-1,col] # Took the upper value
          
          if(col != 1){
            if(percolation_matrix[row,col-1] != 0){
            percolation_matrix[row,col-1] -> percolation_matrix[row,col]
            }
          }
          if(col != ncol(percolation_matrix)){
          if(percolation_matrix[row,col + 1] != 0 & col != ncol(percolation_matrix)){
            percolation_matrix[row,col + 1] -> percolation_matrix[row,col]
          }
          }
        }
      }
    }
  }
  for(row in 1:nrow(percolation_matrix)){
    for(col in 1:ncol(percolation_matrix)){
      if(row == 1){
        next()
      }
      if(percolation_matrix[row,col] != 0 & percolation_matrix[row -1 , col] != 0){
        percolation_matrix[row,col] <- percolation_matrix[row-1,col]
      }
    }
  }
 for(row in 1:nrow(percolation_matrix)){
   for(col in 1:ncol(percolation_matrix)){
     if(col == ncol(percolation_matrix)){
       next()
     }
     if(percolation_matrix[row,col] != 0 &  percolation_matrix[row, col + 1] != 0){
      percolation_matrix[which(percolation_matrix == percolation_matrix[row,col])] <- percolation_matrix[row, col + 1]
     }
   }
 }
secondary_matrix <- percolation_matrix
par(mfrow = c(1,2))
percolates = FALSE
tt <- 0
for(elem in unique(c(percolation_matrix))){
  if(elem == 0){
    next()
  }
  for(row in 1:nrow(percolation_matrix)){
    if(elem %in% percolation_matrix[row,]){
      tt <- tt + 1
      if(tt == nrow(percolation_matrix)){
        print("Percolates")
        percolates = TRUE
        break()
      }
    } else {
      tt <- 0
    }
  }
}
if(plot){
plot(initial_matrix, main = "Binary", col = unname(polychrome()),key = NULL)
plot(percolation_matrix,col = c(unname(polychrome()),unname(glasbey())) , main = paste("Percolates: ",percolates,"|", "Number of Clusters: ",
                                                                                       length(unique(c(percolation_matrix))) - 1,"|", " Prob:",prob),key = NULL,
     breaks = NULL)
}
print(paste("Number of Clusters:", length(unique(c(percolation_matrix))) - 1))
print(paste("Average Density: ", sum(initial_matrix)/square_matrix_size**2 ))
return(list(length(unique(c(percolation_matrix))) - 1, percolates,(sum(initial_matrix)/square_matrix_size**2)))
}







