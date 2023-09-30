## 10 times, for 10,20,40,80, with seq(0.05,1,0.05) prob.

results <- as.data.frame(matrix(NA_real_, nrow = 4000, ncol = 5))
colnames(results) <- c("N_Clusters","Percolation","Density","Probability","MSize")
results$Probability <- seq(0.025,1,0.025)
results$MSize[1:800] <- 10
results$MSize[801:1600] <- 20
results$MSize[1601:2400] <- 30
results$MSize[2401:3200] <- 40
results$MSize[3201:4000] <- 80
for(i in 1:nrow(results)){
  temp <- hoshen_kopelman(results$MSize[i], prob = results$Probability[i], 
                          plot = FALSE, seed = NULL)
  results[i,1] <- temp[[1]]
  results[i,2] <- temp[[2]]
  results[i,3] <- temp[[3]]
}

results$Percolation <- as.factor(results$Percolation)
results$MSize <- as.factor(results$MSize)




percolating <- results%>%group_by(MSize)%>%
  filter(Percolation == 1)






results%>%group_by(MSize)%>%
  ggplot(mapping = aes(x = Probability, y = Density)) + 
  geom_point(alpha = 0.3,size = 1, aes(color = Percolation))+ geom_vline(xintercept = 0.592746, linetype = 2) + 
  scale_color_discrete(type = "viridis") + facet_wrap("~MSize") + theme_minimal() + geom_smooth(se = TRUE)






