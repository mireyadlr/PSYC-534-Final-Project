##Plots


#making heatmap and adding color palette for mean GDP
heatmap(adjusted_pMatrices[["meanGDP"]], 
  Rowv = NA, Colv= NA,
  cexRow=0.5,
  cexCol=0.5,
  col = colorRampPalette(c("darkgreen", "lightgreen", "white", 
                           "skyblue", "navy"))(200))
#Adding a title
title("Heatmap of meanGDP adjusted P-values")

#Adding a legend on the side
zlim_meanGDP <- range(adjusted_pMatrices[["meanGDP"]], na.rm = TRUE)
cols <- colorRampPalette(c("darkgreen", "lightgreen", "white", 
                           "skyblue", "navy"))(200)
legend(
  "topleft", 
  legend = round(seq(zlim_meanGDP[1], zlim_meanGDP[2], length.out = 10), 2),
  fill = cols[seq(1, length(cols), length.out = 10)],                            
  border = NA,
  bty = "n",
  title = "Value"
)





#for Mean Life expectancy
heatmap(adjusted_pMatrices[["meanLifeExpectancy"]], 
        Rowv = NA, Colv= NA,
        cexRow = 0.5,
        cexCol = 0.5,
        col = colorRampPalette(c("darkgreen", "lightgreen", "white", 
                                 "skyblue", "navy"))(200))
#Adding a title
title("Heatmap of meanLifeExpectancy adjusted P-values")

#Adding a legend on the side
zlim_meanLifeExpectancy <- range(adjusted_pMatrices[["meanLifeExpectancy"]], na.rm = TRUE)

legend(
  "topleft", 
  legend = round(seq(zlim_meanLifeExpectancy[1], zlim_meanLifeExpectancy[2], length.out = 10), 2),
  fill = cols[seq(1, length(cols), length.out = 10)],                            
  border = NA,
  bty = "n",
  title = "Value"
)





#for mean Freedom
heatmap(adjusted_pMatrices[["meanFreedom"]], 
        Rowv = NA, Colv= NA,
        cexRow = 0.5,
        cexCol = 0.5,
        col = colorRampPalette(c("darkgreen", "lightgreen", "white", 
                                 "skyblue", "navy"))(200))
#Adding a title
title("Heatmap of meanFreedom adjusted P-values")

#Adding a legend on the side
zlim_meanFreedom <- range(adjusted_pMatrices[["meanFreedom"]], na.rm = TRUE)

legend(
  "topleft", 
  legend = round(seq(zlim_meanFreedom[1], zlim_meanFreedom[2], length.out = 10), 2),
  fill = cols[seq(1, length(cols), length.out = 10)],                            
  border = NA,
  bty = "n",
  title = "Value"
)





#Making a plot of points and cut off line

#for mean GDP
# Suppose your matrix of adjusted p-values is:
adj_p_meanGDP <- adjusted_pMatrices[["meanGDP"]]

# Flatten to a vector
adj_p_vec_meanGDP <- as.vector(adj_p_meanGDP)

# Remove NAs (if any)
adj_p_vec_meanGDP <- adj_p_vec_meanGDP[!is.na(adj_p_vec_meanGDP)]

#sort the values
sorted_adj_p_meanGDP <- sort(adj_p_vec_meanGDP)

plot(
  1:length(sorted_adj_p_meanGDP),
  sorted_adj_p_meanGDP,
  type = "p",
  pch = 16,
  col = "purple",
  xlab = "Index",
  ylab = "Adjusted P-value",
  main = "Sorted Adjusted P-values for mean GDP"
)

# Add horizontal significance threshold at 0.05
abline(h = 0.05, col = "blue", lty = 2)





#for mean LIFE EXPECTANCY
# Suppose your matrix of adjusted p-values is:
adj_p_meanLifeExpectancy <- adjusted_pMatrices[["meanLifeExpectancy"]]

# Flatten to a vector
adj_p_vec_meanLifeExpectancy <- as.vector(adj_p_meanLifeExpectancy)

# Remove NAs (if any)
adj_p_vec_meanLifeExpectancy <- adj_p_vec_meanLifeExpectancy[!is.na(adj_p_vec_meanLifeExpectancy)]

#sort the values
sorted_adj_p_meanLifeExpectancy <- sort(adj_p_vec_meanLifeExpectancy)

plot(
  1:length(sorted_adj_p_meanLifeExpectancy),
  sorted_adj_p_meanLifeExpectancy,
  type = "p",
  pch = 16,
  col = "purple",
  xlab = "Index",
  ylab = "Adjusted P-value",
  main = "Sorted Adjusted P-values for mean Life Expectancy"
)

# Add horizontal significance threshold at 0.05
abline(h = 0.05, col = "blue", lty = 2)





##for mean FREEDOM
# Suppose your matrix of adjusted p-values is:
adj_p_meanFreedom <- adjusted_pMatrices[["meanFreedom"]]

# Flatten to a vector
adj_p_vec_meanFreedom <- as.vector(adj_p_meanFreedom)

# Remove NAs (if any)
adj_p_vec_meanFreedom <- adj_p_vec_meanFreedom[!is.na(adj_p_vec_meanFreedom)]

#sort the values
sorted_adj_p_meanFreedom <- sort(adj_p_vec_meanFreedom)

plot(
  1:length(sorted_adj_p_meanFreedom),
  sorted_adj_p_meanFreedom,
  type = "p",
  pch = 16,
  col = "purple",
  xlab = "Index",
  ylab = "Adjusted P-value",
  main = "Sorted Adjusted P-values for mean Freedom"
)

# Add horizontal significance threshold at 0.05
abline(h = 0.05, col = "blue", lty = 2)






##Barcharts of the means by region

#for meanGDP
mean_meanGDP <-tapply(happyData$meanGDP, happyData$Region, mean)
barplot(
  mean_meanGDP,
  col = "steelblue",
  ylab = "Mean GDP Score",
  main = "Mean GDP Score by Region",
  las = 2,  # rotate region names 90 degrees
  cex.names = 0.3 #make region names smaller
)





#for meanLifeExpectancy
mean_meanLifeExpectancy <-tapply(happyData$meanLifeExpectancy, happyData$Region, mean)
barplot(
  mean_meanLifeExpectancy,
  col = "steelblue",
  ylab = "Mean Life Expectancy Score",
  main = "Mean Life Expectancy Score by Region",
  las = 2, #rotate region names 90 degrees
  cex.names = 0.3 #make region names smaller
)




#for meanFreedom
mean_meanFreedom <-tapply(happyData$meanFreedom, happyData$Region, mean)
barplot(
  mean_meanFreedom,
  col = "steelblue",
  ylab = "Mean Freedom Score",
  main = "Mean Freedom Score by Region",
  las = 2,  # rotate region names 90 degrees
  cex.names = 0.3 #make region names smaller
)
