library(dplyr)

#read in cleaned data
happyData <- read.csv("./happy_avg.csv",
                      header = TRUE,
                      sep = ",")
View(happyData)

##Creating vector of regions so they are each assigned a number (by row number)
regionKey <- c("Southern Asia","Central and Eastern Europe",
               "Middle East and Northern Africa", "Sub-Saharan Africa",
               "Latin America and Caribbean", "Australia and New Zealand",
               "Western Europe", "Southeastern Asia",
               "North America", "Eastern Asia")
regionKey


##Create empty matrix that will hold p-values
pMatrix <- matrix(nrow = 10, ncol = 10)

#Adding p-values into matrix by rows, then columns
for(i in 1:9){
  for(j in (i+1):10){
    #cat("i = ", i, "j = ", j,"\n")
    pMatrix[i,j] <- GetPValue(i,j)
  }
}
View(pMatrix)

colnames(pMatrix) <- c("Southern Asia","Central and Eastern Europe",
                       "Middle East and Northern Africa", "Sub-Saharan Africa",
                       "Latin Acmerica and Caribbean", "Australia and New Zealand",
                       "Western Europe", "Southeastern Asia",
                       "North America", "Eastern Asia")

rownames(pMatrix) <- c("Southern Asia","Central and Eastern Europe",
                       "Middle East and Northern Africa", "Sub-Saharan Africa",
                       "Latin Acmerica and Caribbean", "Australia and New Zealand",
                       "Western Europe", "Southeastern Asia",
                       "North America", "Eastern Asia")