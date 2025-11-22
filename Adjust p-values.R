library(dplyr)

# Read in cleaned data
happyData <- read.csv("./happy_avg.csv", header = TRUE, sep = ",")
View(happyData)

# Vector that acts as a key assigning regions to a number
regionKey <- c(
  "Southern Asia","Central and Eastern Europe",
  "Middle East and Northern Africa", "Sub-Saharan Africa",
  "Latin America and Caribbean", "Australia and New Zealand",
  "Western Europe", "Southeastern Asia",
  "North America", "Eastern Asia"
)

# Vector of happyFactor to analyze in happyData
vars <- c("meanGDP", "meanFreedom", "meanLifeExpectancy")

# Function to compute t-test p-value
GetPValue <- function(i, j, happyFactor) {
  x <- happyData %>% filter(Region == regionKey[i]) %>% pull(happyFactor)
  y <- happyData %>% filter(Region == regionKey[j]) %>% pull(happyFactor)
  return(t.test(x, y)$p.value)
}

# Create p-value matrices for each variable
pMatrices <- list()

for (var in vars) { #loops factors
  pMat <- matrix(NA, nrow = 10, ncol = 10)#creates empty matrix where p-values will be stored
  for (i in 1:9) {   #loops rows
    for (j in (i+1):10) {  #loops columns
      pMat[i, j] <- GetPValue(i, j, var) #passes the two regions to conduct the t.test on for one specific happy factor
    }
  }
  
  #giving the columns and rows headers with the country names instead of their numbers using the region keys
  rownames(pMat) <- regionKey
  colnames(pMat) <- regionKey
  
  #storing the completed p-value matrix into the larger list of matrices for every variable
  pMatrices[[var]] <- pMat
}

View(pMatrices[["meanGDP"]]) #checking the matrix for one factor

#### ============== Adjusting p-values ============== ####

adjusted_pMatrices <- lapply(pMatrices, function(mat) {
  
  # get upper-triangle p-values
  pvals <- mat[upper.tri(mat)]
  
  # FDR correction
  adj <- p.adjust(pvals, method = "fdr")
  
  # fill into matrix
  mat_adj <- mat
  mat_adj[upper.tri(mat_adj)] <- adj
  
  return(mat_adj)
})

# check results
names(adjusted_pMatrices)

# view one variable
View(adjusted_pMatrices[["meanGDP"]])
View(adjusted_pMatrices[["meanLifeExpectancy"]])
View(adjusted_pMatrices[["meanFreedom"]])


########======================================================================



