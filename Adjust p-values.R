library(dplyr)

# Read in cleaned data
happyData <- read.csv("./happy_avg.csv", header = TRUE, sep = ",")
View(happyData)

# Regions
regionKey <- c(
  "Southern Asia","Central and Eastern Europe",
  "Middle East and Northern Africa", "Sub-Saharan Africa",
  "Latin America and Caribbean", "Australia and New Zealand",
  "Western Europe", "Southeastern Asia",
  "North America", "Eastern Asia"
)

# Variables to analyze
vars <- c("meanGDP", "meanFreedom", "meanLifeExpectancy")

# Function to compute t-test p-value
GetPValue <- function(region_i, region_j, variable) {
  x <- happyData %>% filter(Region == regionKey[region_i]) %>% pull(variable)
  y <- happyData %>% filter(Region == regionKey[region_j]) %>% pull(variable)
  return(t.test(x, y)$p.value)
}

# Create p-value matrices for each variable
pMatrices <- list()

for (var in vars) {
  
  pMat <- matrix(NA, nrow = 10, ncol = 10)
  
  for (i in 1:9) {
    for (j in (i+1):10) {
      pMat[i, j] <- GetPValue(i, j, var)
    }
  }
  
  rownames(pMat) <- regionKey
  colnames(pMat) <- regionKey
  
  pMatrices[[var]] <- pMat
}

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



