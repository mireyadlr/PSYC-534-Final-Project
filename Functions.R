GetVectorForRegion <- function(m){
  currentVector <- subset(happyData, happyData$Region == regionKey[m], select = meanGDP)
  return(currentVector$meanGDP)
}

GetPValue <- function(i, j){
  t.test(GetVectorForRegion(i),
       GetVectorForRegion(j),
       paired = FALSE)$p.value
}