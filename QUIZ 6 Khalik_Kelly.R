# Names: Sofiia Khalik, David Kelly

printCheck <- function(checked){
  if(checked == FALSE){
    stop('Object is not a number. Please provide number.')
  }
  return(checked)
}

checked <- FALSE

printCheck(checked)
