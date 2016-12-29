#' @title 
#' Select the numeric columns of a given dataset
#'  
#' @description
#' Extract and return a data frame with the columns that include only numeric values
#' 
#' @usage nums(data)
#' 
#' @param data The input data frame
#' 
#' @return This function returns a data frame with the numeric columns of the input dataset.
#'
#' @author Kleanthis Koupidis
#' 
#' @rdname nums
#' 
#' @export
#####################################################################################################

nums=function(data){
  data = as.data.frame(data)
  
  if(ncol(data)>=2){
    num = sapply(data, is.numeric)
    
    data.num = data[num]
    
    data.num = as.data.frame(data.num)
  } else data.num = data
  
  return(data.num)
}
