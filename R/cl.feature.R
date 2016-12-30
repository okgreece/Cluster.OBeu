#' @title
#' Clustering feature
#' 
#' @description 
#' Select clustering characteristic of OBEU datasets.
#' 
#' 
#' @usage cl.feature(data, feature=NULL, measured="variable",aggregate="sum")
#' @param data The input data
#' @param feature The clustering feature
#' @param measured The measured dimension of dataset
#' @param aggregate The function to aggregate
#' 
#' @details This function adapts the dataset according to the selected dimension of the dataset 
#' and the aggregation function. 
#'
#' @return This function returns the dataset for cluster analysis adapted to the desired feature. 
#' 
#' @author Kleanthis Koupidis
#' 
#' @seealso \code{\link{cl.analysis}}
#' 
#' @examples
#' cl.feature(Budget_Thessaloniki_2015_Revenue, feature=NULL, measured="variable", aggregate="sum")
#' 
#' @import reshape
#' @import stringr
#' 
#' @rdname cl.feature
#' 
#' @export
########################################################################################################

cl.feature = function(data, feature=NULL, measured="variable", aggregate="sum") {
  
  # Convert to data frame
  
  data=as.data.frame(data)
  
  # If all numeric variables 
  
  if   ( all(sapply(data, is.double) | sapply(data, is.numeric))==T ){
    
    cluster.data = data
    
  }else{
    
  #If feature is not provided
    
  if (is.null(feature)){
  
  sel = which(sapply(data, is.factor) | sapply(data, is.character) )
  
    if ( length(names(sel))>1){
      
      feature = stringr::str_c(names(sel), collapse = "+")
      
      # Melt data
      molten_data=reshape::melt.data.frame(data)
      
      # Expression
      expression = stringr::str_c(feature,"~", measured, collapse = " ")
      
      # Form Dataset
      cluster.data = reshape::cast(molten_data, expression, fun.aggregate = aggregate)    
      
      }else cluster.data=data
  }
  
  }
  return(cluster.data)
  
  }
