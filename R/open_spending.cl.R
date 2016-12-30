#' @title 
#' Read and Calculate the Basic Information for Cluster Analysis Tasks from Open Spending API
#'  
#' @description
#' Extract and analyze the input data provided from Open Spending API, using the \code{\link{cl.analysis}} function.
#' 
#' @usage open_spending.ds(json_data, dimensions=NULL, amounts=NULL, measured.dimensions=NULL,
#'                         cl_feature=NULL, cl.measured.dimension="variable", cl.aggregate="sum",
#'                         cl.method=NULL, cl.number=NULL, cl.distance="euclidean")
#' 
#' @param json_data The json string, URL or file from Open Spending API
#' @param dimensions The dimensions of the input data
#' @param amounts The measures of the input data
#' @param measured.dimensions The dimensions to which correspond amount/numeric variables
#' @param cl.feature ...
#' @param cl.measured.dimension ...
#' @param cl.aggregate ...
#' @param cl.method ...
#' @param cl.number ...
#' @param cl.distance ...
#' 
#' @details 
#' This function is used to read data in json format from Open Spending API, in order to implement 
#' some cluster analysis tasks through \code{\link{cl.analysis}} function.
#' 
#' @return A json string with the resulted parameters of the \code{\link{cl.analysis}} function.
#'
#' @author Kleanthis Koupidis
#' 
#' @seealso \code{\link{cl.analysis}}
#' 
#' @rdname open_spending.cl
#' 
#' @import jsonlite
#' @import reshape
#'
#' @export
############################################################################################################

open_spending.cl <- function(json_data, dimensions=NULL, amounts=NULL, measured.dimensions=NULL,
                             cl.feature=NULL, cl.measured.dimension="variable", cl.aggregate="sum",
                             cl.method=NULL, cl.number=NULL, cl.distance="euclidean"){  
  
  if (RCurl::url.exists(json_data)){
  json_data<-RCurl::getURL( json_data, ssl.verifyhost=FALSE )
  } 
  
  dt <- jsonlite::fromJSON(json_data)
  
  components <- c("data", "cells")
  
  select.comp <- match.arg(components, names(dt), several.ok = T)
  
  dt <- as.data.frame(dt[select.comp])
  
  amounts = unlist(strsplit(amounts,"\\|"))
  
  dimensions = unlist(strsplit(dimensions,"\\|"))
  
  if (select.comp== "data") {
    
    names(dt) <- gsub("data.","",names(dt)) 
    
    variables <- c(dimensions,amounts)
    
    dt2 <- dt[variables]
    dt2[dimensions] <- sapply(dt2[dimensions],as.character)
  }  else {
    names(dt) <- gsub("cells.","",names(dt))
    
    melt <- reshape::melt.data.frame(dt)
    
    formula <- paste(dimensions,measured.dimensions,sep="~") 
    
    dt2 <- reshape::cast(melt,formula,sum,
                         subset=melt$variable==amounts) 
  }
  
  dt2 <- stats::na.omit(dt2) 
  
  cl.result <- cl.analysis(cluster.data= dt2, cl_feature=cl.feature, measured.dimension=cl.measured.dimension, cl.aggregate=cl.aggregate,
                     cluster.method=cl.method, cluster.number=cl.number, distance=cl.distance)
 
  cl.results <- jsonlite::toJSON(cl.result)
  
  return(cl.results)
} 

