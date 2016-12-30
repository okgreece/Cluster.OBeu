#' @title 
#' Read and Calculate the Basic Information for Cluster Analysis Tasks from Open Spending API
#'  
#' @description
#' Extract and analyze the input data provided from Open Spending API, using the \code{\link{cl.analysis}} function.
#' 
#' @usage open_spending.cl(json_data, dimensions=NULL, amounts=NULL, measured.dim=NULL,
#'                         cl.feature=NULL, cl.measured.dim="variable", cl.aggregate="sum",
#'                         cl.method=NULL, cl.num=NULL, cl.dist="euclidean")
#' 
#' @param json_data The json string, URL or file from Open Spending API
#' @param dimensions The dimensions of the input data
#' @param amounts The measures of the input data
#' @param measured.dim The dimensions to which correspond amount/numeric variables
#' @param cl.feature ...
#' @param cl.measured.dim ...
#' @param cl.aggregate ...
#' @param cl.method ...
#' @param cl.num ...
#' @param cl.dist ...
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

open_spending.cl <- function(json_data, dimensions=NULL, amounts=NULL, measured.dim=NULL,
                             cl.feature=NULL, cl.measured.dim="variable", cl.aggregate="sum",
                             cl.method=NULL, cl.num=NULL, cl.dist="euclidean"){  
  
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
    
    formula <- paste(dimensions,measured.dim,sep="~") 
    
    dt2 <- reshape::cast(melt,formula,sum,
                         subset=melt$variable==amounts) 
  }
  
  dt2 <- stats::na.omit(dt2) 
  
  cl.result <- cl.analysis(cl.data= dt2, cl_feature=cl.feature, measured.dim=cl.measured.dim, cl.aggregate=cl.aggregate,
                           cl.meth=cl.method, clust.numb=cl.num, dist=cl.dist)
 
  cl.results <- jsonlite::toJSON(cl.result)
  
  return(cl.results)
} 

