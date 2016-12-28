#' @title 
#' Read and Calculate the Basic Information for Basic Descriptive Tasks from Open Spending API
#'  
#' @description
#' Extract and analyze the input data provided from Open Spending API, using the ds.analysis function.
#' 
#' @usage open_spending.ds(json_data,
#' dimensions=NULL, amounts=NULL, measured.dimensions=NULL, 
#' coef.outl=1.5, box.outliers=T, box.wdth=0.15,
#' cor.method= "pearson", freq.select=NULL)
#' 
#' @param json_data The json string, URL or file from Open Spending API
#' @param dimensions The dimensions of the input data
#' @param amounts The measures of the input data
#' @param measured.dimensions The dimensions to which correspond amount/numeric variables
#' @param coef.outl Determines the length of the "whiskers" plot.
#' If it is equal to zero no outliers will be returned.
#' @param box.outliers If TRUE the outliers will be computed at the selected "coef.outl" level 
#' (default is 1.5 times the Interquartile Range).
#' @param box.wdth The width level is determined 0.15 times the square root of the size of the input data.
#' @param cor.method The correlation coefficient method to compute: "pearson" (default),
#' "kendall" or "spearman".
#' @param freq.select One or more nominal variables to calculate their corresponding frequencies.
#' 
#' @details 
#' This function is used to read data in json format from Open Spending API, in order to implement 
#' some basic descriptive tasks through \code{\link{ds.analysis}} function.
#' 
#' @return A json string with the resulted parameters of the \code{\link{ds.analysis}} function.
#'
#' @author Kleanthis Koupidis
#' 
#' @seealso \code{\link{ds.analysis}}
#' 
#' @rdname open_spending.ds
#' 
#' @import jsonlite
#' @import reshape
#'
#' @export
############################################################################################################
 
open_spending.ds <- function(json_data,  
                             dimensions=NULL, amounts=NULL, measured.dimensions=NULL, 
                             coef.outl=1.5, box.outliers=T, box.wdth=0.15,
                             cor.method= "pearson", freq.select=NULL){  
  
  if (RCurl::url.exists(json_data)){
  json_data<-RCurl::getURL( json_data, ssl.verifyhost=FALSE )
  } 
  
  #s <- readLines(json_data);
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
  
  ds.result <- ds.analysis(dt2, c.out=coef.outl,outliers=box.outliers,box.width=box.wdth, 
                           corr.method= cor.method, fr.select=freq.select) 
  
  ds.results <- jsonlite::toJSON(ds.result)
  
  return(ds.results)
} 

