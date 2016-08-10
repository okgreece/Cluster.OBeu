#' @title
#' Aggregate an OBEU dataset
#' 
#' @description 
#' Aggregate an OBEU dataset according to its dimensions.
#' @usage aggregationsobeu(data,what="...", to_what= "variable", fun.aggregate="sum", ...)
#' @param data The input dataset to aggregate
#' @param what A string of dimensions to aggregate (e.g."dim1,dim2" that results in dim1+dim2)
#' @param to_what A string of dimensions to aggregate (e.g."dim1,dim2" that results in dim1+dim2)
#' @param fun.aggregate The aggregation function (e.g.sum,mean,sd,var,max,min,...,summary,decribe).
#' @param ... Other arguments to pass, see \code{\link{cast}} from \pkg{reshape} package
#' 
#' @details Works perfect for every dataset but needs a few slight changes.
#' 
#' @return A json string with the results of the selected aggregation parameters.
#' Aggregate \code{what} \code{to_what}.
#' 
#' @author Kleanthis Koupidis
#' 
#' @references add
#' 
#' @seealso add
#' 
#' @examples
#' #OBeu Example
#' aggregations.obeu(Budget_Thessaloniki_2015_Revenue)
#' aggregations.obeu(Budget_Thessaloniki_2015_Revenue,"Υπηρεσία","variable",sum)
#' aggregations.obeu(Budget_Thessaloniki_2015_Revenue,"Υπηρεσία","variable",mean)
#' @rdname aggregations.obeu
#' 
#' @import jsonlite
#' @import reshape2
#' @import stringr
#' 
#' @export
#' @export
############################################################################

aggregations.obeu<- function(data, what="...", to_what="variable",
                             fun.aggregate="sum", ...)
{
  Sys.setlocale(category = "LC_ALL", locale = "Greek")
  
  # Melt data

  molten_data=reshape2::melt(data)
  names(molten_data)=tolower(names(molten_data))
  .Export(molten_data)
  
  ##Formula
  if(missing(what)==F){

    what=  tolower(as.character(substitute(what)))
    what=as.vector(unlist(stringr::str_split(what, pattern = ",")))

    if (what[1]=="c"){what=what[-1]}

    if (all(what %in% names(molten_data))==F){
      print("Please give an appropriate value to what")
      names(molten_data)
    } else {

      what= what[which(what %in% names(molten_data))]
      what=stringr::str_c(what, collapse = "+")

    }
  }

  if(missing(to_what)==F){

    to_what=  tolower(as.character(substitute(to_what)))
    to_what=as.vector(unlist(stringr::str_split(to_what, pattern = ",")))

    if (to_what[1]=="c"){to_what=to_what[-1]}

    if (all(to_what %in% names(molten_data))==F){
      print("Please give an appropriate value  to_what")
      names(molten_data)
    } else {

      to_what= to_what[which(to_what %in% names(molten_data))]
      to_what=stringr::str_c(to_what, collapse = "+")

    }
  }

  ##
  expression=stringr::str_c(what,"~", to_what, collapse = " ")

  #Multiple aggregation functions
  fun.aggregate=as.vector(as.character(substitute(fun.aggregate)))
  if (length(fun.aggregate)>1)
  { fun.aggregate=fun.aggregate[-1]}
  result=list()
  for (i in 1:length(fun.aggregate)){
    result[[i]] <-reshape::cast(molten_data, expression,  fun.aggregate[i], ... )
    names(result)[i]<-paste(fun.aggregate)[i]

  }

  result<-jsonlite::toJSON(result)

  return(result)

}
