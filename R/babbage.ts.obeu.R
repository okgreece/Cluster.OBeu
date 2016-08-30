#' @title 
#' Reading and analyze babbage time series data
#'  
#' @description
#' EXtract and analyze the time series data from babbage api, using the ts.obeu function.
#' 
#' @usage babbage.ts.obeu(json_data,time,amount,prediction_steps)
#' 
#' @param json_data The json string, URL or file from babbage api.
#' @param time Specify the time label of the json time series data.
#' @param amount Specify the amount label of the json time series data.
#' @param prediction_steps The number of prediction steps.
#' 
#' @details 
#' 
#' This function extracts the time series data from a json file resulted from the babbage api
#' and then analyze it using the ts.obeu function.
#' 
#' @return A json string with the resulted parameters of the ts.obeu.
#' 
#' @author Kleanthis Koupidis
#' 
#' @references add
#' 
#' @seealso add
#' 
#' @examples
#' #Not YET an OBeu Example
#' 
#' 
#' @rdname babbage.ts.obeu
#' 
#' @import jsonlite
#' @import lubridate
#' 
#' @export
############################################################################
babbage.ts.obeu<-function(json_data,time,amount,prediction_steps){
  
  data <- jsonlite::fromJSON(json_data)
  data<-data$cells[-3]
  names(data)=c("time","amount")
  data$time<-as.integer(data$time)
  
  # Check prediction_step
  if( is.nan(prediction_steps)==T | is.na(prediction_steps)==T |
      is.character(prediction_steps)==T | is.numeric(as.numeric(as.character(prediction_steps)))==F){
    stop("Please give an integer input as 'prediction_steps', e.g. prediction_steps= 3.")}
  
  # Check time
  if( any(is.nan(data$time)==T) | any(is.na(data$time)==T) |
      is.numeric(as.numeric(as.character(data$time)))==F | 
      any(data$time>lubridate::year(now()))==T | any(data$time<1990)==T) {
    stop("Please give a valid year input as 'time', time should be greater than 1990 and
         could not be greater than the current year")}
  
  # Check amount
  if( any(is.nan(data$amount)==T) | any(is.na(data$amount)==T) |
      is.character(data$amount)==T | is.numeric(as.numeric(as.character(data$amount)))==F){
    stop("Please give a numeric input as 'amount'.")}
 
  tsdata<-ts(data$amount,start=min(data$time),end=max(data$time))

  ts.result<-ts.obeu(tsdata,prediction_steps)
  
  return(ts.result)  
}