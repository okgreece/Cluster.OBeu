#' @title 
#' Time series analysis results for OBEU Time series
#'  
#' @description
#' Univariate time series analysis for short and long time series different models implemented.
#' 
#' @usage ts.obeu(tsdata, prediction_steps)
#' 
#' @param tsdata The input univariate time series data
#' @param prediction_steps The number of prediction steps
#' 
#' @details 
#' Works perfect but still needs more.
#' This function automatically selects the appropriate arima model that fits the input data.
#' The model selection depends on the results of some diagnostic tests (acf,pacf,pp adf and kpss).
#' 
#' @return A json string with the resulted parameters.
#' 
#' @author Kleanthis Koupidis
#' 
#' @references add
#' 
#' @seealso add
#' 
#' @examples
#' #Not YET an OBeu Example
#' ts.obeu(Athens_draft_ts,1)
#' ts.obeu(Athens_revised_ts,2)
#' ts.obeu(Athens_reserved_ts,3)
#' ts.obeu(Athens_approved_ts,4)
#' ts.obeu(Athens_executed_ts,5)
#' 
#' @rdname ts.obeu
#' 
#' @import forecast
#' @import jsonlite
#' @import tseries
#' 
#' @export
############################################################################

ts.obeu<-function(tsdata,prediction_steps){

  Sys.setlocale(category = "LC_ALL", locale = "Greek")
  
  # Stop if no time series data provided

  if( length(tsdata)<3 ) {
    stop("Invalid time series object.")}

  # Stop if no time series data provided

  if( is.nan(prediction_steps)==T | is.na(prediction_steps)==T |
      is.character(prediction_steps)==T | is.numeric(as.numeric(as.character(prediction_steps)))==F){
    stop("Please give an integer input as 'prediction_steps', e.g. prediction_steps= 3.")}

  ## FUTURE STEP: IF NOT TRY TO CREATE and CALL "HERE" A FUNCTION THAT WILL CONVERT IT  ##
  ## FUTURE STEP: Include other "ts" objects (is.ts(tsdata)==F oris.zoo(tsdata)==F) ##

  ##############################
  ## Stationarity Testing
  ##############################

  #ACF
  acF<-stats::acf(tsdata,plot=F)
  acftest<-ifelse(acF$acf[2:length(acF$lag)]<1.96/sqrt(length(tsdata)) &&
                    acF$acf[2:length(acF$lag)]>-1.96/sqrt(length(tsdata)),
                  "Stationary","Non-Stationary")
  #PACF
  pacF<-stats::pacf(tsdata,plot=F)
  pacftest<-ifelse(pacF$acf[2:length(pacF$lag)]<1.96/sqrt(length(tsdata)) &&
                     pacF$acf[2:length(pacF$lag)]>-1.96/sqrt(length(tsdata)),
                   "Stationary","Non-Stationary")

  acf_pacf<-c(acftest,pacftest)

  ###############################################################################################
  ## If TS is <20
  ###############################################################################################

  if ( length(tsdata)<=10) {

    #Selection of the appropriate model
    aiccc<-list()
    modelss<-list()
    for(i in 1:(length(tsdata)-3)){
      modelss[[i]]<-forecast::Arima(tsdata,order=c(i,1,1))
      aiccc[[i]]<-c(aic=modelss[[i]]$aic,order=c(i,1,1))
    }
    mat<-data.frame(matrix(unlist(aiccc),ncol=4,byrow = T))
    colnames(mat)=c("aic","ar","diff","ma")
    minmat<-mat[mat$aic==min(mat$aic),]
    x<-c(minmat$ar,minmat$diff,minmat$ma)

    #Fit the appropriate model
    ts_model<-forecast::Arima(tsdata,order=x)

    ## Forecasting Time Series
    forecasts<-forecast::forecast(object=ts_model,h = prediction_steps)
  }
    else if ( length(tsdata)>10 && length(tsdata)<=20) {
      
      #Selection of the appropriate model
      aiccc<-list()
      modelss<-list()
      for(i in 1:5){
        modelss[[i]]<-forecast::Arima(tsdata,order=c(i,1,1))
        aiccc[[i]]<-c(aic=modelss[[i]]$aic,order=c(i,1,1))
      }
      mat<-data.frame(matrix(unlist(aiccc),ncol=4,byrow = T))
      colnames(mat)=c("aic","ar","diff","ma")
      minmat<-mat[mat$aic==min(mat$aic),]
      x<-c(minmat$ar,minmat$diff,minmat$ma)
      
      #Fit the appropriate model
      ts_model<-forecast::Arima(tsdata,order=x)
      
      ## Forecasting Time Series
      forecasts<-forecast::forecast(object=ts_model,h = prediction_steps)

  }else if(length(tsdata)>20 ) {
    ###############################################################################################
    ## If TS is >20
    ###############################################################################################

    ##############################
    ## Further Stationarity Testing
    ##############################

    # Phillips-Perron test for the null hypothesis that 'tsdata'
    # has a unit root against a stationary alternative.
    # the truncation lag parameter is set to trunc(4*(n/100)^0.25), where n=length(tsdata)

    pptest<-tseries::pp.test(tsdata,alternative = "stationary")

    # Augmented Dickey–Fuller (ADF) test
    # for the null that 'tsdata' has a unit root
    # the truncation lag parameter is set to trunc((n-1)^(1/3))), where n=length(tsdata)

    adftest<-tseries::adf.test(tsdata,alternative = "stationary")

    # Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
    # the null hypothesis that 'tsdata' is level or trend stationary.
    # the truncation lag parameter is set to trunc(3*sqrt(n)/13), where n=length(tsdata)

    kpsstest<-tseries::kpss.test(tsdata)

    ## Summary of Tests Results

    test_hypo<-data.frame("p_value"=c(pptest$p.value,adftest$p.value,kpsstest$p.value))

    rownames(test_hypo)<-c("Phillips-Perron test","Augmented Dickey–Fuller test",
                           "Kwiatkowski-Phillips-Schmidt-Shin test")

    test_hypo$result<-ifelse(test_hypo$p_value>0.05,"Non-Stationary","Stationary")

    #Fix the Kpss Result
    test_hypo$result[3]<-ifelse(kpsstest$p.value<0.05,"Non-Stationary","Stationary")

    # Most test show that the tsdata are (see check_stat result):

    occurences<-table(unlist(test_hypo$result))

    check_stat<-ifelse(diff(occurences)<0 && acf_pacf=="Stationary","Stationary","Non-Stationary")

    ################ Stationary #################

    if(check_stat=="Stationary") {

      ## ARIMA Model Selection    ##

      ts_model<-forecast::auto.arima(tsdata,trace=F)
      ## Forecasting Time Series
      forecasts<-forecast::forecast(ts_model,h=prediction_steps)


      ################ ΝΟΝ Stationary #################

    }else if(check_stat=="Non-Stationary") {

      ## Transformations
      #FUTURE TASK: Another transformation techniques will be checked and selected

      tsr<-log(tsdata+0.000000001)

      ## ARIMA Model Selection

      ts_model<-forecast::auto.arima(tsr,trace=F)
      ## Forecasting Time Series
      forecasts<-forecast::forecast(ts_model,h=prediction_steps)

    }
  }



  ################  TO JSON  ################

  param<-list(data_year=stats::time(tsdata),
              data=tsdata,
              predict_time=stats::time(forecasts$mean),
              predict_values=forecasts$mean,
              up80=forecasts$upper[,"80%"],
              low80=forecasts$lower[,"80%"],
              up95=forecasts$upper[,"95%"],
              low95=forecasts$lower[,"95%"] )
  ################  RESULT  ################

  parameters<-jsonlite::toJSON(param)

  return(parameters)
}
