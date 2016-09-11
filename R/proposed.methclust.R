#' @title
#' Extract the proposed clustering method from clvalid method
#' 
#' @description 
#' ...
#' 
#' @usage proposed.methclust(clv)
#' @param clv A clValid object
#' 
#' @details ...
#'  
#' @return ...
#' 
#' @author Kleanthis Koupidis
#' 
#' @seealso proposed.meth.nb.clust, proposed.nbclust
#' 
#' @examples
#' #OBeu Example
#' ...
#' 
#' @rdname proposed.methclust
#' @export
############################################################################
proposed.methclust<-function(clv){
  
  if(!inherits(clv,"clValid")) stop("clValid object should be provided")
  
  df<-t(as.data.frame(clv@measures))
  row.names(df)<-dimnames(clv@measures)[[3]]
  df<-as.data.frame(df)
  apn<-c(rownames(df[which(df$APN==min(df$APN)),]),min(df$APN))
  ad<-c(rownames(df[which(df$AD==min(df$AD)),]),min(df$AD))
  adm<-c(rownames(df[which(df$ADM==min(df$ADM)),]),min(df$ADM))
  fom<-c(rownames(df[which(df$FOM==min(df$FOM)),])[1],min(df$FOM))
  
  connect<-c(rownames(df[which(df$Connectivity==max(df$Connectivity)),]),max(df$Connectivity))
  dun<-c(rownames(df[which(df$Dunn==max(df$Dunn)),]),max(df$Dunn))
  silh<-c(rownames(df[which(df$Silhouette==max(df$Silhouette)),])[1],max(df$Silhouette))
  
  tab<-rbind(apn,ad,adm,fom,connect,dun,silh)
  tab<-table(tab[,1])
  cluster.method=which.max(tab)
  cluster.method<-names(cluster.method)
  names(cluster.method)<-"Proposed clustering method is:"
  return(cluster.method)
}