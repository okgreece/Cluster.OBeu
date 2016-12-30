#' @title
#' Extract the proposed number of clusters from clvalid method
#' 
#' @description 
#' ...
#' 
#' @usage proposed.nbclust(clv)
#' @param clv A clValid object
#' 
#' @details ...
#'  
#' @return ...
#' 
#' @author Kleanthis Koupidis
#' 
#' @seealso proposed.meth.nb.clust, proposed.methclust
#' 
#' @rdname proposed.nbclust
#' @export
############################################################################
proposed.nbclust<-function(clv){
  
  if(!inherits(clv,"clValid")) stop("clValid object should be provided")
  
  df<-t(as.data.frame(clv@measures))
  row.names(df)<-dimnames(clv@measures)[[2]]
  df<-as.data.frame(df)
  apn<-c(rownames(df[which(df$APN==min(df$APN)),]),min(df$APN))
  ad<-c(rownames(df[which(df$AD==min(df$AD)),]),min(df$AD))
  adm<-c(rownames(df[which(df$ADM==min(df$ADM)),]),min(df$ADM))
  fom<-c(rownames(df[which(df$FOM==min(df$FOM)),]),min(df$FOM))
  
  connect<-c(rownames(df[which(df$Connectivity==max(df$Connectivity)),]),max(df$Connectivity))
  dun<-c(rownames(df[which(df$Dunn==max(df$Dunn)),]),max(df$Dunn))
  silh<-c(rownames(df[which(df$Silhouette==max(df$Silhouette)),]),max(df$Silhouette))
  
  tab<-rbind(apn,ad,adm,fom,connect,dun,silh)
  cluster.number=which.max(tab[,1])
  names(cluster.number)<-"Proposed number of clusters:"
  return(cluster.number)
}
