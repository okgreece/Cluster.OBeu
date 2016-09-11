#' @title
#' Extract the proposed clustering method and the number of clusters from clvalid method
#' 
#' @description 
#' ...
#' 
#' @usage proposed.meth.nb.clust(clv)
#' @param clv A clValid object
#' 
#' @details ...
#'  
#' @return ...
#' 
#' @author Kleanthis Koupidis
#' 
#' @seealso proposed.methclust, proposed.nbclust
#' 
#' @examples
#' #OBeu Example
#' ...
#' 
#' @rdname proposed.meth.nb.clust
#' @export
############################################################################
proposed.meth.nb.clust<-function(clv){
  
  if(!inherits(clv,"clValid")) stop("clValid object should be provided")
  
  df<-t(as.data.frame(clv@measures))
  df<-as.data.frame(df)
  apn<-c(rownames(df[which(df$APN==min(df$APN)),]),min(df$APN))
  ad<-c(rownames(df[which(df$AD==min(df$AD)),]),min(df$AD))
  adm<-c(rownames(df[which(df$ADM==min(df$ADM)),]),min(df$ADM))
  fom<-c(rownames(df[which(df$FOM==min(df$FOM)),])[1],min(df$FOM))
  
  connect<-c(rownames(df[which(df$Connectivity==max(df$Connectivity)),]),max(df$Connectivity))
  dun<-c(rownames(df[which(df$Dunn==max(df$Dunn)),])[1],max(df$Dunn))
  silh<-c(rownames(df[which(df$Silhouette==max(df$Silhouette)),])[1],max(df$Silhouette))
  
  tab<-rbind(apn,ad,adm,fom,connect,dun,silh)
  tab<-table(tab[,1])
  cluster.details=which.max(tab)
  cluster.details<-names(cluster.details)
  cluster.details<-strsplit(cluster.method,".",fixed=T)
  cluster.details<-list(nb.clust=as.integer(cluster.details[[1]][1]),method.cluster=cluster.details[[1]][2])
  return(cluster.details)
}