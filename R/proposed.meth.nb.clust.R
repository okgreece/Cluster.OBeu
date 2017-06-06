#' @title
#' Extract the proposed clustering method and the number of clusters from clvalid method
#' 
#' @description 
#' ...
#' 
#' @usage proposed.meth.nb.clust(clv)
#' @param clv A clValid object
#' 
#' @details This function returns the proposed method or number of clusters or both according to the majority clustering indices of a clValid process
#'  
#' @return A value that indicates the proposed method and number of clusters.
#' 
#' @author Kleanthis Koupidis
#' 
#' @rdname proposed.meth.nb.clust
#' @export

proposed.meth.nb.clust<-function(clv){
  
  if(!inherits(clv,"clValid")) stop("clValid object should be provided")
  
  df<-t(as.data.frame(clv@measures))
  df=as.data.frame(df)
  tab=data.frame(index=c(rep("APN",length(rownames(df[which(df$APN==min(df$APN)),]))),
                        rep("AD",length(rownames(df[which(df$AD==min(df$AD)),]))),
                        rep("ADM",length(rownames(df[which(df$ADM==min(df$ADM)),]))),
                        rep("FOM",length(rownames(df[which(df$FOM==min(df$FOM)),]))),
                        rep("Connectivity",length(rownames(df[which(df$Connectivity==max(df$Connectivity)),]))),
                        rep("Dunn",length(rownames(df[which(df$Dunn==max(df$Dunn)),]))),
                        rep("Silhouette",length(rownames(df[which(df$Silhouette==max(df$Silhouette)),])))
                        ),
                 model=c(rownames(df[which(df$APN==min(df$APN)),]),
                         rownames(df[which(df$AD==min(df$AD)),]),
                         rownames(df[which(df$ADM==min(df$ADM)),]),
                         rownames(df[which(df$FOM==min(df$FOM)),]),
                         rownames(df[which(df$Connectivity==max(df$Connectivity)),]),
                         rownames(df[which(df$Dunn==max(df$Dunn)),]),
                         rownames(df[which(df$Silhouette==max(df$Silhouette)),])
                         ),
                 value=c(rep(min(df$APN),length(rownames(df[which(df$APN==min(df$APN)),]))),
                         rep(min(df$AD),length(rownames(df[which(df$AD==min(df$AD)),]))),
                         rep(min(df$ADM),length(rownames(df[which(df$ADM==min(df$ADM)),]))),
                         rep(min(df$FOM),length(rownames(df[which(df$FOM==min(df$FOM)),]))),
                         rep(max(df$Connectivity),length(rownames(df[which(df$Connectivity==max(df$Connectivity)),]))),
                         rep(max(df$Dunn),length(rownames(df[which(df$Dunn==max(df$Dunn)),]))),
                         rep(max(df$Silhouette),length(rownames(df[which(df$Silhouette==max(df$Silhouette)),])))
                         )
                 )
  tab$model <-gsub("[0-9]\\.","",tab$model)
  
  cl.algorithm=table(tab[,2])
  
  #tab=a[order(-a)]
  max=which(cl.algorithm==max(cl.algorithm))
  #i=sample(max,1, replace=TRUE)
  
    if (length(max) <= 1) {
      i=max
    } else {
      i= sample(max,1)
    }
 
  
  #if (proposed=="both"){
  cl.nb.meth=data.frame(nb.clust=as.integer(cl.algorithm[i]),method.cluster=names(cl.algorithm[i]))
  #} else if (proposed=="method"){
  #  cl.nb.meth=names(cl.algorithm[i])
  #} else if (proposed=="nb"){
  #  cl.nb.meth=as.integer(cl.algorithm[i])
  #} 
  return(cl.nb.meth)
}
