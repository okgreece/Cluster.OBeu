#' @title
#' Cluster OBEU
#' 
#' @description 
#' ...
#' 
#' @usage cl.analysis(cluster.data,cluster.method=NULL,cluster.number=NULL,distance="euclidean")
#' @param cluster.data The input data
#' @param cluster.method The clustering algorithm
#' @param cluster.number The number of clusters
#' @param distance The distance metric
#' 
#' @details ...
#'  
#' @return ...
#' 
#' @author Kleanthis Koupidis
#' 
#' @seealso ...
#' 
#' @examples
#' #OBeu Example
#' cl.analysis(Budget_Thessaloniki_2015_Expenditure,"pam",3)
#' 
#' @rdname cl.analysis
#' @export
########################################################################################################
cl.analysis<-function(cluster.data, cluster.method=NULL, cluster.number=NULL, distance="euclidean"){
  
  if(ncol(cluster.data)<2)
  {
    stop("The dimension (number of columns) of dataset must be at least 2.")
  }
  
  ## If method and number of clusters is not provided
  
  if(is.null(cluster.method) & is.null(cluster.number)){
    
    method_clvalid<-clValid::clValid(as.matrix(cluster.data),2:5,
                            clMethods=c("hierarchical","diana","agnes","kmeans", "pam", "clara","fanny", "som", "model", "sota"),
                            validation=c("internal","stability"),
                            metric = "euclidean")
    
    cluster.number=proposed.meth.nb.clust(method_clvalid)$nb.clust
    cluster.method=proposed.meth.nb.clust(method_clvalid)$method.cluster
  }
  
  ## If method is not provided
  
  if(is.null(cluster.method)){
    
    method_clvalid<-clValid::clValid(as.matrix(cluster.data),cluster.number,
                            clMethods=c("hierarchical","diana","agnes","kmeans", "pam", "clara","fanny", "som", "model", "sota"),
                            validation=c("internal","stability"),
                            metric = "euclidean")	
    cluster.method<- proposed.methclust(method_clvalid)
  }
  
  ## If number of clusters is not provided
  
  if(is.null(cluster.number)){
    
    method_clvalid<-clValid::clValid(as.matrix(cluster.data),2:10,
                            clMethods=cluster.method ,
                            validation=c("internal","stability"),
                            metric = "euclidean")	
    
    cluster.number<-proposed.nbclust(method_clvalid)
    }  
  
  
################################################################################
  ## Hierarchical methods
################################################################################
  if(cluster.method %in% c("hierarchical","diana","agnes") ){
    # hierarchical
    if(cluster.method == "hierarchical"){
      
      hierarchical <- stats::hclust(dist(cluster.data), method = "ward.D2")
      
    }   
    # Diana (DIvisive ANAlysis Clustering)
    
    else if(cluster.method == "diana") {
      
      hierarchical <- cluster::diana(cluster.data)
    }
    # Agnes (Agglomerative Nesting- Hierarchical Clustering)
    
    else if(cluster.method == "agnes") {
      
      hierarchical <- cluster::agnes(cluster.data, method = "ward")
    }  
    
    ## Create Clusters
    create.clust <- stats::cutree(hierarchical, k = cluster.number)
    
    ## Add properties to the hierarchical model
    hierarchical$cluster = create.clust
    hierarchical$nb.clust <- cluster.number
    hierarchical$size <- as.vector(table(create.clust))
    
    ## Define model class
    class(cluster.method) <- c(class(cluster.method), "hcut.clust")
    
    # Model Parameters
    
    modelparam<-list( data=cluster.data,
                      clusters=hierarchical$cluster,
                      hierarchical$nb.clust <- cluster.number,
                      size=hierarchical$size,
                      cluster_merging=hierarchical$merge,
                      clustering_height=hierarchical$height,
                      order=hierarchical$order,
                      clusters=hierarchical$labels
    )
  }
  
################################################################################
  
  ## K-Means
  
  else if(cluster.method=="kmeans"){
    
    kmeans=kmeans(cluster.data,cluster.number)
    
    #comparative parameters
    comp.parameters=list(
                      total.sumOfsquares=kmeans$totss,
                      within.sumofsquares=kmeans$withinss,
                      total.within.sumofsquares=kmeans$tot.withinss,
                      between.sumofsquares=kmeans$betweenss,
                      cluster.size=kmeans$size)
    
    #model parameters
    modelparam<-list( data=cluster.data,
                      clusters=kmeans$cluster,
                      cluster.centers=kmeans$centers,
                      compare=comp.parameters)
    
################################################################################
    
    ## Pam (Partitioning Around Medoids)
    
  }else if(cluster.method=="pam"){
    
    pam=cluster::pam(cluster.data,cluster.number, metric = "euclidean")
    
    #comparative parameters
    comp.parameters<- list(cluster.size=pam$clusinfo[,"size"],
                      cluster.max_diss=pam$clusinfo[,"max_diss"],
                      cluster.av_diss=pam$clusinfo[,"av_diss"],
                      cluster.diameter=pam$clusinfo[,"diameter"],
                      cluster.separation=pam$clusinfo[,"separation"],
                      silhouette.info=pam$silinfo)
    
   #model parameters
    modelparam<-list( data=cluster.data,
                      medoids=pam$medoids,
                      medoids.id=pam$id.med,
                      clusters=pam$clustering,
                      compare=comp.parameters)
    
################################################################################
    
    ## Clara (Clustering Large Applications)
    
  }else if(cluster.method=="clara"){
    
    clara=cluster::clara(cluster.data, cluster.number, metric = "euclidean", samples=100)
    
    #comparative parameters
    comp.parameters<- list(
                      cluster.size=clara$clusinfo[,"size"],
                      cluster.max_diss=clara$clusinfo[,"max_diss"],
                      cluster.av_diss=clara$clusinfo[,"av_diss"],
                      cluster.diameter=clara$clusinfo[,"isolation"],
                      silhouette.info=clara$silinfo)
      
    
    #model parameters
    modelparam<-list( data=cluster.data,
                      medoids=clara$medoids,
                      medoids.id=clara$i.med,
                      clusters=clara$clustering,
                      compare=comp.parameters)
    
################################################################################
    
    ## Fanny (Fuzzy Analysis Clustering)
    
  }else if(cluster.method=="fanny"){
    
    
    fanny=cluster::fanny(cluster.data, cluster.number, metric = "euclidean")
    
    #comparative parameters
    comp.parameters<- list( 
                      membership=fanny$membership,
                      coeff=fanny$coeff,
                      memb.exp=fanny$memb.exp,
                      fanny$k.crisp,
                      fanny$objective,
                      fanny$convergence,
                      fanny$silinfo)
      
    #model parameters
    modelparam<-list( 
                      data=cluster.data,
                      clusters=fanny$clustering,
                      compare=comp.parameters)
    
################################################################################
    ## SOM (Self-Organizing Maps)
    
  }else if(cluster.method=="som"){
    
    #model parameters
    
    modelparam<-list( )  
    
################################################################################
    
    ## Model Based Clustering
    
  }else if(cluster.method=="model"){
    mclust=mclust::Mclust(cluster.data, cluster.number)
    
    #comparative parameters
    comp.parameters<- list(  
                      model.name= mclust$modelName,
                      observations= mclust$n,
                      data.dimension= mclust$d,
                      cluster.number= mclust$G,
                      all.Bics=data.frame(matrix(mclust$BIC,dimnames=list(c(colnames(mclust$BIC)),"bic"))),
                      optimal.bic= mclust$bic,
                      optimal.loglik= mclust$loglik,
                      numb.estimated.parameters=mclust$df,
                      hypervolume.parameter= mclust$hypvol,
                      mixing.proportion=mclust$parameters$pro,
                      mean.component=mclust$parameters$mean,
                      variance.components=mclust$parameters$variance,
                      class.probs=mclust$z,
                      uncertainty= mclust$uncertainty)
    #model parameters
    modelparam<-list( 
                      data= cluster.data,
                      classification=mclust$classification,
                      compare=comp.parameters)
  }
  
################################################################################
  ## JSON Output
################################################################################
  
  parameters<- jsonlite::toJSON(modelparam)
  return(parameters)
}

