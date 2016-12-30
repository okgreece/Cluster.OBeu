#' @title
#' Cluster OBEU
#' 
#' @description 
#' Clustering Analysis for OBEU datasets.
#' 
#' 
#' @usage cl.analysis(cluster.data, cl_feature=NULL, measured.dimension="variable", cl.aggregate="sum",
#'                      cluster.method=NULL, cluster.number=NULL, distance="euclidean")
#'                      
#' @param cluster.data The input data
#' @param cl_feature ...
#' @param measured.dimension ...
#' @param cl.aggregate ...
#' @param cluster.method The clustering method algorithm
#' @param cluster.number The number of clusters
#' @param distance The distance metric
#' 
#' @details ...
#'  
#' @return This function returns the graphical and evaluation paremeters of the selected algorithm. 
#' 
#' @author Kleanthis Koupidis, Jaroslav Kuchar
#' 
#' @seealso \code{\link{cl.feature}}
#' 
#' @examples
#' cl.analysis(Budget_Thessaloniki_2015_Expenditure, cl_feature=NULL, measured.dimension="variable", cl.aggregate="sum",
#'                cluster.method="pam", cluster.number=3, distance="euclidean")
#' 
#' @import cluster
#' @import clValid
#' @import dendextend
#' @import mclust
#' @import utils
#' 
#' @rdname cl.analysis
#' 
#' @export
########################################################################################################
cl.analysis=function(cluster.data, cl_feature=NULL, measured.dimension="variable", cl.aggregate="sum",
                     cluster.method=NULL, cluster.number=NULL, distance="euclidean"){
  
  if( ncol(cluster.data)< 2 ) 
  {
    stop("The dimension (number of columns) of dataset must be at least 2 numeric variables.")
  }
  if ( ncol(nums(cluster.data))< 2 )
  {
    stop("The dimension (number of columns) of dataset must be at least 2 numeric variables.")
  }
  
  # Select clustering feature
  
  clusterr.data = cl.feature(cluster.data, feature=cl_feature, measured=measured.dimension, aggregate=cl.aggregate)
  cluster.data = nums(clusterr.data)
  
  ## If method and number of clusters is not provided
  
  if(is.null(cluster.method) & is.null(cluster.number)){
    
    method_clvalid=clValid::clValid(as.matrix(cluster.data),2:5,
                            clMethods=c("hierarchical","kmeans", "pam", "clara","fanny", "model"),
                            validation=c("internal","stability"),
                            metric = "euclidean",maxitems = nrow(cluster.data))
    
    cluster.number=proposed.meth.nb.clust(method_clvalid)$nb.clust
    cluster.method=proposed.meth.nb.clust(method_clvalid)$method.cluster
  }
  
  ## If method is not provided
  
  if(is.null(cluster.method)){
    
    method_clvalid=clValid::clValid(as.matrix(cluster.data),cluster.number,
                            clMethods=c("hierarchical","kmeans", "pam", "clara","fanny", "model"),
                            validation=c("internal","stability"),
                            metric = "euclidean")	
    cluster.method= proposed.methclust(method_clvalid)
  }
  
  ## If number of clusters is not provided
  
  if(is.null(cluster.number)){
    
    method_clvalid=clValid::clValid(as.matrix(cluster.data),2:10,
                            clMethods=cluster.method ,
                            validation=c("internal","stability"),
                            metric = "euclidean")	
    
    cluster.number=proposed.nbclust(method_clvalid)
    }  
  
  
################################################################################
  ## Hierarchical methods
################################################################################
  if(cluster.method %in% c("hierarchical","diana","agnes") ){
    # hierarchical
    if(cluster.method == "hierarchical"){
      
      tree = stats::hclust(stats::dist(cluster.data), method = "ward.D2")
      
    }   
    # Diana (DIvisive ANAlysis Clustering)
    
    else if(cluster.method == "diana") {
      
      tree = cluster::diana(cluster.data)
    }
    # Agnes (Agglomerative Nesting- Hierarchical Clustering)
    
    else if(cluster.method == "agnes") {
      
      tree = cluster::agnes(cluster.data, method = "ward")
    }  
    #Convert to dendrogram
    dendr=stats::as.dendrogram(tree)
    
    ## Create Clusters
    create.clust = dendextend::cutree(dendr, k = cluster.number)
    
    ## Add properties to the hierarchical model
    #size = as.matrix(table(create.clust), c )
    
    ## Define model class
    #class(cluster.method) = c(class(cluster.method), "hcut.clust")
    
    
    
    # Tree Graphical Parameters
    node.coordinates = dendextend::get_nodes_xy(dendr)
    node.names = dendextend:: partition_leaves(dendr)

    #a=lapply(node.names,as.character)
    #aa=lapply(a,paste,collapse=",")
    #pr=data.frame(cbind(node.coordinates,unlist(aa)))
    
    # Model Parameters
    
    modelparam=list( data=cluster.data,
                      clustered.data=create.clust,
                      #cluster.table=size,
                      x.coordinates = node.coordinates[,1],
                      y.coordinates = node.coordinates[,2],
                      node.names = node.names,
                      compare= list(method= tree$dist.method)
    )
################################################################################
  ## K-Means
  
 } else if(cluster.method=="kmeans"){
    
    kmeans=kmeans(cluster.data,cluster.number)
    
    #comparative parameters
    comp.parameters=list(
                      total.sumOfsquares=kmeans$totss,
                      within.sumofsquares=kmeans$withinss,
                      total.within.sumofsquares=kmeans$tot.withinss,
                      between.sumofsquares=kmeans$betweenss,
                      cluster.size=kmeans$size)
    
    #model parameters
    modelparam=list( data=cluster.data,
                      clusters=kmeans$cluster,
                      cluster.centers=kmeans$centers,
                      compare=comp.parameters)
    
    # PCA
    data.pca = stats::prcomp(cluster.data, scale. = T, center = T)
    # ellipses + convex hulls
    cluster.ellipses = .ellipses(modelparam, data.pca)
    cluster.convex.hulls = .convex.hulls(modelparam, data.pca)
    # model parameters
    modelparam = utils::modifyList(list(data.pca=data.pca$x[,1:2], cluster.ellipses=cluster.ellipses, cluster.convex.hulls=cluster.convex.hulls), modelparam)
    
################################################################################
    
    ## Pam (Partitioning Around Medoids)
    
  }else if(cluster.method=="pam"){
    
    pam=cluster::pam(cluster.data,cluster.number, metric = "euclidean")
    
    #comparative parameters
    comp.parameters= list(cluster.size=pam$clusinfo[,"size"],
                      cluster.max_diss=pam$clusinfo[,"max_diss"],
                      cluster.av_diss=pam$clusinfo[,"av_diss"],
                      cluster.diameter=pam$clusinfo[,"diameter"],
                      cluster.separation=pam$clusinfo[,"separation"],
                      silhouette.info=pam$silinfo)
    
   #model parameters
    modelparam=list( data=cluster.data,
                      medoids=pam$medoids,
                      medoids.id=pam$id.med,
                      clusters=pam$clustering,
                      compare=comp.parameters)
    # PCA
    data.pca = stats::prcomp(cluster.data, scale. = T, center = T)
    # ellipses + convex hulls
    cluster.ellipses = .ellipses(modelparam, data.pca)
    cluster.convex.hulls = .convex.hulls(modelparam, data.pca)
    # model parameters
    modelparam = utils::modifyList(list(data.pca=data.pca$x[,1:2], cluster.ellipses=cluster.ellipses, cluster.convex.hulls=cluster.convex.hulls), modelparam)
    
################################################################################
    
    ## Clara (Clustering Large Applications)
    
  }else if(cluster.method=="clara"){
    
    clara=cluster::clara(cluster.data, cluster.number, metric = "euclidean", samples=100)
    
    #comparative parameters
    comp.parameters= list(
                      cluster.size=clara$clusinfo[,"size"],
                      cluster.max_diss=clara$clusinfo[,"max_diss"],
                      cluster.av_diss=clara$clusinfo[,"av_diss"],
                      cluster.diameter=clara$clusinfo[,"isolation"],
                      silhouette.info=clara$silinfo)
      
    
    #model parameters
    modelparam=list( data=cluster.data,
                      medoids=clara$medoids,
                      medoids.id=clara$i.med,
                      clusters=clara$clustering,
                      compare=comp.parameters)
    # PCA
    data.pca = stats::prcomp(cluster.data, scale. = T, center = T)
    # ellipses + convex hulls
    cluster.ellipses = .ellipses(modelparam, data.pca)
    cluster.convex.hulls = .convex.hulls(modelparam, data.pca)
    # model parameters
    modelparam = utils::modifyList(list(data.pca=data.pca$x[,1:2], cluster.ellipses=cluster.ellipses, cluster.convex.hulls=cluster.convex.hulls), modelparam)
    
################################################################################
    
    ## Fanny (Fuzzy Analysis Clustering)
    
  }else if(cluster.method=="fanny"){
    
    
    fanny=cluster::fanny(cluster.data, cluster.number, metric = "euclidean")
    
    #comparative parameters
    comp.parameters= list( 
                      membership=fanny$membership,
                      coeff=fanny$coeff,
                      memb.exp=fanny$memb.exp,
                      fanny$k.crisp,
                      fanny$objective,
                      fanny$convergence,
                      fanny$silinfo)
      
    #model parameters
    modelparam=list( 
                      data=cluster.data,
                      clusters=fanny$clustering,
                      compare=comp.parameters)
    # PCA
    data.pca = stats::prcomp(cluster.data, scale. = T, center = T)
    # ellipses + convex hulls
    cluster.ellipses = .ellipses(modelparam, data.pca)
    cluster.convex.hulls = .convex.hulls(modelparam, data.pca)
    # model parameters
    modelparam = utils::modifyList(list(data.pca=data.pca$x[,1:2], cluster.ellipses=cluster.ellipses, cluster.convex.hulls=cluster.convex.hulls), modelparam)
    

################################################################################
    
    ## Model Based Clustering
    
  }else if(cluster.method=="model"){
    mclust=mclust::Mclust(cluster.data, cluster.number)
    
    data.list = utils::combn(cluster.data, 2,simplify = F)
    
    data.list.colnames = lapply(data.list,colnames)
    
    #comparative parameters
    comp.parameters= list(  
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
    modelparam=list(
                      data.list = data.list,
                      data.list.colnames = data.list.colnames,
                      classification = mclust$classification,
                      compare = comp.parameters)
    
  }
  
################################################################################
  ## JSON Output
################################################################################
  
  # extend model parameters
  modelparam = utils::modifyList(list(cluster.method=cluster.method, cluster.number=cluster.number), modelparam)
  
  #parameters= jsonlite::toJSON(modelparam)
  return(modelparam)
}

