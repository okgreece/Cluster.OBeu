#' @title Clustering model plotting
#' 
#' @description \code{cl.plot} function plots the clustering model constructed by the \code{\link{cl.analysis}} function.
#'
#' @param clustering.model Object returned by the \code{\link{cl.analysis}} function.
#' @param parameters List of parameters to indicate plotting of ellipses or convex hulls. Default values: \code{list(ellipses=FALSE, convex.hulls=FALSE)}.
#'
#' @author Jaroslav Kuchar <https://github.com/jaroslav-kuchar>
#' 
#' @seealso \code{\link{cl.analysis}}
#' 
#' @examples 
#' #data("iris")
#' #inputs.data <- scale(iris[,1:4])
#' #inputs.clustering <- cl.analysis(inputs.data, cl.meth="kmeans", clust.numb=3)
#' #cl.plot(inputs.clustering, parameters = list(convex.hulls=TRUE))
#' 
#' @import car
#' @importFrom grDevices chull palette 
#' @importFrom graphics legend lines par plot points
#' @importFrom methods is
#' 
#' @rdname cl.plot
#' 
#' @export

cl.plot <- function(clustering.model, parameters = list()) {
  # convert json to list if passed as json
  if(is(clustering.model,"json")){
    clustering.model <- jsonlite::fromJSON(clustering.model)
  }
  message(clustering.model$cl.meth)
  # kmeans
  if(clustering.model$cl.meth %in% c("kmeans","pam","clara","funny")) {
    # initialize
    parameters <- utils::modifyList(list(ellipses=FALSE, convex.hulls=FALSE), parameters)
    # PCA
    inputs.pca <- stats::prcomp(clustering.model$data, scale. = T, center = T)
    message("PCA summary:")
    print(summary(inputs.pca))
    par(oma=c(0, 0, 0, 5))
    graphics::plot(inputs.pca$x[,1:2], main = clustering.model$cl.meth)
    sapply(seq_along(unique(clustering.model$clusters)), function(clId) points(inputs.pca$x[which(clustering.model$clusters==unique(clustering.model$clusters)[clId]),1:2],col=palette()[clId]))
    legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,
           unique(clustering.model$clusters), lwd = 1, lty = 1, pch = 1, col = palette()[unique(clustering.model$clusters)])
    # Ellipses
    if(parameters$ellipses == TRUE){
      inputs.ellipses <- .ellipses(clustering.model, inputs.pca)
      sapply(seq_along(inputs.ellipses), function(clId) lines(inputs.ellipses[[clId]], col=palette()[clId]))
    }
    # Convex hulls
    if(parameters$convex.hulls == TRUE) {
      inputs.convexHulls <- .convex.hulls(clustering.model, inputs.pca)
      sapply(seq_along(inputs.convexHulls), function(clId) lines(inputs.convexHulls[[clId]], col=palette()[clId]))
    }
  } else {
    message(paste("Clusterring model", clustering.model$cluster.method, "is not supported!"))
  }
  invisible()
}

#' @title Computes points to plot an ellipse for each cluster of the clustering model
#' @param clustering.model Object returned by the \code{\link{cl.analysis}} function.
#' @param PCA data as result of the \code{stats::prcomp(clustering.model$data, scale. = T, center = T)}.
#' @return List of vectors with points for each ellipse.
ellipses <- function(clustering.model, data.pca) {
  lapply(
    unique(clustering.model$clusters), 
    function(cl) {
      
      if (length(data.pca$x[which(clustering.model$clusters==2),1])>1)
        car::dataEllipse(
          x=data.pca$x[which(clustering.model$clusters==2),1],
          y=data.pca$x[which(clustering.model$clusters==2),2], 
          draw=F, 
          levels=0.95, 
          segments=4) else NULL
    }
  )
}






#' @title Computes points to plot a convex hull for each cluster of the clustering model
#' @param clustering.model Object returned by the \code{\link{cl.analysis}} function.
#' @param PCA data as result of the \code{stats::prcomp(clustering.model$data, scale. = T, center = T)}.
#' @return List of vectors with points for each convex hull.
convex.hulls <- function(clustering.model, data.pca) {
  lapply(
    sort(unique(clustering.model$clusters),decreasing = FALSE),
    function(clId){
      dat <- data.pca$x[which(clustering.model$clusters==clId),1:2]
      pts <- grDevices::chull(dat)
      if (length(dat[pts])>2) dat[c(pts,pts[1]), 1:2] else dat
      #return(dat[c(pts,pts[1]), 1:2])
    }
  )
}

