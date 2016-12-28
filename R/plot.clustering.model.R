#' @title Plot of clustering model
#' 
#' @description 
#' ...
#'
#' @param clustering.model
#' @param parameters
#' @return ...
#'
#' @author Jaroslav Kuchar <https://github.com/jaroslav-kuchar>
#' 
#' @examples 
#' data("iris")
#' inputs.data <- scale(iris[,1:4])
#' inputs.clustering <- cl.analysis(inputs.data,"kmeans",3)
#' plot.clustering.model(inputs.clustering, parameters = list(convex.hulls=TRUE))
#' @import car
#' @export
plot.clustering.model <- function(clustering.model, parameters = list()) {
  # convert json to list if passed as json
  if(is(clustering.model,"json")){
    clustering.model <- jsonlite::fromJSON(clustering.model)
  }
  message(clustering.model$cluster.method)
  # kmeans
  if(clustering.model$cluster.method == "kmeans") {
    # initialize
    parameters <- utils::modifyList(list(ellipses=FALSE, convex.hulls=FALSE), parameters)
    # PCA
    inputs.pca <- stats::prcomp(clustering.model$data, scale. = T, center = T)
    message("PCA summary:")
    print(summary(inputs.pca))
    par(oma=c(0, 0, 0, 5))
    plot(inputs.pca$x[,1:2], main = clustering.model$cluster.method)
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

.ellipses <- function(clustering.model, data.pca) {
  lapply(
    unique(clustering.model$clusters), 
    function(cl) car::dataEllipse(
      data.pca$x[which(clustering.model$clusters==cl),1],
      data.pca$x[which(clustering.model$clusters==cl),2], 
      draw=F, 
      levels=c(0.99), 
      segments=100)
  )
}

.convex.hulls <- function(clustering.model, data.pca) {
  lapply(
    unique(clustering.model$clusters),
    function(clId){
      dat <- data.pca$x[which(clustering.model$clusters==clId),1:2]
      pts <- chull(dat)
      return(dat[c(pts, pts[1]), 1:2])
    }
  )
}

