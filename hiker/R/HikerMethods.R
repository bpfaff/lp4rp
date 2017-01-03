#' @rdname HikeR-class
#' @param object An object of S4 class \code{HikeR}.
#' @export
setMethod("show",
          signature(object = "HikeR"), function(object){
              cat(paste("Peak/trough score computed as: ",
                        object@scoreby, ".\n", sep = ""))
              cat(paste("Count of left/right neighbours: ", object@k,
                        ".\n", sep = ""))
              cat("\nSummary statistics of scores:\n")
              print(summary(object))
          }
)
#' @rdname HikeR-class
#' @aliases summary
#' @param ... Ellipsis argument.
#' @export
setMethod("summary",
          signature(object = "HikeR"),
          function (object, ...){
              summary(na.omit(coredata(object@ys[, 2])))
          }
)