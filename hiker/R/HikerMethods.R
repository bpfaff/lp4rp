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
# generic for extracting peaks
setGeneric("peaks", function(object, ...) standardGeneric("peaks"))
#' @rdname HikeR-class
#' @aliases peaks
#' @param h \code{numeric}, the threshold value for scores
#' to be considered as peaks/troughs.
#' @return Object of S4-class \code{PTBB}.
#' @export
setMethod("peaks",
    signature(object = "HikeR"),
    function (object, h = 0) {
        ans <- object@ys[, 2] > h
        new("PTBB", pt = ans, type = "peak", h = h)
    }
)
# generic for extracting troughs
setGeneric("troughs", function(object, ...) standardGeneric("troughs"))
#' @rdname HikeR-class
#' @aliases troughs
#' @export
setMethod("troughs",
    signature(object = "HikeR"),
    function (object, h = 0) {
        ans <- object@ys[, 2] < -h
        new("PTBB", pt = ans, type = "trough", h = h)
    }
)
# generic for extracting bursts
setGeneric("bursts", function(object, ...) standardGeneric("bursts"))
#' @rdname HikeR-class
#' @aliases bursts
#' @param b \code{integer}, intermittent count of points between peaks.
#' @export
#' @rdname HikeR-class
#' @aliases busts
#' @export
setMethod("bursts",
    signature(object = "HikeR"),
    function (object, h = 0, b = object@k) {
        lpts <- object@ys[, 2] > h
        lidx <- which(lpts == TRUE)
        nidx <- length(lidx)
        if ( nidx > 1 ){
            for ( i in 2:nidx ){
                didx <- lidx[i] - lidx[i - 1]
                if ( didx <= b ){
                    lpts[(lidx[i]):(lidx[i - 1])] <- TRUE
                }
            }
        }
        ans <- zoo(lpts, order.by = index(object@ys))
        new("PTBB", pt = ans, type = "burst", h = h)
})