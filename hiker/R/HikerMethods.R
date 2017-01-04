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
# generic for extracting busts
setGeneric("busts", function(object, ...) standardGeneric("busts"))
#' @rdname HikeR-class
#' @aliases busts
#' @export
setMethod("busts",
    signature(object = "HikeR"),
    function (object, h = 0, b = object@k) {
        lpts <- object@ys[, 2] < h
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
        new("PTBB", pt = ans, type = "bust", h = h)
})
# generic for computing ridges
setGeneric("ridges", function(object, ...) standardGeneric("ridges"))
#' @rdname HikeR-class
#' @aliases ridges
#' @export
setMethod("ridges",
    signature(object = "HikeR"),
    function (object, h = 0, b = object@k) {
        N <- nrow(object@ys)
        k <- object@k
        bustp <- busts(object, h = h, b = b)@pt
        burstp <- bursts(object, h = h, b = b)@pt
        bbp <- cbind(bustp, burstp)
        ans <- zoo(FALSE, order.by = index(bustp))
        ridx <- which ( (rowSums(bbp) > 1) | (rowSums(bbp) < 1) )
        ans[ridx] <- TRUE
        ans[1:object@k] <- NA
        ans[(N - k + 1):N] <- NA
        new("PTBB", pt = ans, type = "ridge", h = h)
})
# generic for computing phases
setGeneric("phases", function(object, ...) standardGeneric("phases"))
#' @rdname HikeR-class
#' @aliases phases
#' @export
setMethod("phases",
    signature(object = "HikeR"),
    function (object, h = 0, b = object@k) {
        N <- nrow(object@ys)
        ans <- rep(NA, N)
        bustp <- busts(object, h = h, b = b)@pt
        ans[which(bustp == TRUE)] <- "bust"
        burstp <- bursts(object, h = h, b = b)@pt
        burstp
        ans[which(burstp == TRUE)] <- "burst"
        ridgep <- ridges(object, h = h, b = b)@pt
        ans[which(ridgep == TRUE)] <- "ridge"
        ans <- factor(ans)
        ans <- zoo(ans, order.by = index(object@ys))
        new("PTBB", pt = ans, type = "phase", h = h)
})
# generic for extracting topeaks
setGeneric("topeaks", function(object, ...) standardGeneric("topeaks"))
#' @rdname HikeR-class
#' @aliases topeaks
#' @export
setMethod("topeaks",
    signature(object = "HikeR"),
    function (object, h = 0) {
        N <- nrow(object@ys)
        k <- object@k
        ans <- zoo(rep(TRUE, N), order.by = index(object@ys))
        ans[1:k] <- NA
        ans[(N - k + 1):N] <- NA
        peakp <- peaks(object, h)@pt
        pidx <- which(peakp == TRUE)
        npidx <- length(pidx)
        troup <- troughs(object, h)@pt
        tidx <- which(troup == TRUE)
        ntidx <- length(tidx)
        if ( npidx == 0 ){
            warning("\nNo local peak points.\n")
            return(NULL)
        }
        if ( ntidx == 0 ){
            warning("\nNo local trough points.\n")
            return(NULL)
        }
        ## if trough comes first, set prior points to FALSE
        if ( tidx[1] < pidx[1] ){
            ans[(k + 1):tidx[1]] <- FALSE
        }
        for ( i in 1:ntidx ) {
            previouspeaks <- which(pidx < tidx[i])
            countpreviouspeaks <- length(previouspeaks)
            if ( countpreviouspeaks > 0 ){
                maxpos <- which.max(object@ys[pidx[previouspeaks], 1])
                ans[(pidx[maxpos] + 1):tidx[i]] <- FALSE
                pidx <- pidx[-c(1:countpreviouspeaks)]
            }
        }
        new("PTBB", pt = ans, type = "topeak", h = h)
})