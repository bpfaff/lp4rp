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
# generic for extracting totroughs
setGeneric("totroughs", function(object, ...) standardGeneric("totroughs"))
#' @rdname HikeR-class
#' @aliases totroughs
#' @export
setMethod("totroughs",
    signature(object = "HikeR"),
    function (object, h = 0) {
        ans <- topeaks(object, h)
        ans@pt <- !ans@pt
        ans@type <- "tottrough"
        ans
})
#' @rdname HikeR-class
#' @aliases plot
#' @param type \code{character}, whether series, scores or both should be plotted.
#' @param phase \code{character}, whether burst/bust or topeak/totrough phases
#' should be drawn in series plot.
#' @param pt.peak \code{list}, named elements are passed to \code{graphics::points()}.
#' @param main \code{character}, main title of plot.
#' @param sub \code{character}, sub title of plot
#' @param pt.trough \code{list}, named elements are passed to \code{graphics::points()}.
#' @param area.se \code{list}, named elements are passed to \code{graphics::rect()}
#' for areas of pre- and post sample points.
#' @param area.pb \code{list}, named elements are passed to \code{graphics::rect()}
#' for 'to-peak' or 'burst' phases.
#' @param area.tb \code{list}, named elements are passed to \code{graphics::rect()}
#' for 'to-trough' or 'bust' phases.
#' @export
setMethod("plot",
    signature(x = "HikeR", y = "missing"),
    function (x, type = c("series", "score", "both", "zoo"),
              h = 0, b = x@k, phase = c("none", "pt", "bb"),
              main = NULL, sub = NULL,
              pt.peak = list(col = "darkgreen", pch = 19, cex = 0.8),
              pt.trough = list(col = "darkred", pch = 19, cex = 0.8),
              area.se = list(col = "lightgray"),
              area.pb = list(col = "seagreen", density = 20),
              area.tb = list(col = "red2", density = 20),
              ...){
        type <- match.arg(type)
        phase <- match.arg(phase)
        N <- nrow(x@ys)
        xidx <- 1:N
        if ( is.null(sub) ){
            sub <- paste("Score method: ", x@scoreby,
                         ", k = ", x@k, sep = "")
        }
        if ( type == "score" ){
            srange <- range(na.omit(x@ys[, 2]))
            if ( is.null(main) ){
                main <- paste("Scores of time series:", x@yname)
            }
            plot(c(1, N), srange, axes = FALSE,
                 main = main,
                 sub = sub,
                 xlab = "Time", ylab = "Score", type = "n")
            do.call(graphics::rect, c(list(xleft = 1,
                                           ybottom = srange[1],
                                           xright = x@k,
                                           ytop = srange[2]),
                                      area.se))
            do.call(graphics::rect, c(list(xleft = N - x@k + 1,
                                           ybottom = srange[1],
                                           xright = N,
                                           ytop = srange[2]),
                                      area.se))
            graphics::lines(coredata(x@ys[, 2]), type = "h", ...)
            graphics::abline(h = 0)
            graphics::abline(h = c(h, -h), col = "red")
            graphics::box()
            graphics::axis(1, at = xidx, labels = index(x@ys), tick = FALSE)
            idx <- pretty(srange)
            graphics::axis(2, at = idx, labels = idx)
        }
        if ( type == "series" ){
            yrange <- range(na.omit(x@ys[, 1]))
            if ( is.null(main) ){
                if ( phase == "none" ){
                    main <- paste("Peaks and Troughs of:", x@yname)
                } else if ( phase == "pt" ){
                    main <- paste("To-Peak and To-Trough Phases of:", x@yname)
                }
                else if ( phase == "bb") {
                    main <- paste("Burst and Bust Phases of:", x@yname)
                }
            }
            plot(c(1, N), yrange, axes = FALSE,
                 main = main, sub = sub,
                 xlab = "Time", ylab = "", type = "n")
            do.call(graphics::rect, c(list(xleft = 1,
                                           ybottom = yrange[1],
                                           xright = x@k,
                                           ytop = yrange[2]),
                                      area.se))
            do.call(graphics::rect, c(list(xleft = N - x@k + 1,
                                           ybottom = yrange[1],
                                           xright = N,
                                           ytop = yrange[2]),
                                      area.se))
            graphics::lines(coredata(x@ys[, 1]), ...)
            peakidx <- which(peaks(x, h = h)@pt == TRUE)
            ynum <- coredata(x@ys[index(x@ys)[peakidx], 1])
            do.call(graphics::points, c(list(y = ynum, x = peakidx),
                                        pt.peak))
            troughidx <- which(troughs(x, h = h)@pt == TRUE)
            ynum <- coredata(x@ys[index(x@ys)[troughidx], 1])
            do.call(graphics::points, c(list(y = ynum, x = troughidx),
                                        pt.trough))
            if ( phase  == "bb" ){
                p <- phases(x, h = h, b = b)
                pchar <- as.character(coredata(p@pt))
                burstz <- new("PTBB",
                              pt = zoo(pchar == "burst",
                                       order.by = index(p@pt)),
                              type = "burst",
                              h = p@h)
                burstr <- runs(burstz)
                if ( !is.null(burstr) ) {
                    xleft <- which(as.character(index(burstz@pt)) %in%
                                   as.character(burstr[, "From"]))
                    xright <- which(as.character(index(burstz@pt)) %in%
                                    as.character(burstr[, "To"]))
                    for (i in 1:length(xleft)) {
                        do.call(graphics::rect, c(list(xleft = xleft[i],
                                                       ybottom = yrange[1],
                                                       xright = xright[i],
                                                       ytop = yrange[2]),
                                                  area.pb))
                    }
                }
                bustz <- new("PTBB",
                             pt = zoo(pchar == "bust",
                                      order.by = index(p@pt)),
                              type = "bust",
                             h = p@h)
                bustr <- runs(bustz)
                if ( !is.null(bustr) ) {
                    xleft <- which(as.character(index(bustz@pt)) %in%
                                   as.character(bustr[, "From"]))
                    xright <- which(as.character(index(bustz@pt)) %in%
                                    as.character(bustr[, "To"]))
                    for (i in 1:length(xleft)) {
                        do.call(graphics::rect, c(list(xleft = xleft[i],
                                                       ybottom = yrange[1],
                                                       xright = xright[i],
                                                       ytop = yrange[2]),
                                                  area.tb))
                    }
                }
            }
            if ( phase == "pt" ){
                top <- topeaks(x, h)
                topr <- runs(top)
                tot <- totroughs(x, h)
                totr <- runs(tot)
                if ( !is.null(topr) ) {
                    xleft <- which(as.character(index(top@pt)) %in%
                                   as.character(topr[, "From"]))
                    xright <- which(as.character(index(top@pt)) %in%
                                    as.character(topr[, "To"]))
                    for (i in 1:length(xleft)) {
                        do.call(graphics::rect, c(list(xleft = xleft[i],
                                                       ybottom = yrange[1],
                                                       xright = xright[i],
                                                       ytop = yrange[2]),
                                                  area.pb))
                    }
                }
                if ( !is.null(totr) ) {
                    xleft <- which(as.character(index(tot@pt)) %in%
                                   as.character(totr[, "From"]))
                    xright <- which(as.character(index(tot@pt)) %in%
                                    as.character(totr[, "To"]))
                    for (i in 1:length(xleft)) {
                        do.call(graphics::rect, c(list(xleft = xleft[i],
                                                       ybottom = yrange[1],
                                                       xright = xright[i],
                                                       ytop = yrange[2]),
                                                  area.tb))
                    }
                }
            }
            graphics::box()
            graphics::axis(1, at = xidx, labels = index(x@ys),
                           tick = FALSE)
            idx <- pretty(yrange)
            graphics::axis(2, at = idx, labels = idx)
        }
        if ( type == "both" ){
            op <- graphics::par(no.readonly = TRUE)
            graphics::par(mfrow = c(2, 1))
            plot(x, type = "series", h = h, ...)
            plot(x, type = "score", h = h, ...)
            graphics::par(op)
        }
        if ( type == "zoo" ){
            plot(x@ys, ...)
        }
    }
)