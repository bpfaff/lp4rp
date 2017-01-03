#' Peak/trough scores of time series points
#'
#' This function computes the score value for each
#' data point of a time series. The first and last
#' \code{k} observations are set to \code{NA}.
#'
#' @inheritParams score
#' @param y \code{zoo}, univariate time series.
#' @return An object of S4-class \code{HikeR}.
#' @family scores
#'
#' @references Girish K. Palshikar. Simple Algorithms for
#' Peak Detection in Time-Series. In \emph{Proc. 1st Int. Conf.
#' Advanced Data Analysis,
#' Business Analytics and Intelligence}, 2009.
#'
#' @examples
#' TEX <- SP500[, "TEX"]
#' ans <- hiker(TEX, k = 8, scoreby = "hybrid", tval = 0.1)
#' #ans
#' #plot(ans)
#'
#' @export
hiker <- function(y, k,
                  scoreby = c("vote", "avg", "diff", "max", "ent",
                              "ttype", "hybrid"),
                  tval = 1.0, confby = 3, ...){
    y <- as.zoo(y)
    ## checking arguments
    k <- as.integer(abs(k))
    ms <- 2 * k + 1L
    if ( is.null(dim(y)) ){
        yname <- "series"
        n <- length(y)
        if ( n < ms ) {
            stop(paste("Sample size of 'y' is ", n,
                       " and k = ", k, ".\n", sep = ""))
            }
    } else {
        n <- nrow(y)
        yname <- colnames(y)[1]
        if ( n < ms ) {
            stop(paste("Sample size of 'y' is ", n,
                       " and k = ", k, ".\n", sep = ""))
        }
        if ( ncol(y) > 1 ) {
            stop("Provide univariate time series of S3-class 'zoo'.\n")
        }
    }
    if ( (confby < 3) || (confby > 5) ){
        stop("\nArgument 'confby' must be integer and in set {3, 4, 5}.\n")
    }
    scoreby <- match.arg(scoreby)
    ## rolling centered window for peak scores
    s <- rollapply(y, width = ms, FUN = score,
                   k = k, scoreby = scoreby, tval = tval, ...)
    ## merging time series and scores
    ans <- merge(y, s)
    colnames(ans) <- c("Series", "Scores")
    des <- switch(scoreby,
                  vote = "majority vote",
                  avg = "average of averaged differences",
                  diff = "average of mean differences",
                  max = "average of maximum differences",
                  ent = "difference of entropies",
                  ttype = "t-type statistic",
                  hybrid = "hybrid")
    new("HikeR", ys = ans, k = k, scoreby = des, yname = yname)
}