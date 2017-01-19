#' @rdname PTBB-class
#' @aliases runs
#' @param x An object of S4 class \code{PTBB}.
#' @export
setMethod("runs",
          signature(x = "PTBB"), function(x){
              if ( any(na.omit(x@pt)) ){
                  p <- as.numeric(coredata(x@pt))
                  run <- rle(p)
                  runs <- which(run$values == 1)
                  cumidx <- cumsum(run$lengths)
                  n <- length(cumidx)
                  xidxright <- cumidx
                  xidxleft <- c(1, cumidx + 1)[-c(n + 1)]
                  xidx <- cbind(xidxleft, xidxright)
                  idx <- xidx[runs, ]
                  runscount <- nrow(xidx)
                  runsidx <- 1:runscount
                  ans <- sapply(runsidx, function(i)
                      x@pt[xidx[i, 1]:xidx[i, 2]])
                  pidx <- which(unlist(lapply(ans, function(r)
                      is.element(TRUE, r[1]))))
                  anstrue <- ans[pidx]
                  per <- lapply(anstrue, function(i)
                      data.frame(start(i), end(i)))
                  ans <- data.frame(do.call("rbind", per))
                  ans[, "Type"] <- x@type
                  colnames(ans) <- c("From", "To", "Type")
                  return(ans)
              } else {
                  return(NULL)
              }
          }
)