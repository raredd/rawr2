### formatting, knitr, html-related, misc utils
# pvalr
###


#' p-value formatter
#'
#' Formats several cases of p-values; see details.
#'
#' \code{pvalr} will deal with several cases of common p-values: 1) p-values
#' which are > 0.1 will be rounded to two decimal places (and keep any trailing
#' 0s) since we are not usually interested in precision of insignificant
#' values; 2) p-values which are less than the specified \code{sig.level} will
#' be formatted as \code{< 0.001}, for example; 3) p-values that are less than
#' 0.01 but greater than \code{sig.level} will be precise to \code{digits} and
#' keep any trailing 0s.
#'
#' \code{pvalr2} deals with common cases of character string p-values which are
#' not ideal (0.000, 1.00, etc.) and will leave others unchanged.
#'
#' @param pv for \code{pvalr}, a numeric value or vector of p-values; for
#'   \code{pvalr2}, a vector of p-values as character strings
#' @param sig.limit lower bound for precision; smaller values will be shown as
#'   \code{< sig.limit}
#' @param digits number of digits past the decimal point to keep
#' @param html logical; if \code{TRUE}, uses HTML entities for \code{<}
#'   and \code{>}
#' @param show.p logical; if \code{TRUE}, inserts \code{p = }, \code{p < }, or
#'   \code{p > } where appropriate
#' @param journal logical; if \code{TRUE}, p-values greater than
#'   \code{sig.limit} are rounded to \code{digits}
#' @param limits a vector controlling the number of digits used in p-values
#' 
#'   if \code{journal = TRUE}, \code{limits} should be length one, and p-values
#'   will be rounded to two digits for values greater than \code{limits} or
#'   \code{digits} for values between \code{sig.limit} and \code{limits}
#' 
#'   if \code{journal = FALSE}, \code{limits} should be length two, and
#'   p-values will be rounded to one digit for values greater than
#'   \code{limits[2L]}, two digits for values between \code{limits[1L]} and
#'   \code{limits[2L]}, and \code{digits} for values between \code{sig.limit}
#'   and \code{limits[1L]}
#' @param ... additional arguments passed to \code{\link{format.pval}} or
#'   further to \code{\link{format}}
#'
#' @seealso
#' \code{\link[rawr]{roundr}}; \code{\link[base]{format.pval}}
#'
#' @examples
#' pv <- c(-1, 0.00001, 0.0042, 0.0601, 0.1335, 0.4999, 0.51, 0.89, 0.9, 1)
#' format.pval(pv, eps = 0.001)
#'
#' pvalr(pv)
#' pvalr(pv, journal = FALSE)
#' pvalr(pv, limits = 0.1)
#' pvalr(pv, show.p = TRUE, html = TRUE)
#'
#' @export

pvalr <- function(pv, sig.limit = 10 ^ -digits, digits = 3L, html = FALSE,
                  show.p = FALSE, journal = TRUE, limits = c(0.01, 0.5), ...) {
  stopifnot(
    sig.limit > 0,
    sig.limit < 1,
    (journal & length(limits) > 0L) |
      (!journal & length(limits) > 1L)
  )
  
  show.p <- show.p + 1L
  html <- html + 1L
  limits <- sort(limits)
  
  sapply(pv, function(x) {
    if (is.na(x) | !nzchar(x))
      return(NA)
    if (x >= 0.99)
      return(paste0(c('', 'p ')[show.p], c('> ', '&gt; ')[html], '0.99'))
    if (x >= 0.9 && !journal)
      return(paste0(c('', 'p ')[show.p], c('> ', '&gt; ')[html], '0.9'))
    if (x < sig.limit) {
      paste0(c('', 'p ')[show.p], c('< ', '&lt; ')[html],
             format.pval(sig.limit, ...))
    } else {
      nd <- if (journal)
        c(digits, 2L)[findInterval(x, c(-Inf, limits[1L], Inf))]
      else c(digits, 2L, 1L)[findInterval(x, c(-Inf, limits[1:2], Inf))]
      paste0(c('', 'p = ')[show.p], roundr(x, nd))
    }
  })
}
