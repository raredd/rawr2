### utilities
# classMethods, getMethods, dapply, clist, lextract
# 
# rawr_parse:
# parse_yaml, parse_index, parse_news, parse_namespace
# 
# unexported:
# islist, done, js


islist <- function(x) {
  ## is.list(data.frame()); rawr:::islist(data.frame())
  inherits(x, 'list')
}

done <- function(type = c('notifier', 'beep')) {
  type <- match.arg(type)
  switch(type,
         notifier = notifier::notify(
           sprintf('R task is complete - %s', format(Sys.time(), '%I:%M %p')),
           if (nzchar(Sys.getenv('RSTUDIO')))
             'RStudio' else 'R'
         ),
         beep = while (TRUE) {
           beepr::beep(3)
           Sys.sleep(3)
         }
  )
}

js <- function() {
  # <div class="fold o">
  # <div class="fold s">
  # <div class="fold s o">
  # https://stackoverflow.com/a/37839683/2994949
  system.file('scripts', 'toggleFold.js', package = 'rawr')
}

#' Show or get methods
#' 
#' List available methods for a given class or identify a specific method
#' when a \code{generic} is called with \code{object}.
#' 
#' @param object an object or character vector of classes
#' @param generic an S3 generic function like \code{plot} or \code{summary}
#' 
#' @seealso
#' \code{\link{methods}}, \code{\link{S3Methods}}, \code{\link{class}}
#' 
#' @references
#' \url{https://gist.github.com/MrFlick/55ed854eb935e5c21f71};
#' \url{https://stackoverflow.com/q/42738851/2994949}
#' 
#' @examples
#' fit <- glm(vs ~ mpg, data = mtcars)
#' classMethods(fit)
#' classMethods(c('glm', 'lm'))
#' 
#' classMethods(1, plot)
#' classMethods(data.frame(1), plot)
#' classMethods(density(1:2), plot)
#' 
#' classMethods(1, print)
#' classMethods(ordered(1), print)
#' classMethods(mtcars, summary)
#' 
#' @export

classMethods <- function(object, generic = NULL) {
  if (!is.null(generic)) {
    generic <- if (is.character(generic))
      generic else deparse(substitute(generic))
    return(genericMethods(object, generic))
  }
  
  class <- if (!is.character(object))
    class(object) else object
  message('S3 methods for objects of class ', toString(class), '\n')
  
  ml <- lapply(class, function(x) {
    sname <- gsub('([.[])', '\\\\\\1', paste0('.', x, '$'))
    m <- methods(class = x)
    if (length(m)) {
      data.frame(m = as.vector(m), c = x, n = sub(sname, '', as.vector(m)),
                 attr(m, 'info'), stringsAsFactors = FALSE)
    } else NULL
  })
  
  dd <- do.call('rbind', ml)
  dd <- dd[!duplicated(dd$n), ]
  
  structure(
    dd$m, byclass = FALSE, class = 'MethodsFunction',
    info = data.frame(visible = dd$visible, from = dd$from,
                      generic = dd$generic, isS4 = dd$isS4,
                      row.names = dd$m)
  )
}

genericMethods <- function(object, generic) {
  generic <- if (is.character(generic))
    generic else deparse(substitute(generic))
  
  f <- X <- function(x, object) {
    UseMethod('X')
  }
  
  for (m in methods(generic))
    assign(sub(generic, 'X', m), `body<-`(f, value = m))
  
  X(object)
}

#' Diagonal apply
#' 
#' Apply a function on all diagonal slices of a matrix starting from one
#' of the corners.
#' 
#' @param X a matrix or an object to be coerced
#' @param MARGIN the corner to slice from to the opposite corner: \code{1} for
#' lower left, \code{2} for top left, \code{3} for top right, or \code{4} for
#' lower right
#' @param FUN a function to apply to each slice
#' @param ... additional arguments passed to \code{FUN}
#' @param SIMPLIFY logical or character string; attempt to reduce the result to
#' a vector or matrix; see the \code{simplify} argument of \code{\link{sapply}}
#' 
#' @seealso
#' \code{\link{apply}}; \code{\link{simplify2array}}
#' 
#' @examples
#' mat <- matrix(1:12, 3)
#' dapply(mat, 2, identity)
#' 
#' dapply(mat, 2, range)
#' dapply(mat, 2, range, SIMPLIFY = FALSE)
#' 
#' @export

dapply <- function(X, MARGIN, FUN, ..., SIMPLIFY = TRUE) {
  X <- as.matrix(X)
  
  idx <- switch(
    MARGIN,
    col(X) - row(X),
    row(X) + col(X),
    (col(X) - row(X)) * -1L,
    (row(X) + col(X)) * -1L,
    stop('\'MARGIN\' should be 1, 2, 3, or 4')
  )
  
  res <- unname(lapply(split(X, idx), FUN, ...))
  
  if (!isFALSE(SIMPLIFY) && length(res)) 
    simplify2array(res, SIMPLIFY == 'array') else res
}

#' Parsers
#' 
#' Some (mostly internal) simple parsers. \code{parse_yaml} can process
#' simple, single-level yaml-like files such as the \code{DESCRIPTION} files
#' of \code{R} packages; \code{parse_index}, \code{parse_news}, and
#' \code{parse_namespace} process their respective files. These have not
#' been tested for every case, particularly \code{parse_news} since there is
#' not a requirememnt on the structure of \code{NEWS} files.
#' 
#' @param x a vector of character strings
#' @param what for \code{parse_namespace}, what types to parse and return
#' 
#' @return
#' A named list for each section type.
#' 
#' @seealso
#' \code{\link{parseNamespaceFile}}, \code{\link{lss}}, \code{\link{lsf}},
#' \code{\link{lsp}}
#' 
#' @examples
#' parse_yaml(rawr::lsf(rawr2, 'desc'))
#' parse_index(rawr::lsf(rawr2, 'index'))
#' parse_news(rawr::lsf(rawr2, 'news'))
#' parse_namespace(rawr::lsf(rawr2, 'namespace')[1:5], 'S3method')
#' 
#' @name rawr_parse
NULL

#' @rdname rawr_parse
#' @export
parse_yaml <- function(x) {
  pattern <- '(^[^:]+):\\s+?(.*)$'
  setNames(as.list(gsub(pattern, '\\2', x)), gsub(pattern, '\\1', x))
}

#' @rdname rawr_parse
#' @export
parse_index <- function(x) {
  ## collapse descriptions >1 line
  x <- strsplit(gsub('\\$\\$\\s+', ' ', paste0(x, collapse = '$$')),
                split = '\\$\\$')[[1L]]
  x <- strsplit(x, '\\s{2,}')
  as.list(sapply(x, function(xx) setNames(xx[2L], xx[1L])))
}

#' @rdname rawr_parse
#' @export
parse_news <- function(x) {
  ## assume some separator
  x <- Filter(nzchar, gsub('[-=_]{2,}', '', x))
  
  ## assume version updates state with letter, not -, *, etc
  nn <- x[idx <- grepl('^\\w+', x)]
  sp <- split(x, cumsum(idx))
  
  setNames(lapply(sp, '[', -1L), nn)
}

#' @rdname rawr_parse
#' @export
parse_namespace <- function(x, what = NULL) {
  ## remove comments and collapse
  x <- paste0(gsub('#.*$', '', x), collapse = '')
  y <- c(
    'import', 'export', 'exportPattern', 'importClass', 'importMethod',
    'exportClass', 'exportMethod', 'exportClassPattern', 'useDynLib',
    'nativeRoutine', 'S3method'
  )
  what <- if (is.null(what))
    y else match.arg(what, y, TRUE)
  
  mm <- lapply(what, function(xx)
    gregexpr(sprintf('(?i)%s\\((.*?)\\)', xx), x, perl = TRUE))
  
  setNames(
    lapply(mm, function(xx)
      gsub('^\\s+|\\s+$|\\s{2,}', '',
           unlist(strsplit(unlist(regcaptures(x, xx)), ',')))),
    what)
}

#' Concatenate lists
#' 
#' Combine lists with mixed data types "horizontally."
#' 
#' @param x,y \emph{uniquely-named} lists or nested lists with each pair
#' of non-\code{NULL} elements having identical classes
#' @param how the joining method for matrics and data frames, one of
#' \code{"cbind"} (default) or \code{"rbind"}
#' 
#' @return
#' A lists with all elements from \code{x} and \code{y} joined using
#' \code{\link{cbind}} for matrices, \code{\link{cbind.data.frame}} for
#' data frames, lists for factors, and \code{\link{c}} otherwise.
#' 
#' @seealso
#' \code{\link{nestedMerge}}, \code{\link{modifyList}}; \code{\link{bindx}}
#' 
#' @examples
#' ## boxplot stats created from subsets should be identical to
#' ## the stats generated from a single boxplot
#' 
#' f <- function(x) boxplot(mpg ~ vs, data = x, plot = FALSE)
#' 
#' bp1 <- f(mtcars[mtcars$vs == 0, ])
#' bp2 <- f(mtcars[mtcars$vs == 1, ])
#' bp  <- f(mtcars)
#' 
#' identical(clist(bp1, bp2), bp)
#' # [1] TRUE
#' 
#' 
#' l1 <- list(x = factor(1:5), y = matrix(1:4, 2),
#'            z = head(cars), l = list(zz = 1:5))
#' l2 <- list(z = head(cars), x = factor('a'),
#'            l = list(zz = 6:10))
#' l3 <- list(x = factor(1:5), y = matrix(1),
#'            z = head(cars), l = list(zz = 1:5))
#' 
#' clist(l1, l2, how = 'rbind')
#' 
#' clist(l1, l3, how = 'rbindx')[['y']]
#' # clist(l1, l3, how = 'rbind')[['y']] ## error
#' 
#' clist(l1, l3, how = 'cbindx')[['y']]
#' # clist(l1, l3, how = 'cbind')[['y']] ## error
#' 
#' 
#' ## elements of y not in x are added to result
#' clist(l1, l2, how = 'cbind')
#' clist(l1, list(zzz = data.frame(1), l = list(zz = 5:1)))
#' 
#' @export

clist <- function (x, y, how = c('cbind', 'rbind', 'cbindx', 'rbindx')) {
  if (missing(y))
    return(x)
  
  stopifnot(islist(x), islist(y))
  how <- match.arg(how)
  cbindx.data.frame <- cbindx
  rbindx.data.frame <- rbindx
  
  nn <- names(rapply(c(x, y), names, how = 'list'))
  if (is.null(nn) || any(!nzchar(nn)))
    stop('All non-NULL list elements should have unique names', domain = NA)
  
  nn <- unique(c(names(x), names(y)))
  z  <- setNames(vector('list', length(nn)), nn)
  
  bind <- function(x, y) {
    switch(
      class(x %||% y)[1L], ## class(matrix()) is length 2
      matrix = match.fun(how),
      data.frame = function(x, y)
        do.call(sprintf('%s.data.frame', how),
                Filter(Negate(is.null), list(x, y))),
      factor = function(...) unlist(list(...)), c
    )
  }
  
  for (ii in nn)
    z[[ii]] <- if (islist(x[[ii]]) && islist(y[[ii]]))
      Recall(x[[ii]], y[[ii]]) else
        (bind(x[[ii]], y[[ii]]))(x[[ii]], y[[ii]])
  
  z
}

#' List extract
#' 
#' Extract a named object from a list without knowing its specific path in
#' the list. Alternatively, return all list paths.
#' 
#' @param l a list with named element(s)
#' @param pattern a pattern to match list element(s)
#' @param path.only logical; if \code{TRUE}, matching object is not returned,
#'   only named and unnamed paths
#' @param verbose logical; if \code{TRUE}, named and unnamed paths to the
#'   matching elements are printed
#' @param all.paths logical; if \code{TRUE}, named and unnamed paths to all
#'   list elements is returned
#' 
#' @examples 
#' fit <- lm(mpg ~ wt, mtcars)
#' fit$qr$qraux
#' 
#' lextract(fit, 'qraux')
#' lextract(fit, all.paths = TRUE)
#' 
#' @export

lextract <- function(l, pattern, path.only = FALSE, verbose = TRUE, all.paths = FALSE) {
  unname2 <- function(l) {
    ## unname all lists
    ## str(unname2(lm(mpg ~ wt, mtcars)))
    l <- unname(l)
    if (inherits(l, 'list'))
      for (ii in seq_along(l))
        l[[ii]] <- Recall(l[[ii]])
    l
  }
  
  lpath <- function(l) {
    ## return all list elements with path as character string
    ## l <- lm(mpg ~ wt, mtcars); lpath(l)
    ln <- deparse(substitute(l))
    rl <- rapply(l, unclass, how = 'list')
    ll <- list(named = rl, unnamed = unname2(rl))
    
    if (identical(ll$named, ll$unnamed))
      warning('list does not have any named elements', call. = FALSE)
    
    lapply(ll, function(x) {
      x <- capture.output(x)
      paste0(ln, x[grep('^\\$|^[[]{2,}', x)])
    })
  }
  
  ln <- eval(substitute(lpath(l), list(l = match.call()$l)))
  if (all.paths)
    return(ln)
  
  idx <- grep(pattern, ln$named)
  res <- list(named = ln$named[idx], unnamed = ln$unnamed[idx])
  
  if (verbose) {
    cat(res$named, sep = '\n')
    cat('\n')
    cat(res$unnamed, sep = '\n')
    cat('\n')
  }
  
  if (path.only)
    res else setNames(lapply(idx, function(x)
      eval(parse(text = ln$named[x]))), ln$named[idx])
}
