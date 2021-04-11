### some random things
# ht, progress, recoder, search_df, search_hist, try_require, list2file,
# Restart, helpExtract, updateR, read_clip, read_clip.csv, read_clip.tab,
# read_clip.fwf, icols, fill_df, rgene, kmer, install_temp, nestedMerge,
# nestedmerge, path_extract, fname, file_name, file_ext, rm_ext, mgrepl,
# mgrep, msub, mgsub, tree, vgrep, vgrepl, justify, factors, sparkDT, clc,
# clear
# 
# unexported:
# helpExtract_, mgrep_, msub_, fill_spaces_, render_sparkDT
###


#' \code{head}/\code{tail}
#' 
#' \code{\link{rbind}} the \code{\link{head}} and \code{\link{tail}} of an
#' object.
#' 
#' @param x an object
#' @param n an integer giving the first and last \code{n / 2} elements if
#' positive or the middle \code{n} elements if negative
#' @param sep separator
#' 
#' @examples
#' ht(letters, 6, '...')
#' ht(letters, -6)
#' 
#' 
#' mt <- cbind(mtcars, n = seq.int(nrow(mtcars)))
#' 
#' ## ends
#' ht(mt)
#' ht(mt, sep = '...')
#' 
#' ## middle
#' ht(as.matrix(mt), -6)
#' ht(mt, -6)
#' ht(mt, -6, sep = '...')
#' 
#' @export

ht <- function(x, n = 6L, sep = NULL) {
  pn <- abs(n) / 2
  FUN <- if (is.null(dim(x)))
    function(...) setNames(c(...), NULL) else 'rbind'
  FUN <- match.fun(FUN)
  
  if (n < 0L) {
    idx <- cut(seq.int(NROW(x)), breaks = 2L, labels = 1:2)
    x <- if (is.null(dim(x)))
      list(x[idx %in% '1'], x[idx %in% '2'])
    else
      list(
        x[idx %in% '1', , drop = FALSE],
        x[idx %in% '2', , drop = FALSE]
      )
    
    FUN(' ' = sep, tail(x[[1L]], pn), head(x[[2L]], pn), '  ' = sep)
  } else FUN(head(x, pn), ' ' = sep, tail(x, pn))
}

#' Progress function
#' 
#' Displays the percent (or iterations) completed during some loop.
#' 
#' @param value numeric; i-th iteration or percent completed (values 0-100)
#' @param max.value numeric; n-th iteration; if missing, will assume percent
#' completion is desired
#' @param textbar logical; if \code{TRUE}, uses text progress bar which will
#' span across the console width; see \code{\link{options}}
#' 
#' @examples
#' \dontrun{
#' iterations <- 77
#' ## percent completed:
#' for (ii in 1:iterations) {
#'    progress(ii / iterations * 100)
#'    Sys.sleep(.01)
#' }
#' 
#' ## iterations completed
#' for (ii in 1:iterations) {
#'    progress(ii, iterations)
#'    Sys.sleep(.01)
#' }
#'
#' ## text progress bar
#' for (ii in 1:iterations) {
#'    progress(ii, iterations, textbar = TRUE)
#'    Sys.sleep(.01)
#' }
#' }
#' 
#' @export

progress <- function(value, max.value, textbar = FALSE) {
  if (!is.numeric(value))
    stop('\'value\' must be numeric')
  
  oo <- options(scipen = 10)
  on.exit(options(oo))
  
  percent <- if (missing(max.value)) {
    max.value <- 100
    TRUE
  } else FALSE
  
  f <- function(...) paste0(..., collapse = '')
  erase.only <- value > max.value
  max.value <- as.character(round(max.value))
  l <- nchar(max.value)
  # value <- formatC(round(value), width = l, format = 'd')
  # max.value <- formatC(max.value, width = l, format = 'd')
  
  if (textbar) {
    # m <- getOption('width')
    # r <- trunc(as.numeric(value) / as.numeric(max.value) * m)
    # backspaces <- f(rep('\b', m * 2))
    #
    # if (erase.only) message <- ''
    # else {
    #   message <- f('|', f(rep('=', max(0, r - 1))),
    #                f(rep(' ', max(0, m - r))), '|')
    #   cat(backspaces, message, sep = '')
    # }
    m <- getOption('width') - 5L
    pct <- as.numeric(value) / as.numeric(max.value)
    r <- trunc(pct * m)
    backspaces <- f(rep('\b', m * 2))
    
    message <- if (erase.only)
      '' else {
        message <- f('|', f(rep('=', max(0, r - 1))),
                     f(rep(' ', max(0, m - r))), '|')
        cat(backspaces, message, sprintf('  %s%%', round(pct * 100)), sep = '')
      }
  } else {
    if (percent) {
      backspaces <- f(rep('\b', l + 14L))
      message <- if (erase.only)
        '' else sprintf('Progress: %s%%', round(value))
      cat(backspaces, message, sep = '')
    } else {
      backspaces <- f(rep('\b', 2 * l + 17L))
      message <- if (erase.only)
        '' else sprintf('Progress: %s of %s  ', value, max.value)
      cat(backspaces, message, sep = '')
    }
  }
  if (.Platform$OS.type == 'windows')
    flush.console()
  cat('\n')
}

#' Recode variables
#' 
#' Recodes numeric, character, and factor values in a vector, list, matrix,
#' or data frame.
#' 
#' When recoding a factor variable with a new level, \code{recoder}
#' automatically adds the corresponding level to \code{levels(object)} to
#' avoid errors.
#' 
#' The function currently recursively replaces \code{pattern[i]} with
#' \code{replacement[i]} in sequential order, so if you intend to swap values,
#' say \code{a} and \code{b}, in an \code{object}, \code{recoder} will instead
#' first replace all occurrences of \code{a} with \code{b} and then all
#' occurrences of \code{b} with \code{a} resulting in the \code{object} with
#' no \code{b} occurrences; see examples. I will (may) fix this eventually.
#' 
#' @param object object to recode
#' @param pattern what to replace
#' @param replacement what to replace \code{pattern} with
#' @param ... ignored
#' 
#' @return
#' An object with the same length (or dimensions) and class as \code{object}
#' with the recoded variables.
#' 
#' @seealso
#' \code{\link{fill_df}}; \code{\link[car]{recode}};
#' \code{\link{combine_levels}}
#' 
#' @examples
#' recoder(mtcars$carb, c(1, 2), c('A', 'B'))
#' recoder(mtcars, c(1, 2), c('A', 'B'))
#' 
#' mtcars <- within(mtcars, carb1 <- factor(carb))
#' recoder(mtcars$carb1, 1, 999)
#' 
#' tmp <- c(list(1:5), list(5), list(NA))
#' recoder(tmp, 5, NA)
#' 
#' ## example from note
#' tmp <- 1:10
#' recoder(tmp, c(1, 2), c(2, 1))
#' # [1]  1  1  3  4  5  6  7  8  9 10    ## actual return
#' # [1]  2  1  3  4  5  6  7  8  9 10    ## desired return
#' 
#' @export

recoder <- function(object, pattern, replacement, ...) {
  ## to do:
  # add swapping option
  # add expression option, eg, if object[i, j] > 0, use replacement
  # fix level printing: DONE
  # allow NA for input: DONE
  # need to recode factor and numeric NAs simultaneously?
  
  m <- match.call()
  op <- options(stringsAsFactors = FALSE)
  on.exit(options(op))
  
  if (is.factor(object)) {
    lvl <- setdiff(replacement, levels(object))
    if (length(lvl))
      cat('level(s)', levels(factor(levels = lvl)),
          'added to factor variable', deparse(m$object), '\n')
    levels(object) <- c(levels(object), replacement)
    # object <- droplevels(object)
  }
  if (length(replacement) == 1L)
    replacement <- rep(replacement, length(pattern))
  
  ## helper functions
  splitter <- function(df) {
    setNames(split(t(df), seq.int(ncol(df))), names(df))
  }
  
  switcher <- function(f, g, h) {
    if (is.na(g))
      f[is.na(f)] <- h
    else
      f[f == g] <- h
    f
  }
  
  superswitcher <- function(x, y, z) {
    DF <- data.frame(y, z, stringsAsFactors = FALSE)
    z <- x
    if (class(DF[, 2L]) %in% c('character', 'factor')) {
      lapply(seq.int(nrow(DF)), function(i) {
        if (sum(z %in% DF[i, 1]) == 0) {
          z <<- z
        } else {
          z <<- switcher(z, DF[i, 1L], as.character(DF[i, 2L]))
        }
      })
    } else {
      lapply(seq.int(nrow(DF)), function(i) {
        z <<- switcher(z, DF[i, 1L], DF[i, 2L])
      })
    }
    z
  }
  
  # treat certain object classes differently
  if (is.vector(object) & !is.list(object)) {
    sapply(object, superswitcher, pattern, replacement)
  } else {
    if (is.data.frame(object)) {
      tmp <- do.call('data.frame',
                     lapply(unclass(object)[seq.int(ncol(object))],
                            superswitcher, pattern, replacement))
      rownames(tmp) <- attr(object, 'row.names')
      return(tmp)
    }
    if (is.matrix(object)) {
      nrow <- nrow(object)
      tmp <- do.call('rbind',
                     lapply(object, superswitcher, pattern, replacement))
      tmp <- matrix(tmp, nrow = nrow, byrow = FALSE)
      return(tmp)
    } else {
      if (is.factor(object))
        factor(unlist(lapply(object, superswitcher, pattern, replacement)),
               levels(object), ordered = is.ordered(object))
      else lapply(object, superswitcher, pattern, replacement)
    }
  }
}

#' Search function for data frames
#' 
#' Searches a data frame column for matches.
#' 
#' @param pattern string to find
#' @param data data frame to search
#' @param col.name column name in \code{data} to search
#' @param var variation; maximum distance allowed for a match; see
#' \code{\link{agrep}}
#' @param ignore.case logical; if \code{FALSE}, the pattern matching is
#' \emph{case-sensitive}, and if \code{TRUE}, case is ignored during matching
#' @param ... additional arguments passed to \code{\link{agrep}}
#' 
#' @return
#' Subset of the original \code{data} where the \code{pattern} was found in
#' the specified \code{col.name}.
#' 
#' @examples
#' dd <- data.frame(islands = names(islands)[1:32], mtcars)
#' search_df(New, dd, islands)
#' search_df(ho, dd, islands, var = 0.2) # too much variation
#' search_df(ho, dd, islands, var = 0)
#' search_df('Axel Hieberg', dd, islands) # misspelled, not enough variation
#' search_df('Axel Hieberg', dd, islands, var = 2)
#' search_df(19, dd, mpg)
#' 
#' @export

search_df <- function(pattern, data, col.name, var = 0,
                      ignore.case = TRUE, ...) {
  p <- as.character(substitute(pattern))
  x <- as.character(substitute(col.name))
  idx <- agrep(p, data[, x], ignore.case = ignore.case,
               max.distance = var, ...)
  data[idx, ]
}

#' Search history
#' 
#' Searches \code{.Rhistory} file for pattern matches.
#' 
#' @param x numeric or character; if numeric, shows the most recent \code{n}
#' lines in \code{.Rhistory}; if character, searches for pattern matches
#' @param ... additional arguments passed to \code{\link{grep}}
#' 
#' @return
#' A list of recent commands that match \code{pattern}.
#' 
#' @examples
#' search_hist()
#' search_hist(25)
#' search_hist('?')
#' search_hist('?', fixed = TRUE)
#' search_hist('\\?')
#' 
#' @export

search_hist <- function (x, ...) {
  hist <- tryCatch(readLines('.Rhistory'),
                   warning = function(w) message('No history found'),
                   finally = return(invisible(NULL)))
  lhist <- length(hist)
  
  if (is.numeric(x))
    hist[lhist:(lhist - x + 1L)]
  else if (is.character(x))
    grep(x, readLines('.Rhistory'), value = TRUE, ...)
}

#' Quietly try to require a package
#' 
#' Quietly require a package, returning an error message if not installed.
#' 
#' @param package name of package as name or character string
#' 
#' @export

try_require <- function(package) {
  package <- ifelse(!is.character(substitute(package)),
                    as.character(substitute(package)), package)
  available <- suppressMessages(
    suppressWarnings(
      sapply(package, require, quietly = TRUE,
             character.only = TRUE, warn.conflicts = FALSE)
    ))
  missing <- package[!available]
  
  if (length(missing) > 0L)
    stop(paste(package, collapse = ', '), ' package not found.')
}

#' List to file
#' 
#' Save a \emph{named} list of data frames or matrices into \code{R} data files
#' \code{.rda}, \code{.csv}, or \code{.txt} files.
#' 
#' @param l a list of data frames or matrices
#' @param targetdir target directory (created if doesn't exist)
#' @param sep field separator string; default is none which results in
#' \code{.rda} data files; "\code{,}" creates \code{.csv} files; any other
#' separator will create \code{.dat} files
#' @param ... additional arguments passed to \code{\link{save}} if \code{sep}
#' is not given or to \code{\link{write.table}} if \code{sep} is given
#' 
#' @return
#' \code{list2file} will create \code{length(l)} files in the \code{targetdir}.
#' 
#' @examples
#' \dontrun{
#' dfl <- setNames(list(mtcars, iris), c('mtcars','iris'))
#' 
#' ## .csv files
#' list2file(dfl, '~/desktop/tmp', sep = ',')
#' 
#' ## r data files
#' list2file(dfl, '~/desktop/tmp')
#' }
#' 
#' @export

list2file <- function(l, targetdir = getwd(), sep, ...) {
  if (!islist(l))
    stop('\'l\' must be a list')
  if (is.null(names(l)) || any(is.na(names(l))))
    stop('all elements of \'l\' must be named')
  if (!any(sapply(l, class) %in% c('data.frame', 'matrix')))
    stop('all elements of \'l\' should be class \'matrix\' or \'data.frame\'')
  if (!file.exists(targetdir)) {
    message(sprintf('creating directory:\n%s', targetdir))
    dir.create(targetdir)
  }
  
  e <- new.env()
  list2env(l, envir = e)
  
  if (missing(sep))
    sapply(names(l), function(x)
      save(x, file = sprintf('%s/%s.rda', targetdir, x), ...))
  else sapply(names(l), function(x)
    write.table(get(x, envir = e), sep = sep, ...,
                file = sprintf('%s/%s.%s', targetdir, x,
                               ifelse(sep == ',', 'csv', 'dat'))))
  
  message(sprintf('NOTE: %s written to %s', iprint(names(l)), targetdir))
  
  invisible(NULL)
}

#' Restart \code{R} session
#' 
#' Ends current and restarts a clean \code{R} session.
#' 
#' @param afterRestartCommand character string of command(s) to be
#' executed after restarting
#' 
#' @examples
#' \dontrun{
#' Restart("clear(); cat('Here is a clean session just for you')")
#' }
#' 
#' @export

Restart <- function(afterRestartCommand = '') {
  (getOption('restart'))(afterRestartCommand)
}

# Reload <- function(...) {
#   ## clean (rstudio) r session packages:
#   pkgs <- c(".GlobalEnv", "tools:rstudio", "package:stats", "package:graphics",
#             "package:grDevices", "package:utils", "package:datasets",
#             "package:methods", "Autoloads", "package:base")
#   to_unload <- setdiff(search(), pkgs)
#   
#   for (pkg in to_unload)
#     try(detach(pkg, unload = TRUE, character.only = TRUE), silent = TRUE)
#   rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
#   cat('\014')
#   
#   invisible(NULL)
# }

#' Extract \code{R} help files
#' 
#' Extracts specified portions of R help files (from \emph{loaded} libraries)
#' for use in Sweave or R-markdown documents.
#' 
#' The \code{type} argument accepts:
#' 
#' \tabular{llllll}{
#' \tab \code{text}    \tab \tab \tab \tab plain text \cr
#' \tab \code{md_code} \tab \tab \tab \tab markdown code chunks; for use with
#' markdown documents when highlighted code is expected \cr
#' \tab \code{md_text} \tab \tab \tab \tab markdown plain text; for use with
#' markdown documents where regular text is expected \cr
#' \tab \code{sw_code} \tab \tab \tab \tab sweave code chunks; for use with
#' Sweave documents where highlighted code is expected \cr
#' \tab \code{sw_text} \tab \tab \tab \tab sweave plain text; for use with
#' Sweave documents where regular text is expected \cr
#' }
#' 
#' To see the results in the console:
#' 
#' \code{cat(helpExtract(print, type = 'md_text'))}
#' 
#' To insert a (highlighted) chunk into a markdown document:
#' 
#' \verb{
#' ```{r, results='asis'}
#' cat(helpExtract(print), sep ='\n')
#' ```
#' }
#' 
#' To insert a (highlighted) chunk into a Sweave document:
#' 
#' \verb{
#' \\Sexpr{knit_child(textConnection(helpExtract(print, type = 's_code')),
#'      options = list(tidy = FALSE, eval = FALSE))}
#' }
#' 
#' @param FUN a function as name or character string
#' @param show.sections logical; if \code{TRUE}, returns \code{section} options
#' for \code{FUN}
#' @param section section to extract (default is \code{"Usage"}
#' @param type type of character vector you want returned; default is
#' \code{"m_code"}, see details
#' @param ... additional arguments passed to \code{\link[utils]{help}}
#' 
#' @return
#' A character vector to be used in a Sweave or Rmarkdown document.
#' 
#' @examples
#' helpExtract(print)
#' cat(helpExtract(print, section = 'ex'), sep = '\n')
#' cat(helpExtract(print, type = 'md_text', section = 'description'))
#' 
#' ## selecting multiple sections prints section names
#' cat(helpExtract(print, section = c('references', 'see also')), sep = '\n')
#' 
#' @export

helpExtract <- function(FUN, show.sections = FALSE, section = 'Usage',
                        type = c('text','md_code','md_text',
                                 'sw_code','sw_text'), ...) {
  type <- match.arg(type)
  FUN  <- if (is.function(FUN))
    deparse(substitute(FUN)) else as.character(FUN)
  x <- helpExtract_(FUN, ...)
  
  ## section start lines
  B <- grep('^_\b._\b._', x)
  x <- gsub('_\b', '', x, fixed = TRUE)
  if (show.sections)
    return(gsub(':','', x[B]))
  
  X <- rep_len(0L, length(x))
  X[B] <- 1L
  res <- split(x, cumsum(X))
  
  res <- res[which(sapply(res, function(x)
    any(Vectorize(grepl)(section, x[1L], ignore.case = TRUE))))]
  # res <- unlist(sapply(res, '[', -(1:2)))
  res <- if (length(section) > 1L)
    unname(unlist(res)) else res[[1L]][-(1:2)]
  
  while (TRUE) {
    res <- res[-length(res)]
    if (nzchar(res[length(res)]))
      break
  }
  
  switch(
    type,
    text    = res,
    md_code = c('```r', res, '```'),
    sw_code = c('<<>>=', res, '@'),
    md_text = paste('    ', res, collapse = '\n'),
    sw_text = c('\\begin{verbatim}', res, '\\end{verbatim}')
  )
}

helpExtract_ <- function(FUN, ...) {
  # (helpExtract_('print'))
  stopifnot(is.character(FUN))
  
  ## tools:::fetchRdDB
  fetchRdDB <- function(filebase, key = NULL) {
    fun <- function(db) {
      vals <- db$vals
      vars <- db$vars
      datafile <- db$datafile
      compressed <- db$compressed
      envhook <- db$envhook
      fetch <- function(key)
        lazyLoadDBfetch(vals[key][[1L]], datafile, compressed, envhook)
      if (length(key)) {
        if (!key %in% vars)
          stop(gettextf("No help on %s found in RdDB %s",
                        sQuote(key), sQuote(filebase)), domain = NA)
        fetch(key)
      } else {
        res <- lapply(vars, fetch)
        names(res) <- vars
        res
      }
    }
    res <- lazyLoadDBexec(filebase, fun)
    if (length(key))
      res else invisible(res)
  }
  
  ## utils:::.getHelpFile
  getHelpFile <- function(file) {
    path <- dirname(file)
    dirpath <- dirname(path)
    if (!file.exists(dirpath))
      stop(gettextf("invalid %s argument", sQuote("file")), domain = NA)
    pkgname <- basename(dirpath)
    RdDB <- file.path(path, pkgname)
    if (!file.exists(paste(RdDB, "rdx", sep = ".")))
      stop(gettextf(paste("package %s exists but was not installed under R',
                          '>= 2.10.0 so help cannot be accessed"),
                    sQuote(pkgname)), domain = NA)
    fetchRdDB(RdDB, basename(file))
  }
  
  x <- capture.output(
    tools::Rd2txt(getHelpFile(utils::help(FUN, ...)),
                  options = list(sectionIndent = 0))
  )
  
  invisible(x)
}

#' Update \code{R}
#' 
#' Copies and updates \code{R} libraries from most recent installed version
#' into the current \code{\link{.libPaths}} directory. This assumes that the
#' user has installed a new \code{X.x} version of \code{R} but will not copy
#' any libraries from previous frameworks into the new library.
#' 
#' @param update logical; if \code{TRUE}, checks for available packages
#' updates, downloads, and installs
#' 
#' @seealso
#' \code{\link{update.packages}}
#' 
#' @export

updateR <- function(update = TRUE) {
  path <- file.path(R.home(), '..', '..')
  
  v <- tail(sort(list.files(path, pattern = '^\\d{1}.\\d{1}$')), 2L)
  if (!grepl(v[2L], .libPaths()))
    stop('A more recent version of R was found on your system\n')
  
  if (file.exists(v_last <- sub(v[2L], v[1L], .libPaths()))) {
    pkg <- list.files(.libPaths())
    pkg <- setdiff(list.files(v_last), pkg)
    if (length(pkg) > 0L) {
      cat(sprintf("Copying %s package%s to %s\n", length(pkg),
                  ifelse(length(pkg) > 1L, 's', ''), .libPaths()))
      file.copy(file.path(v_last, pkg), .libPaths(), recursive = TRUE)
    } else cat('No packages to copy\n')
  }
  
  if (update) {
    if ((up <- table(packageStatus()$inst$Status)['upgrade']) > 0L) {
      cat(sprintf('Updating %s package%s\n', up, ifelse(up > 1L, 's', '')))
      update.packages(ask = FALSE)
    } else cat('All packages are up-to-date\n')
  }
}

#' Read data from clipboard
#' 
#' Reads data (comma-, tab-, or fixed-width separated) data from clipboard and
#' returns as a data frame.
#' 
#' @param header logical; indicates if variable names are in first line
#' @param ... additional arguments passed to \code{\link{read.table}}
#' 
#' @seealso
#' \code{\link{read.table}}; \code{\link{read.fwf}}
#' 
#' @export

read_clip <- function(header = TRUE, ...) {
  if (!Sys.info()['sysname'] %in% 'Darwin')
    read.table(file = 'clipboard', header = header, ...)
  else read.table(file = pipe('pbpaste'), header = header, ...)
}

#' @rdname read_clip
#' @param sep separator as a character string
#' @export
read_clip.csv <- function(header = TRUE, sep = ',', ...) {
  read_clip(header = header, sep = sep, ...)
}

#' @rdname read_clip
#' @export
read_clip.tab <- function(header = TRUE, sep = '\t', ...) {
  read_clip(header = header, sep = sep, ...)
}

#' @rdname read_clip
#' @param widths a vector of widths of the fixed-width fields or a list of
#' vectors giving the widths for multiple lines
#' @export
read_clip.fwf <- function(header = TRUE, widths, ...) {
  if (!Sys.info()['sysname'] %in% 'Darwin')
    read.fwf(file = 'clipboard', header = header, widths = widths, ...)
  else read.fwf(file = pipe('pbpaste'), header = header, widths = widths, ...)
}

#' Index columns by pattern
#' 
#' Quickly selects and returns columns from a matrix or data frame by
#' \code{\link{grep}}'ing for a desired \code{pattern}.
#' 
#' @param x a matrix or data frame
#' @param pattern pattern to match
#' @param keep optional vector of names of other columns to keep
#' @param ... additional parameters passed to \code{\link{grep}}
#' 
#' @examples
#' icols(iris, 'Petal')
#' icols(iris, '\\.')
#' icols(mtcars, '^[\\w]{2}$')
#' 
#' @export

icols <- function(x, pattern, keep, ...) {
  keep <- if (missing(keep))
    NULL else which(colnames(x) %in% keep)
  x[, c(keep, grep(pattern, colnames(x), perl = TRUE, ...)), drop = FALSE]
}

#' Fill data frame
#' 
#' Fills data frame, \code{data}, containing \code{NA} values using a look-up
#' table, \code{key}. \code{ids} and \code{fill} columns must be in both
#' \code{data} and \code{key}. If neither are given, \code{fill_df} will
#' smartly try to guess which columns need to be filled with the values from
#' the look-up table.
#' 
#' @param data a data frame to recode
#' @param key a look-up table data frame
#' @param ids columns treated as id variables, as character strings or indices
#' @param fill columns to recode, as character strings or column indices
#' @param values optional vector of values to recode with \code{fill}; if
#' missing (default), \code{fill_df} only looks for \code{NA}s in \code{data};
#' otherwise, all occurrences of \code{values} will be replaced with
#' \code{NA}, and \code{fill_df} will procede normally
#' 
#' @return
#' A data frame with \code{NA}s from \code{fill}-columns recoded to match
#' the values from \code{key}.
#' 
#' @seealso
#' \code{\link{recoder}}; \code{\link{locf}}
#' 
#' @examples
#' dd <- mtcars
#' dd[matrix(sample(c(TRUE, FALSE), 32 * 11, replace = TRUE), 32)] <- NA
#' identical(mtcars, fill_df(dd, mtcars))  ## TRUE
#' 
#' ## recode other variables instead of NAs
#' nn <- sum(is.na(dd))
#' dd[is.na(dd)] <- sample(-10:-1, nn, replace = TRUE)
#' identical(mtcars, fill_df(dd, mtcars, values = -1:-10)) ## TRUE
#' 
#' f <- function(x, n = 20) sample(x, size = n, replace = TRUE)
#' set.seed(1)
#' key_df <- data.frame(id = c(1,2,1,2), group = c(3,3,4,4),
#'                      x = c(100, 200, 300, 400), y = I(LETTERS[1:4]))
#' na_df <- data.frame(id = f(1:2), group = f(3:4),
#'                     x = f(c(0, NA)), y = I(f(c('', NA))), z = 1)
#' 
#' ## auto: all cols with no NAs == ids; cols with any NAs = fill
#' fill_df(na_df, key_df)
#' 
#' ## select which to be filled and returned
#' fill_df(na_df, key_df, ids = 1:2, fill = 'x')
#' fill_df(na_df, key_df, ids = 1:2, fill = 4)
#' 
#' @export

fill_df <- function(data, key, ids, fill, values) {
  nn <- names(data)
  ## if given replace "values" with NAs
  if (!missing(values)) {
    idx <- data
    idx[] <- lapply(data, function(x) x %in% values)
    data[as.matrix(idx)] <- NA
  }
  
  ## get columns names not defined as ids or fill
  if (length(whk <- which(!nn %in% names(key)))) {
    whk <- nn[whk]
    keep <- data[, whk, drop = FALSE]
    data[, whk] <- NULL
  } else keep <- NULL
  
  ## error checks
  nd <- names(data)
  nad <- vapply(data, anyNA, logical(1L))
  if (all(!nad))
    return(data)
  
  ## try to guess columns to use for ids/fill
  ids <- if (missing(ids)) {
    ids <- nd[which(!nad)]
    message('\'ids\' : ', paste(ids, collapse = ', '), domain = NA)
    ids
  } else if (is.numeric(ids)) nd[ids] else ids
  fill <- if (missing(fill)) {
    fill <- nd[which(nad)]
    message('\'fill\': ', paste(fill, collapse = ', '), domain = NA)
    fill
  } else if (is.numeric(fill))
    nd[fill] else fill
  
  ## match current data rows with rows in key and fill NAs
  ok <- all(nad)
  nak <- if (ok)
    seq.int(nrow(data)) else do.call('paste0', c(key[, ids, drop = FALSE]))
  
  dfk <- if (ok)
    seq.int(nrow(data)) else do.call('paste0', c(data[, ids, drop = FALSE]))
  mm <- match(dfk, nak)
  
  for (col in fill) {
    nnr <- which(is.na(data[, col]))
    data[nnr, col] <- key[mm[nnr], col]
  }
  
  # data[do.call('order', as.list(data[, c(nnk, nnf)])), ]
  if (!is.null(keep))
    cbind.data.frame(data, keep)[, nn, drop = FALSE]
  else data[, nn, drop = FALSE]
}

#' Generate random gene names
#' 
#' Generate random character strings from pools of letters and digits.
#' 
#' @param n number of gene names to return
#' @param alpha,num vectors of letters or numerics to select from
#' @param nalpha,nnum range of possible number of \code{alpha} or \code{num}
#' to select
#' @param sep character to separate \code{alpha} and \code{num}
#' 
#' @examples
#' rgene()
#' 
#' ## letters only
#' rgene(5, alpha = c('A', 'T', 'C', 'G'), nalpha = 10, num = NULL)
#' 
#' ## fixed prefix (or suffix)
#' rgene(5, alpha = 'ABCD', nalpha = 1, nnum = 5)
#' 
#' @export

rgene <- function(n = 1L, alpha = LETTERS, nalpha = 1:5,
                  num = 0:9, nnum = nalpha, sep = '-') {
  s <- function(x, n) {
    if (is.null(x))
      NULL else sample(x, n[sample.int(length(n), 1)], TRUE)
  }
  p <- function(...) {
    paste0(..., collapse = '')
  }
  
  if (is.null(alpha) || is.null(num) ||
      identical(alpha, '') || identical(num, ''))
    sep <- ''
  
  replicate(n, p(p(s(alpha, nalpha)), sep, p(s(num, nnum))))
}

#' k-mers
#' 
#' Generate all k-mers from a sequence of characters.
#' 
#' @param x a vector of character string(s)
#' @param k length(s) of sub-sequences
#' 
#' @examples
#' x <- 'AATTGCGCGCTGT'
#' kmer(x, 3)
#' kmer(x, 3:4)
#' 
#' @export

kmer <- function(x, k) {
  kmerl <- function(x, k) {
    i <- seq.int(nchar(x) - k + 1)
    substring(x, i, i + k - 1)
  }
  
  l <- max(length(x), length(k))
  res <- Map(kmerl, rep_len(x, l), rep_len(k, l))
  names(res) <- sprintf('%s, k=%s', x, k)
  
  if (length(res) == 1L)
    res[[1L]] else res
}

#' Install packages temporarily
#' 
#' This function will create a temporary \code{.libPath}, install, and load
#' packages for use in a single \code{R} session. \cr \cr To install a repo
#' from github temporarily, use \code{\link[withr]{with_libpaths}}.
#' 
#' @param pkgs character vector of the names of packages whose current
#' versions should be downloaded from the repositories
#' @param lib character vector giving the library directories where to install
#' \code{pkgs}; recycled as needed; if missing (default), a
#' \code{\link{tempdir}} will be created
#' @param ... additional arguments passed to
#' \code{\link[utils]{install.packages}}
#' 
#' @examples
#' \dontrun{
#' install_temp(c('devtools', 'testthat'))
#' }
#' 
#' @export

install_temp <- function(pkgs, lib, ...) {
  if (missing(lib))
    lib <- tempdir()
  
  ## resetting libPaths before restarting r session may not be desired
  # lp <- .libPaths()
  # on.exit(.libPaths(lp))
  .libPaths(lib)
  utils::install.packages(pkgs = pkgs, lib = lib, ...)
  
  for (ii in pkgs)
    try(require(ii, character.only = TRUE))
  
  invisible(NULL)
}

#' Merge nested lists
#' 
#' Recursive functions to merge nested lists.
#' 
#' \code{nestedmerge} recursively calls itself to merge similarly-structured
#' named \emph{or} unnamed lists. Unnamed lists results in a "horizontal"
#' merge; named lists will be matched based on names. In either case, the
#' matching element (or list(s) of elements(s)) should also have the same
#' structure.
#' 
#' \code{nestedMerge} is a convenience wrapper for \code{nestedmerge} in cases
#' where list \code{a} contains elements not in list \code{b}. If using
#' \code{nestedmerge} in this case, only elements of list \code{a} will be
#' merged and returned.
#' 
#' @param x,y lists
#' 
#' @seealso
#' \code{\link{clist}}; adapted from
#' \url{http://stackoverflow.com/questions/23483421/combine-
#' merge-lists-by-elements-names-list-in-list}
#' 
#' @examples
#' ## l1 and l2 have similar structures
#' l1 <- list(a = list(1:2, NULL), b = list(1:3, NULL), c = list(1:5))
#' l2 <- list(a = list(NULL, 0:1), b = list(NULL, 4:6))
#' l3 <- list(a = list(NULL, 0:1), b = list(4:6))
#' 
#' nestedMerge(l1, l2)
#' 
#' ## "fails" for `b` since `l1$b` and `l3$b` are not structured similarly
#' nestedMerge(l1, l3)
#' 
#' l1 <- list(integers = 1:3, letters = letters[1:3],
#'            words = c('two','strings'), rand = rnorm(5))
#' l2 <- list(letters = letters[24:26], booleans = c(TRUE, TRUE, FALSE),
#'            words = 'another', floating = c(1.2, 2.4),
#'            integers = 1:3 * 10)
#'            
#' nestedMerge(l1, l2)
#' 
#' ## compare to
#' nestedmerge(l1, l2)
#' 
#' @export

nestedMerge <- function(x, y) {
  if (missing(y))
    return(x)
  if (islist(x) & islist(y)) {
    nn <- setdiff(names(y), names(x))
    x <- c(x, setNames(vector('list', length(nn)), nn))
  }
  
  nestedmerge(x, y)
}

#' @rdname nestedMerge
#' @export
nestedmerge <- function(x, y) {
  if (missing(y))
    return(x)
  
  if (islist(x) & islist(y)) {
    res <- list()
    if (!is.null(names(x))) {
      for (nn in names(x)) {
        res <- if (nn %in% names(y) && !is.null(y[[nn]]))
          append(res, c(Recall(x[[nn]], y[[nn]]))) else
            append(res, list(x[[nn]]))
        names(res)[length(res)] <- nn
      }
    } else {
      for (ii in seq_along(x))
        res <- if (ii <= length(y) && !is.null(y[[ii]]))
          append(res, Recall(x[[ii]], y[[ii]])) else
            append(res, list(x[[ii]]))
    }
    res
  } else list(c(x, y))
}

#' Extract parts of file path
#' 
#' These functions will extract the directory, file name, and file extension
#' of some common types of files. Additionally, \code{path_extract} will
#' check its results by recreating \code{path} and will give warnings if
#' the results fail to match the input.
#' 
#' \code{fname} and \code{path_extract} do the text processing; 
#' \code{file_name} and \code{file_ext} are convenience functions that only
#' return the file name or file extension, respectively.
#' 
#' @note
#' Known examples where this function fails:
#' \itemize{
#'  \item{\code{.tar.gz} }{files with compound file extensions}
#' }
#' 
#' @param path file path as character string
#' 
#' @seealso \code{\link[rawr]{regcaptures}}; \code{\link{basename}};
#' \code{\link{dirname}}
#' 
#' @examples
#' l <- list(
#'   '~/desktop/tmp.csv',               ## normal file with directory
#'   '.dotfile.txt',                    ## dotfile with extension
#'   '.vimrc',                          ## dotfile with no extension
#'   '~/file.',                         ## file name ending in .
#'   '~/DESCRIPTION',                   ## no extension
#'   '~/desktop/tmp/a.filename.tar.gz'  ## compound extension fails
#' )
#' 
#' setNames(lapply(l, path_extract), l)
#' setNames(lapply(l, fname), l)
#' setNames(lapply(l, file_name), l)
#' setNames(lapply(l, file_ext), l)
#' 
#' @export

path_extract <- function(path) {
  p <- normalizePath(path, mustWork = FALSE)
  m <- cbind(dirname = dirname(p), basename = basename(p), fname(p))
  mm <- file.path(
    m[, 'dirname'],
    paste(m[, 'filename'], m[, 'extension'],
          sep = ifelse(nzchar(m[, 'extension']), '.', ''))
  )
  
  if (gsub('\\./', '', mm) != p || !nzchar(m[, 'filename']))
    warning('Results could not be validated', domain = NA)
  
  m
}

#' @rdname path_extract
#' @export
fname <- function(path) {
  xx <- basename(path)
  pp <- '(^\\.[^ .]+$|[^:\\/]*?[.$]?)(?:\\.([^ :\\/.]*))?$'
  xx <- regcaptures2(xx, pp)[[1L]]
  colnames(xx) <- c('filename', 'extension')
  xx
}

#' @rdname path_extract
#' @export
file_name <- function(path) {
  path_extract(path)[, 'filename']
}

#' @rdname path_extract
#' @export
file_ext <- function(path) {
  path_extract(path)[, 'extension']
}

#' @rdname path_extract
#' @export
rm_ext <- function(path) {
  gsub('(^\\.[^ .]+$|[^:\\/]*?[.$]?)(?:\\.([^ :\\/.]*))?$',
       '\\1', path, perl = TRUE)
}

#' Multiple pattern matching and replacement
#' 
#' Perform multiple pattern matching and replacement.
#' 
#' @param pattern for substituting, a vector of length two for a single
#' replacement or a \emph{list} of length two vectors for multiple
#' replacements where each vector is \code{c(pattern,replacement)}; or for
#' grepping, a vector of character strings containing regular expressions
#' to be matched in \code{x}
#' @param x a character vector where matches are sought
#' @param ... additional parameters passed onto other methods
#' @param parallel logical; if \code{TRUE}, grepping will be performed in
#' \pkg{\link{parallel}}; also, if \code{pattern} is a vector greater than
#' \code{1e4} elements in length, \code{parallel} defaults to \code{TRUE}
#' @param replacement optional; if given, both \code{pattern} and
#' \code{replacement} should be character vectors of equal length
#' (\code{replacement} will be recycled if needed)
#' 
#' @seealso
#' \code{\link[base]{grep}}; \code{\link{vgrep}}
#' 
#' @examples
#' ## grepping
#' mgrep(letters[1:5], letters[1:5])
#' mgrepl(letters[1:5], letters[1:5])
#' 
#' ## subbing
#' s1 <- 'thiS iS SooD'
#' 
#' ## if replacement is given, acts like gsub
#' mgsub(c('hi', 'oo'), c('HI', '00'), s1)
#' mgsub(c('\\bS','$','i'), '_', rep(s1, 3))
#' 
#' ## pattern can also be a list of c(pattern, replacement)
#' r1 <- c('hi','HI')
#' r2 <- c(list(r1), list(c('oo', '00')))
#' r3 <- c(r2, list(c('i', '1'), c('\\b(\\w)', '\\U\\1')))
#' 
#' mgsub(r1, x = s1, ignore.case = TRUE)
#' mgsub(r2, x = s1)
#' mgsub(r3, x = s1, perl = TRUE)
#' 
#' @name mgrep
NULL

mgrep_ <- function(parallel, FUN, vlist, ...) {
  pattern <- vlist$pattern
  x <- vlist$x
  
  if (parallel) {
    ## if parallel = TRUE or long vector x (>1e4), run in parallel
    requireNamespace('parallel')
    cl <- makeCluster(nc <- getOption('cl.cores', detectCores()))
    on.exit(stopCluster(cl))
    clusterExport(cl = cl, varlist = c('x', 'pattern'), envir = environment())
    parLapply(cl, seq_along(pattern),
              function(ii) FUN(pattern = pattern[ii], x = x, ...))
  } else {
    ## slow version
    lapply(seq_along(pattern), function(ii)
      FUN(pattern = pattern[ii], x = x, ...))
  }
}

#' @rdname mgrep
#' @export
mgrepl <- function(pattern, x, ..., parallel = length(pattern) > 1e4L) {
  mgrep_(parallel = parallel, FUN = base::grepl, ...,
         vlist = list(pattern = pattern, x = x))
}

#' @rdname mgrep
#' @export
mgrep <- function(pattern, x, ..., parallel = length(pattern) > 1e4L) {
  mgrep_(parallel = parallel, FUN = base::grep, ...,
         vlist = list(pattern = pattern, x = x))
}

msub_ <- function(pattern, replacement, x, ..., FUN) {
  dots <- match.call(expand.dots = FALSE)$...
  FUN  <- match.fun(FUN)
  
  if (!missing(replacement))
    pattern <- as.list(data.frame(
      t(cbind(I(pattern), I(rep_len(replacement, length(pattern)))))))
  if (!is.list(pattern))
    pattern <- list(pattern)
  
  sub2 <- function(l, x)
    do.call(FUN, c(list(x = x, pattern = l[1L], replacement = l[2L]), dots))
  
  Reduce('sub2', pattern, x, right = TRUE)
}

#' @rdname mgrep
#' @export
msub <- function(pattern, replacement, x, ...) {
  msub_(pattern, replacement, x, ..., FUN = 'sub')
}

#' @rdname mgrep
#' @export
mgsub <- function(pattern, replacement, x, ...) {
  msub_(pattern, replacement, x, ..., FUN = 'gsub')
}

#' tree
#' 
#' List contents of directories in a tree-like format.
#' 
#' @param path file name path as character string
#' @param full.names logical; if \code{TRUE}, the full file path will be
#' returned; otherwise, only the \code{\link{basename}} is returned (default)
#' @param ndirs,nfiles maximum number of directories and files per directory
#' to print
#' 
#' @references
#' \url{https://stackoverflow.com/q/14188197/2994949}
#' 
#' @examples
#' str(tree(system.file(package = 'rawr2'), FALSE))
#' 
#' @export

tree <- function(path = '.', full.names = FALSE, ndirs = 5L, nfiles = 5L) {
  tree_ <- function(path = '.', full.names, n) {
    isdir <- file.info(path)$isdir
    n <- as.integer(n)
    
    res <- if (!isdir) {
      if (full.names)
        path else basename(path)
    } else {
      files <- list.files(path, full.names = TRUE, include.dirs = TRUE)
      isdir <- file.info(files)$isdir
      files <- files[isdir | cumsum(!isdir) <= n]
      res <- lapply(files, tree_, full.names, n)
      names(res) <- basename(files)
      res
    }
    
    res
  }
  
  path <- normalizePath(path, mustWork = TRUE)
  head(tree_(path, full.names, nfiles), ndirs)
}

#' \code{grep} for vectors
#' 
#' \code{grep} vectors for patterns given by other vectors.
#' 
#' @param pattern a vector to be matched
#' @param x vector having the same type as \code{pattern} where matches are
#' sought
#' 
#' @return
#' For \code{vgrep}, a vector of indices indicating the start of the matches
#' found in \code{x}. For \code{vgrepl}, a list of logical vetors of
#' \code{length(x)} for each match found in \code{x}.
#' 
#' @references
#' Adapted from \url{https://stackoverflow.com/q/33027611/2994949}
#' 
#' @seealso
#' \code{\link{grep}}; \code{\link{mgrep}}; \code{\link[rawr]{\%==\%}}
#' 
#' @examples
#' x <- c(0,1,1,0,1,1,NA,1,1,0,1,1,NA,1,0,0,1,
#'        0,1,1,1,NA,1,0,1,NA,1,NA,1,0,1,0,NA,1)
#' vgrep(c(1, NA, 1), x)
#' vgrepl(c(1, NA, 1), x)
#' 
#' vgrep(c(1, 0, 1, NA), x)
#' which(vgrepl(c(1, 0, 1, NA), x)[[1]])
#' 
#' @export

vgrep <- function(pattern, x) {
  vgrep_ <- function(pp, xx, acc = if (length(pp))
    seq_along(xx) else integer(0L)) {
    if (!length(pp))
      return(acc)
    Recall(pp[-1L], xx, acc[which(pp[[1L]] %==% xx[acc])] + 1L)
  }
  vgrep_(pattern, x) - length(pattern)
}

#' @rdname vgrep
#' @export
vgrepl <- function(pattern, x) {
  m  <- vgrep(pattern, x)
  lp <- length(pattern)
  pp <- rep(FALSE, length(x))
  
  if (!length(m))
    integer(0L) else lapply(m, function(y) {
      pp[y:(y + lp - 1L)] <- TRUE
      pp
    })
}

#' Justify text
#' 
#' Add whitespace to (monospaced) text for justified or block-style spacing.
#' 
#' @param string a character string
#' @param width desired width text in characters given as a positive integer
#' @param fill method of adding whitespace, i.e., by starting with the
#' \code{"right"}- or \code{"left"}-most whitespace or \code{"random"}
#' 
#' @seealso
#' \code{\link{strwrap}}
#' 
#' @references
#' Adapted from \url{https://stackoverflow.com/q/34710597/2994949}
#' 
#' @examples
#' x <- paste(rownames(mtcars), collapse = ' ')
#' cat(justify(x))
#' 
#' ## slight differences in whitespace for fill methods
#' op <- par(xpd = NA, family = 'mono', cex = 0.8)
#' plot(0, ann = FALSE, axes = FALSE, type = 'n')
#' text(1, 1, justify(x, fill = 'random'))
#' text(1, 0, justify(x, fill = 'right'), col = 2)
#' text(1, -1, justify(x, fill = 'left'), col = 3)
#' par(op)
#' 
#' @export

justify <- function(string, width = getOption('width') - 10L,
                    fill = c('random', 'right', 'left')) {
  fill <- match.arg(fill)
  string <- gsub('\n', '\n\n', string, fixed = TRUE)
  strs <- strwrap(string, width = width)
  paste(fill_spaces_(strs, width, fill), collapse = '\n')
}

fill_spaces_ <- function(lines, width, fill) {
  tokens <- strsplit(lines, '\\s+')
  res <- lapply(head(tokens, -1L), function(x) {
    nspace <- max(length(x) - 1L, 1L)
    extra <- width - sum(nchar(x)) - nspace
    reps  <- extra %/% nspace
    extra <- extra %% nspace
    times <- rep.int(if (reps > 0L) reps + 1L else 1L, nspace)
    
    if (extra > 0L) {
      if (fill == 'right')
        times[seq.int(extra)] <- times[seq.int(extra)] + 1L
      else if (fill == 'left')
        times[(nspace - extra + 1L):nspace] <-
          times[(nspace - extra + 1L):nspace] + 1L
      else times[inds] <- times[(inds <- sample(nspace, extra))] + 1L
    }
    
    spaces <- c('', unlist(lapply(times, formatC, x = ' ', digits = NULL)))
    res <- paste(c(rbind(spaces, x)), collapse = '')
    
    if (sum(c(nchar(x), length(x), extra)) < width / 2)
      gsub('\\s{1,}', ' ', res) else res
  })
  
  c(res, paste(tail(tokens, 1L)[[1L]], collapse = ' '))
}

#' Find factors
#' 
#' Find common factors of two or more integers.
#' 
#' @param ... integers
#' 
#' @examples
#' factors(21)
#' factors(3 * 2 ^ 20)
#' factors(64, 128, 58)
#' 
#' @export

factors <- function(...) {
  factors_ <- function(x) {
    x <- as.integer(x)
    y <- seq_len(abs(x))
    y[x %% y == 0L]
  }
  
  Reduce(intersect, lapply(list(...), factors_))
}

#' \code{datatable}s with sparklines
#' 
#' Create an HTML table widget using the JavaScript library DataTables
#' (\code{\link[DT]{datatable}}) with \code{\link[sparkline]{sparkline}}
#' columns.
#' 
#' @param data a data frame or matrix
#' @param spark a \emph{named} list of lists for each column of \code{data}
#' for which an interactive sparkline will replace each row cell
#' 
#' each named list of \code{spark} should have length \code{nrow(data)} and
#' contain at least one numeric value
#' @param type the type of sparkline, one or more of "line", "bar", "box1",
#' or "box2", recycled as needed; the only difference between "box1" and
#' "box2" is the use of \code{spark_range}
#' @param spark_range an optional list or vector (recycled as needed) giving
#' the overall range for each list of \code{spark}; if missing, the ranges
#' will be calculated; note this is only applicable for \code{type = "line"}
#' or \code{type = "box1"}
#' @param options,... \code{options} or additional arguments passed to
#' \code{\link[DT]{datatable}}
#' 
#' @seealso
#' Adapted from \url{leonawicz.github.io/HtmlWidgetExamples/ex_dt_sparkline.html}
#' 
#' @examples
#' \dontrun{
#' library('DT')
#' 
#' ## strings of data separated by commas should be passed to each row
#' ## this data will be used to generate the sparkline
#' dd <- aggregate(cbind(wt, mpg) ~ gear, mtcars, function(x)
#'   toString(fivenum(x)))
#' sparkDT(dd)
#' 
#' 
#' ## for each column, create a list of vectors for each row to be plotted
#' l <- sapply(c('wt', 'mpg'), function(x)
#'   split(mtcars[, x], mtcars$gear), simplify = FALSE, USE.NAMES = TRUE)
#' 
#' sparkDT(dd, l, type = 'box1')
#' 
#' 
#' set.seed(1)
#' spark <- replicate(nrow(mtcars), round(rnorm(sample(20:100, 1)), 2), FALSE)
#' 
#' sparkDT(mtcars, list(mpg = spark, wt = spark, disp = spark, qsec = spark))
#' 
#' sparkDT(mtcars, list(mpg = spark, wt = spark, disp = spark, qsec = spark),
#'         spark_range = list(disp = c(-5, 5), mpg = c(0, 10)))
#' 
#' ## note difference between box1 (boxes aligned) and box2 (max size)
#' sparkDT(mtcars[, c('mpg', 'wt')],
#'         list(mpg = spark, wt = spark),
#'         type = c('box1', 'box2'),
#'         # range = c(-2, 2),
#'         rownames = FALSE,
#'         colnames = c('box1', 'box2')
#' )
#' }
#' 
#' @export

sparkDT <- function(data, spark, type = c('line', 'bar', 'box1', 'box2'),
                    spark_range, options = list(), ...) {
  data <- as.data.frame(data)
  if (missing(spark))
    return(DT::datatable(data = data, ..., options = options))
  
  srange <- lapply(spark, function(x) range(unlist(x), na.rm = TRUE))
  
  spark_range <- if (missing(spark_range))
    srange
  else if (!is.list(spark_range))
    setNames(list(spark_range)[rep_len(1L, length(spark))], names(spark))
  else if (length(names(spark_range)))
    modifyList(srange, spark_range)
  else setNames(spark_range, names(spark))
  
  spark_range <- spark_range[names(spark)]
  
  type <- match.arg(type, several.ok = TRUE)
  type <- rep_len(type, length(spark))
  
  stopifnot(
    all(names(spark) %in% names(data))
  )
  
  spark <- rapply(spark, paste, collapse = ', ', how = 'list')
  data[, names(spark)] <- lapply(spark, unlist)
  
  render_sparkDT(data, names(spark), type, spark_range, options, ...)
}

render_sparkDT <- function(data, variables, type, range, options, ...) {
  ## catch case of rownames = FALSE - first spark col does not render
  dots <- lapply(substitute(alist(...))[-1L], eval)
  if (identical(dots$rownames, FALSE))
    dots$rownames <- rep_len('', nrow(data))
  
  targets <- match(variables, names(data))
  idx <- seq_along(targets)
  
  ## each column definition and type need a distinct class with variable name
  columnDefs <- lapply(idx, function(ii)
    list(
      targets = targets[ii],
      render = DT::JS(
        sprintf("function(data, type, full){ return '<span class=spark%s>' + data + '</span>' }",
                variables[ii])
      )
    )
  )
  
  type <- lapply(idx, function(ii) {
    bar  <- "type: 'bar' ,  barColor: 'orange', negBarColor: 'purple',      highlightColor: 'black'"
    line <- "type: 'line', lineColor: 'black',    fillColor: '#cccccc', highlightLineColor: 'orange', highlightSpotColor: 'orange'"
    box  <- "type: 'box' , lineColor: 'black', whiskerColor: 'black' ,    outlierFillColor: 'black' ,   outlierLineColor: 'black',  medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"
    
    r <- range[[ii]]
    line_range <- sprintf('%s , chartRangeMin: %s , chartRangeMax: %s',
                          line, r[1L], r[2L])
    box_range  <- sprintf('%s , chartRangeMin: %s , chartRangeMax: %s',
                          box, r[1L], r[2L])
    
    types <- list(bar = bar, line = line_range, box1 = box_range, box2 = box)
    
    types[match(type[ii], names(types))]
  })
  
  js <- sapply(idx, function(ii) sprintf(
    "$('.spark%s:not(:has(canvas))').sparkline('html', { %s }); \n",
    variables[ii], type[[ii]])
  )
  js <- sprintf(
    'function (oSettings, json) {\n %s }\n', paste(js, collapse = '\n')
  )
  
  oo <- list(columnDefs = columnDefs, fnDrawCallback = DT::JS(js))
  dt <- do.call(
    DT::datatable,
    c(list(data = data, options = modifyList(options, oo)), dots)
  )
  dt$dependencies <-
    c(dt$dependencies, htmlwidgets::getDependency('sparkline'))
  
  dt
}

#' Clear workspace
#' 
#' Clear the workspace by removing all objects in \code{\link{ls}} followed
#' by \code{\link[=gc]{garbage collection}}.
#' 
#' @param all.names logical; if \code{TRUE}, also removes hidden (dot) objects
#' 
#' @seealso
#' \code{\link{clear}}
#' 
#' @export

clc <- function(all.names = FALSE) {
  rm(list = ls(.GlobalEnv, all.names = all.names), envir = .GlobalEnv)
  gc(TRUE)
  invisible(NULL)
}

#' Clear console
#' 
#' Clear the console window.
#' 
#' @param ... ignored
#' 
#' @seealso
#' \code{\link{clc}}
#' 
#' @export

clear <- function(...) {
  cat('\014')
}

#' Re-load a package
#' 
#' Detach and re-load a package.
#' 
#' @param package package name, as a \code{\link{name}} or literal character
#' string
#' 
#' @export

reload <- function(package) {
  if (!is.character(substitute(package)))
    package <- deparse(substitute(package))
  
  # tryCatch(
  #   detach(paste0('package:', package), unload = TRUE, character.only = TRUE),
  #   error = function(e) NULL
  # )
  ok <- tryCatch(
    {unloadNamespace(package); library(package, character.only = TRUE); TRUE},
    error = function(e) {print(e); FALSE}
  )
  
  invisible(ok)
}