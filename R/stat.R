### statistical functions
# power_cv, simon2, fakeglm, gcd, lm.beta, install.bioc, hl_est, rcor, rsum,
# winsorize, cor.n, cor.ci
# 
# unexported:
# combn_fun, rcor1, rcorn
###


#' Power calculations for coefficient of variation
#' 
#' Compute power of one- and two-sample t-test using ratio of means and
#' coefficient of variation. Alternatively, determine the other parameters
#' to obtain a target power.
#' 
#' Exactly one of \code{n}, \code{f}, \code{cv}, \code{sig.level}, and
#' \code{power} must be \code{NULL}.
#' 
#' @param n number of observations (per group)
#' @param f ratio of means (>1)
#' @param cv coefficient of variation
#' @param sig.level significance level (type-I error probability)
#' @param power power of test (1 - type-II error probability)
#' @param type type of t test, \code{"two-sample"} or \code{"one-sample"}/
#' \code{"paired"}
#' @param alternative one- or two-sided test
#' @param distribution underlying distribution assumption
#' 
#' @return
#' Object of class \code{power.htest}, a list of the arguments (including
#' the computed) augmented with method and note elements.
#' 
#' @note
#' \code{\link{uniroot}} is used to solve power equation for unknowns, so one
#' may see errors from it, notably about inability to bracket the root when
#' invalid arguments are given.
#' 
#' @references
#' Van Belle, G., Martin, D. Sample size as a function of coefficient of
#' variation and ratio of means. Am Statistician, Vol. 47, No. 3 (Aug., 1993),
#' pp. 165-7.
#' @references
#' Martin, D.C., and van Belle, G. Approximations for Power and Sample Size
#' for Student's t-Test. Technical Report 125 (1991), University of Washington,
#' Dept. of Biostatistics.
#' 
#' @examples
#' power_cv(n = NULL, 1.25, 0.2, 0.05, 0.8, distribution = 'normal')
#' power_cv(13, 1.25, 0.2, 0.05, power = NULL, distribution = 't')
#' power_cv(13, 1.25, 0.2, 0.05, power = NULL, distribution = 'log.normal')
#' 
#' @export

power_cv <- function(n = NULL, f = NULL, cv = NULL,
                     sig.level = NULL, power = NULL,
                     type = c('two.sample', 'one.sample', 'paired'),
                     alternative = c('two.sided', 'less', 'greater'),
                     distribution = c('t', 'normal', 'log.normal')) {
  if (sum(sapply(list(n, f, cv, sig.level, power), is.null)) != 1)
    stop('exactly one of n, f, cv, power, sig.level must be NULL')
  if (!is.null(sig.level) && !is.numeric(sig.level) || 
      any(0 > sig.level | sig.level > 1))
    stop('\'sig.level\' must be numeric in [0, 1]')
  if (!is.null(power) && !is.numeric(power) || any(0 > power | power > 1))
    stop('\'power\' must be numeric in [0, 1]')
  if (f < 1) {
    warning('ratio of means must be such that mu1/mu0 > 1: 1/f used')
    f <- 1 / f
  }
  
  type <- match.arg(type)
  alternative <- match.arg(alternative)
  dist <- match.arg(distribution)
  if (dist == 'log.normal' && !(sig.level %in% c(0.01, 0.05)))
    stop('cannot use desired significance level: ',
         'use 5% or 10% type-I error probability')
  
  tsample <- switch(type, one.sample = 1L, two.sample = 2L, paired = 1L)
  ttside  <- switch(alternative, less = 1L, two.sided = 2L, greater = 3L)
  tside   <- switch(alternative, less = 1L, two.sided = 2L, greater = 1L)
  
  ## assuming underlying t distribution
  # one-sided, less
  if (ttside == 1L) {
    p.body <- quote({
      df <- (n - 1) * tsample
      qt <- qt(sig.level/tside, df, lower.tail = TRUE)
      pt(-(-qt + (sqrt(n) * (f - 1)) / (cv * (f ^ 2 + 1) ^ 0.5)), df,
         lower.tail = TRUE)
    })
  }
  
  # two-sided
  if (ttside == 2L) {
    p.body <- quote({
      df <- (n - 1) * tsample
      qt <- qt(sig.level / tside, df, lower.tail = FALSE)
      pt(-qt + (sqrt(n) * (f - 1)) / (cv * (f ^ 2 + 1) ^ 0.5), df,
         lower.tail = TRUE) / 2 +
        pt(-(-qt + (sqrt(n) * (f - 1)) / (cv * (f ^ 2 + 1) ^ 0.5)), df,
           lower.tail = FALSE) / 2
    })
  }
  
  # one-sided, greater
  if (ttside == 3L) {
    p.body <- quote({
      df <- (n - 1) * tsample
      qt <- qt(sig.level / tside, df, lower.tail = FALSE)
      pt(-(-qt + (sqrt(n) * (f - 1)) / (cv * (f ^ 2 + 1) ^ 0.5)), df,
         lower.tail = FALSE)
    })
  }
  
  ## assuming underlying normal distribution
  if (dist == 'normal') {
    p.body <- quote({
      qt <- qnorm(sig.level / tside, lower.tail = FALSE)
      pnorm(-qt + (sqrt(n) * (f - 1)) / (cv * (f ^ 2 + 1) ^ 0.5),
            lower.tail = TRUE)
    })
  }
  
  ## assuming underlying lognormal distribution with unknown variance
  if (dist == 'log.normal') {
    mat <- matrix(
      c(0.005, -2.57583, -2.203837,  0.6699734, -0.0524065, -0.0059258,
        0.010, -2.32635, -1.821394,  0.5380802,  0.0181774, -0.0584748,
        0.025, -1.95996, -1.145521,  0.2370261,  0.0392020, -0.0670915,
        0.050, -1.64485, -0.8455414, 0.1745865,  0.0774911, -0.0865455),
      ncol = 6L, byrow = TRUE,
      dimnames = list(NULL, c('alpha', 'z', 'a', 'b', 'c', 'd'))
    )
    
    alpha <- which(mat[, 1L] %in% sig.level)
    
    p.body <- quote({
      delta <- ifelse(cv < 0.5, sqrt(n / 2) * log(f) / cv,
                      sqrt(n / 2) * log(f) / sqrt(log(cv ^ 2 + 1)))
      v <- 2 * n - 2
      pnorm(mat[alpha - (tside == 2L), 'z'] + delta *
              (1 + mat[alpha, 'a'] / v + mat[alpha, 'b'] / (v - 1) +
                 delta * (mat[alpha, 'c'] / v + mat[alpha, 'd'] / (v - 1))))
    })
  }
  
  ## calculate missing parameter
  if (is.null(power))
    power <- eval(p.body)
  else if (is.null(n))
    n <- ifelse(dist == 'log.normal', ceiling(uniroot(function(n)
      eval(p.body) - power, c(2, 1e+07))$root), uniroot(function(n)
        eval(p.body) - power, c(2, 1e+07))$root)
  else if (is.null(cv)) {
    cv <- if (ttside == 2L)
      uniroot(function(cv) eval(p.body) - power, f * c(1e-07, 10))$root
    else if (ttside == 1L)
      uniroot(function(cv) eval(p.body) - power, f * c(-10, 5))$root
    else if (ttside == 3L)
      uniroot(function(cv) eval(p.body) - power, f * c(-5, 10))$root
  } else if (is.null(f)) {
    f <- if (ttside == 2L)
      uniroot(function(f) eval(p.body) - power, cv * c(1e-07, 10))$root
    else if (ttside == 1L)
      uniroot(function(f) eval(p.body) - power, cv * c(-10, 5))$root
    else if (ttside == 3L)
      uniroot(function(f) eval(p.body) - power, cv * c(-5, 10))$root
  } else if (is.null(sig.level))
    sig.level <- uniroot(function(sig.level) eval(p.body) - power,
                         c(1e-10, 1 - 1e-10))$root
  else stop('internal error - solve for null value')
  
  NOTE <- switch(
    type,
    paired = paste('n is number of *pairs*, cv is coefficient of',
                   'variation of *differences* within pairs'),
    two.sample = 'n is number in *each* group',
    one.sample = NULL
  )
  METHOD <- paste(
    switch(type, one.sample = 'One-sample', two.sample = 'Two-sample',
           paired = 'Paired'),
    't test power calculation'
  )
  
  structure(
    list(n = n, f = f, cv = cv, sig.level = sig.level, power = power,
         alternative = alternative, distributon = dist, note = NOTE,
         method = METHOD),
    class = 'power.htest'
  )
}

#' Simon two-stage designs
#' 
#' Function for sample sizes for Simon optimal two-stage, single-arm designs.
#' 
#' For two-stage designs for studies with binary endpoints, searches over
#' possible two-stage sampling designs to find those that minimize the
#' expected number of subjects, subject to specified constraints.
#'    
#' @param p0,pa null and alternative hypothesis response probability
#' @param n1max maximum number of subjects entered during the first stage; 
#' ignored if <= 0
#' @param ntmax maximum total number of total subjects
#' @param alpha,beta type-I and type-II errors
#' @param del searches for designs where the expected number of subjects under
#' the null with within \code{del} of the minimum possible value
#' @param minimax logical; if \code{TRUE}, only searches for designs which will
#' minimize the maximum sample size
#' 
#' @return
#' A list with the following elements:
#' 
#' \item{\code{$designs}}{a matrix with a row giving a summary of each
#' design which meets the criteria. The columns are: \code{n1}, the number
#' of subjects entered in the first stage; \code{r1}, the cutoff for stopping
#' at the first stage (continue if the number of responses is > \code{r1});
#' \code{n2}, the additional number of subjects enrolled in the second stage;
#' \code{r2}, the cutoff for inactivity after the second stage (reject the
#' null if the number of responses is > \code{r2}); \code{Pstop1.H0}, the
#' probability of stopping after the first stage under H0 (\code{p0});
#' \code{size}, the actual type-I error; \code{type2}, the actual type-II
#' error; \code{E.tot.n.H0}, the expected number of subjects under H0;
#' \code{Pstop1.H1}, the probability of stopping after the first stage under
#' H1; and \code{E.tot.n.H1}, the expected number of subjects under H1.}
#' \item{\code{$call}}{the call to \code{simon2}}
#' \item{\code{$description}}{a text string giving a brief description of
#' the columns in \code{$designs}}
#' 
#' @seealso
#' \pkg{desmon}: \code{simon}, \code{twostg}, \code{bin1samp}, \code{pickwin},
#' \code{rp21}
#' 
#' @author
#' Robert Gray (\code{desmon::simon}); Robert Redd (\code{simon2})
#' 
#' @references
#' Simon R (1989). Optimal two-stage designs for phase II clinical trials.
#' \emph{Controlled Clinical Trials}, 10:1-10.
#' 
#' @examples
#' simon2(0.2, c(0.4, 0.5))
#' simon2(p0 = seq(0.55, 0.6, by = 0.01), pa = 0.75, ntmax = 60)
#' 
#' ## compare this function to results from desmon::simon
#' simon2(0.4, 0.6)
#' ## from desmon package
#' rawr:::simon(0.4, 0.6)
#' 
#' @export

simon2 <- function(p0, pa, n1max = 0, ntmax = 1e+05, alpha = 0.1, beta = 0.1,
                   del = 1, minimax = FALSE) {
  args <- expand.grid(p0 = p0, pa = pa)
  
  sim <- Map(
    desmon::simon,
    p0 = args[['p0']],
    pa = args[['pa']],
    n1max = n1max,
    alpha = alpha,
    beta = beta,
    del = del,
    minimax = minimax
  )
  sim <- lapply(seq_along(sim), function(ii) {
    x <- sim[[ii]][[1L]]
    a <- apply(x, 1L, function(a)
      desmon::twostg(a['n1'], a['n2'], args$pa[ii], a['r1'], a['r2'])$prob)
    cbind(
      x,
      Pstop1.H1 = a[2L, ],
      E.tot.n.H1 = x[, 'n1'] + x[, 'n2'] * (1 - a[2L, ])
    )
  })
  sim <- setNames(sim, sapply(seq_len(nrow(args)), function(x)
    catlist(args[x, ])))
  
  list(
    designs = sim,
    call = match.call(),
    description = c('n1, n2 = cases 1st stage and additional # in 2nd',
                    paste('r1, r2 = max # responses 1st stage and total to',
                          'declare trt inactive'))
  )
}

#' Fake GLM
#' 
#' Creates a "fake" \code{\link{glm}} object using specific coefficients
#' which can be used with \code{predict.glm} without fitting a model first.
#' 
#' Using \code{data} a \code{glm} object will be created with a series of the
#' desired coefficients passed in \dots in the order that they would appear
#' in \code{names(coef(glm(...)))}. An unnamed value will be treated as
#' the intercept and all other coefficients must be named in the order given
#' in \code{formula}, eg, \code{1, x1 = 1, x2 = 3}, etc.
#' 
#' Also note that factor variables must already be factors in \code{data},
#' that is, it is not possible to use \code{y ~ factor(x)} currently in
#' \code{fakeglm}.
#' 
#' @param formula a \code{\link{formula}}
#' @param ... coefficients for new model; see details
#' @param family a description of the error distribution and link to be used
#' in the model; this can be a character string naming a family function or
#' the result of a clall to a family function; see \code{\link{family}}
#' @param data a data frame
#' 
#' @references
#' \url{https://gist.github.com/MrFlick/ae299d8f3760f02de6bf}
#' 
#' @examples
#' f1 <- glm(vs ~ mpg + wt + disp, data = mtcars, family = 'binomial')
#' p1 <- predict(f1, type = 'response')
#' 
#' f2 <- fakeglm(vs ~ mpg + wt + disp, data = mtcars, family = 'binomial',
#'               -21.9020, mpg = 0.6470, wt = 5.3315, disp = -0.0403)
#' p2 <- predict(f2, newdata = mtcars, type = 'response')
#' 
#' all.equal(p1, p2, tolerance = .0001) ## TRUE
#' 
#' dat <- within(mtcars, gear <- factor(gear))
#' fakeglm(vs ~ mpg + gear, data = dat, family= 'binomial', 0, mpg = 1,
#'         gear4 = 3, gear5 = 1)
#'
#' @export

fakeglm <- function(formula, ..., family, data = NULL) {
  dots <- list(...)
  res  <- list()
  
  ## stats:::.MFclass
  .MFclass <- function (x) {
    if (is.logical(x))
      'logical'
    else if (is.ordered(x))
      'ordered'
    else if (is.factor(x))
      'factor'
    else if (is.character(x))
      'character'
    else if (is.matrix(x) && is.numeric(x))
      paste('nmatrix', ncol(x), sep = '.')
    else if (is.numeric(x))
      'numeric'
    else 'other'
  }
  
  tt <- terms(formula, data = data)
  if (!is.null(data)) {
    mf <- model.frame(tt, data)
    vn <- sapply(attr(tt, 'variables')[-1L], deparse)
    if ((yvar <- attr(tt, 'response')) > 0)
      vn <- vn[-yvar]
    xlvl <- lapply(data[vn], function(x)
      if (is.factor(x))
        levels(x)
      else if (is.character(x))
        levels(as.factor(x))
      else NULL)
    attr(res, 'xlevels') <- xlvl[!vapply(xlvl, is.null, NA)]
    attr(tt, 'dataClasses') <- sapply(data[vn], .MFclass)
  }
  res$terms <- tt
  coef <- numeric(0L)
  stopifnot(length(dots) > 1L, !is.null(names(dots)))
  
  for (ii in seq_along(dots)) {
    if ((n <- names(dots)[ii]) != '') {
      v <- dots[[ii]]
      if (!is.null(names(v))) {
        coef[paste0(n, names(v))] <- v
      } else {
        stopifnot(length(v) == 1L)
        coef[n] <- v
      }
    } else {
      coef['(Intercept)'] <- dots[[ii]]
    }
  }
  
  res$coefficients <- coef
  res$rank <- length(coef)
  if (!missing(family)) {
    res$family <- if (inherits(family, 'family'))
      family
    else if (inherits(family, 'function'))
      family()
    else if (is.character(family))
      get(family)()
    else stop('Invalid family class: ', class(family))
    
    res$qr <- list(pivot = seq_len(res$rank))
    res$deviance <- 1
    res$null.deviance <- 1
    res$aic <- 1
    class(res) <- c('glm', 'lm')
  } else {
    class(res) <- 'lm'
    res$fitted.values <- predict(res, newdata = data)
    res$residuals <- res$mf[attr(tt, 'response')] - res$fitted.values
    res$df.residual <- nrow(data) - res$rank
    res$model <- data
    ## qr doesn't work
  }
  
  res
}

#' GCD
#' 
#' Find greatest common divisor of two integers.
#' 
#' @param x,y integers
#' 
#' @examples
#' gcd(99, 2048)
#' gcd(2 ^ (1:12), 2048)
#' 
#' @export

gcd <- function(x, y) {
  ifelse(r <- x %% y, Recall(y, r), y)
}

#' Standardize regression coefficients
#' 
#' Computes standardized regression coefficients (beta) for linear models.
#' 
#' The optional \code{weights} argument can be used to scale the standard
#' deviation(s) of the coefficient(s). The default is \code{weights = 1}, but
#' \code{weights = 2} has also been suggested so that the generic comparison
#' is with inputs equal to the mean +/- 1 standard deviation [Gelman] (see
#' references). Additionally, \code{weights} can be a vector of weights for
#' each coefficient.
#' 
#' @param x an \code{\link{lm}} object
#' @param weights a vector of weights; see details
#' 
#' @seealso
#' \code{QuantPsyc::lm.beta}
#' 
#' @references
#' \url{http://onlinelibrary.wiley.com/doi/10.1002/sim.3107/abstract}
#' 
#' @examples
#' cc <- with(mtcars, cor(mpg, wt))
#' lm.beta(lm(mpg ~ wt, data = mtcars))
#' 
#' cc ^ 2
#' summary(lm(mpg ~ wt, data = mtcars))$r.squared
#' 
#' lm.beta(lm(mpg ~ wt + disp + vs, data = mtcars), weights = 2)
#' 
#' @export

lm.beta <- function (x, weights = 1) {
  stopifnot(inherits(x, 'lm'))
  
  b  <- coef(x)[-1L]
  mf <- x$model
  
  sx <- vapply(mf[, -1L, drop = FALSE], sd, double(1L))
  sy <- vapply(mf[,  1L, drop = FALSE], sd, double(1L))
  
  b * sx / sy * weights
}

#' Install bioconductor
#' 
#' Installs bioconductor base, packages, or upgrades.
#' 
#' @param pkgs character vector of package names to install
#' @param upgrade logical; if \code{TRUE} updates installed packages
#' 
#' @examples
#' \dontrun{
#' install.bioc()
#' install.bioc('Biostrings')
#' }
#' 
#' @export

install.bioc <- function(pkgs, upgrade = FALSE) {
  source(system.file('scripts', 'biocLite.R', package = 'rawr'))
  
  f <- function(...)
    biocLite(..., suppressAutoUpdate = TRUE, suppressUpdates = TRUE)
  if (upgrade) {
    biocLite(suppressAutoUpdate = TRUE, suppressUpdates = FALSE)
    return(invisible(NULL))
  }
  
  if (missing(pkgs))
    f() else f(pkgs)
  
  invisible(NULL)
}

#' Hodges-Lehmann estimator
#' 
#' A function to calculate a nonparametric estimation of a population's
#' location parameter for one sample described by Hodges and Lehmann (1963).
#' 
#' For a numeric vector \code{x} with length \code{n}, the median of the
#' means of \code{n(n+1)/2} pairs (i.e., including the vector itself)
#' are calculated.
#' 
#' For symmetric distributions, the Hodges-Lehmann statistic estimates the
#' population's median and has greater efficiency than the sample median.
#' For the normal distribution, the Hodges-Lehmann statistic is nearly as
#' efficient as the sample mean and better when estimating mixtures of
#' normal distributions.
#' 
#' For non-symmetric distributions, the Hodges-Lehmann statistic estimates
#' the population's "pseudo-median" (see \code{\link[stats]{wilcox.test}}), a
#' location parameter that is closely related to the median. The pseudo-median
#' is well defined for all distributions of random variables having dimension
#' two or greater, and like the median, the pseudo-median is defined for even
#' heavy-tailed distributions that lack any finite mean.
#' 
#' @param x a numeric or logical vector
#' @param na.rm logical; if \code{TRUE}, \code{NA} values are removed
#' 
#' @references
#' Hettmansperger, T. P.; McKean, J. W. (1998). \emph{Robust nonparametric
#' statistical methods}. Kendall's Library of Statistics. \strong{5}.
#' 
#' Hodges, J. L.; Lehmann, E. L. (1963). "Estimation of location based on
#' ranks." \emph{Annals of Mathematical Statistics}. \strong{34} (2): 598-611.
#' 
#' @seealso
#' \code{\link{wilcox.test}}; \code{ICSNP::hl.loc}
#' 
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' hl <- hl_est(x)
#' 
#' ## compare
#' hl2 <- wilcox.test(x, exact = TRUE, conf.int = TRUE)
#' identical(hl, unname(hl2$estimate))
#' 
#' @export

hl_est <- function(x, na.rm = FALSE) {
  stopifnot(is.numeric(x) | is.logical(x))
  if (na.rm)
    x <- x[!is.na(x)]
  median(c(combn_fun(x, mean.default, 2L), x))
}

combn_fun <- function(x, FUN, n = 2L, ...) {
  FUN <- match.fun(FUN)
  n <- as.integer(n)
  x <- combn(x, n)
  apply(x, 2L, FUN, ...)
}

#' Correlate variables
#' 
#' Generate a (random) vector having a desired correlation with one or more
#' variables.
#' 
#' @param y a numeric vector or matrix used to set correlation(s)
#' @param x a vector to be correlated with \code{y}; if not given, a random,
#' normally distributed vector is used
#' @param rho the desired correlation(s); should be equal in length to the
#' number of columns of \code{y}, recycled as needed (or length 1 if \code{y}
#' is a vector)
#' 
#' @seealso
#' Adapted from \url{https://stats.stackexchange.com/q/15011/66247}
#' 
#' @examples
#' set.seed(1)
#' y <- matrix(rnorm(150), 50)
#' x <- rnorm(50)
#' 
#' cor(y[, 1], rcor(y[, 1]))
#' cor(y[, 1], rcor(y[, 1], x, rho = 0.8))
#' 
#' cor(y, rcor(y))
#' cor(y, rcor(y, rho = c(.75, -.5, .25)))
#' 
#' pairs(
#'   cbind(rcor(y, x, rho = c(.75, -.5, .25)), y),
#'   upper.panel = function(x, y, ...) {
#'     points(x, y)
#'     abline(lm(y ~ x), col = 2)
#'     mtext(parse(text = sprintf('rho==%.2f', cor(x, y))), cex = 2, col = 2)
#'   }
#' )
#' 
#' @export

rcor <- function(y, x = NULL, rho = 0.5) {
  x <- if (is.null(x))
    rnorm(NROW(y)) else rep_len(x, NROW(y))
  
  if (is.null(dim(y)))
    rcor1(y, x, rho[1L])
  else rcorn(y, x, rep_len(rho, ncol(y)))
}

rcor1 <- function(y, x, rho) {
  re <- residuals(lm(x ~ y))
  rho * sd(re) * y + re * sd(y) * sqrt(1 - rho ^ 2)
}

rcorn <- function(y, x, rho) {
  y  <- as.matrix(y)
  sy <- scale(y)
  nr <- nrow(y)
  
  re  <- residuals(lm(x ~ y))
  rho <- rep_len(rho, ncol(y))
  
  ## get the coefficient sigma of re so that the cor of y with the
  ## linear combination yd %*% rho + sigma * re is the desired vector
  yd <- with(svd(sy), (nr - 1L) * u %*% diag(ifelse(d > 0, 1 / d, 0)) %*% t(v))
  sigma2 <- c((1 - rho %*% cov(yd) %*% rho) / var(re))
  
  ## linear combination
  if (sigma2 >= 0)
    c(yd %*% rho + sqrt(sigma2) * re)
  else {
    warning('joint correlations not possible', call. = FALSE)
    NULL
  }
}

#' Random sum
#' 
#' Returns a vector of length \code{n} from \code{a} to \code{b} which sum to
#' \code{k}.
#' 
#' To find the \code{n} integers in \code{[a, b]} which sum to \code{k}, the
#' range \code{1:k} is broken into \code{n} pieces requiring \code{n - 1}
#' cutpoints, and the distance between cutpoints is summed and shifted by
#' \code{a}.
#' 
#' Note that the solution may not contain a unique set of integers unless
#' \code{unique = TRUE}. Additionally, this algorithm is not guaranteed to
#' find a solution even if one exists regardless of many \code{iterations}.
#' Finally, depending on the input parameters, the algorithm may complete
#' after one or require thousands of iterations.
#' 
#' @param a,b,n,k \code{n} integers in the range \code{[a,b]} which sum to
#' \code{k}
#' @param unique logical; if \code{TRUE}, the returned vector of integers
#' will be unique if possible
#' @param iterations maximum number of iterations to repeat the algorithm
#' until a solution is found
#' 
#' @seealso
#' Adapted from \url{https://stackoverflow.com/a/49016614/2994949};
#' \code{Surrogate::RandVec}
#' 
#' @examples
#' set.seed(1)
#' rsum(1, 100, 5, 98)
#' rsum(1, 100, 5, 120, TRUE)
#' sum(rsum(1, 100, 10, 98))
#' 
#' @export

rsum <- function(a, b, n, k, unique = FALSE, iterations = 100L) {
  a <- as.integer(a)
  b <- as.integer(b)
  n <- as.integer(n)
  k <- as.integer(k)
  
  for (ii in seq.int(iterations + 1L)) {
    if (ii > iterations) {
      message(
        sprintf('%s iterations performed with no solution:\n  ', iterations),
        'Try increasing the number of iterations; otherwise,\n  ',
        'n or k may be too large, b - a is too small, or no solution exists'
      )
      return(invisible(NULL))
    }
    
    r <- k - n * a
    p <- n - 1L
    x <- sample(seq.int(r), p, TRUE)
    x <- diff(c(0, sort(x), r))
    
    if (x[1L] < a || x[p] > b)
      next
    
    if (max(x) <= b - a) {
      res <- structure(sort(a + x), iterations = ii)
      break
    }
  }
  
  if (!unique || length(unique(res)) == n)
    res else Recall(a, b, n, k, TRUE, iterations)
}

#' Winsorization
#' 
#' Winsorize extreme values in a numeric vector.
#' 
#' @param x a numeric vector
#' @param probs numeric vector of probability value(s) in \code{[0,1]}
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#' algorithms; see \code{\link{quantile}}
#' 
#' @examples
#' x <- c(92, 19, 101, 58, 1053, 91, 26, 78, 10, 13,
#'        -40, 101, 86, 85, 15, 89, 89, 28, -5, 41)
#' summary(x)
#' 
#' y <- winsorize(x, 0.1, type = 1L)
#' summary(y)
#' 
#' @export

winsorize <- function(x, probs = 1e-3, type = 7L) {
  probs <- if (length(probs) == 1L)
    c(probs, 1 - probs) else sort(probs)[1:2]
  
  stopifnot(
    is.numeric(x),
    type %in% 1:9,
    probs[1L] > 0, probs[2L] < 1
  )
  
  qn <- sort(quantile(x, probs, na.rm = TRUE, type = type))
  nx <- !is.na(x)
  
  x[nx & x < qn[1L]] <- qn[1L]
  x[nx & x > qn[2L]] <- qn[2L]
  
  x
}
