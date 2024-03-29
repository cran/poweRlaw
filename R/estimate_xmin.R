get_xmin_est = function(dat, xmins) {
  row = which.min(dat[, 1])
  ## Check for numerical instabilities
  ## Can happen in the tails of the LN
  if (!length(row)) {
    row = 1L
    dat[row, ] = NA_real_
    xmins[row] = NA_real_
    warning("Unable to estimate xmin. This may be due to numerical instabilities.
            For example the parameter estimates are in the distribution tails.")
  }

  xmin = xmins[row]
  if (is.null(xmin)) xmin = NA
  pars = dat[row, 2:(ncol(dat) - 1L)]
  ntail = dat[row, ncol(dat)]

  l = list(gof = dat[row, 1], xmin = xmin, pars = pars, ntail = ntail)
  class(l) = "estimate_xmin"
  l
}

get_gof = function(fit_cdf, data_cdf, distance) {
  if (!(distance %in% c("ks", "reweight")) || length(distance) > 1)
    stop("Unknown distance measure. The distance parameter should be either ks or reweight")

  if (distance == "ks")
    gof = max(abs(data_cdf - fit_cdf))
  if (distance == "reweight")
    gof = max(abs(data_cdf - fit_cdf) / sqrt(fit_cdf * (1 - fit_cdf)))
  gof
}

#' @rdname get_KS_statistic-deprecated
#' @title Deprecated function
#' @description This function is now deprecated and may be removed in future
#' versions.
#' @inheritParams get_distance_statistic
#' @seealso get_distance_statistic
#' @export
get_KS_statistic = function(m, xmax = 1e5, distance = "ks") {
  .Deprecated("get_distance_statistic")
  get_distance_statistic(m, xmax, distance)
}

#' @rdname estimate_xmin
#' @export
get_distance_statistic = function(m, xmax = 1e5, distance = "ks") {
  if (is(m, "discrete_distribution")) {
    data_cdf = dist_data_all_cdf(m, xmax = xmax)
    fit_cdf = dist_all_cdf(m, xmax = xmax)
  } else {
    q = m$dat
    n = m$internal[["n"]]
    N = length(q)
    q = q[(N - n + 1):N]
    q = q[q <= xmax]
    fit_cdf = dist_cdf(m, q)
    data_cdf = ((0:(n - 1)) / n)[seq_along(fit_cdf)]
  }
  get_gof(fit_cdf, data_cdf, distance)
}

#' Estimating the lower bound (xmin)
#'
#' When fitting heavy tailed distributions, sometimes it
#' is necessary to estimate the lower threshold, xmin. The
#' lower bound is estimated by minimising the
#' Kolmogorov-Smirnoff statistic
#' (as described in Clauset, Shalizi, Newman (2009)).
#' \describe{
#' \item{`get_KS_statistic`}{Calculates the KS statistic for a particular value of xmin.}
#' \item{`estimate_xmin`}{Estimates the optimal lower cutoff using a
#' goodness-of-fit based approach. This function may issue `warnings`
#' when fitting lognormal, Poisson or Exponential distributions. The
#' warnings occur for large values of `xmin`. Essentially, we are discarding
#' the bulk of the distribution and cannot calculate the tails to enough
#' accuracy.}
#' \item{`bootstrap`}{Estimates the unncertainty in the xmin and parameter values
#' via bootstrapping.}
#' \item{`bootstrap_p`}{Performs a bootstrapping hypothesis test to determine
#' whether a suggested
#' (typically power law) distribution is plausible. This is only available for distributions that
#' have `dist_rand` methods available.}}
#' @param m A reference class object that contains the data.
#' @param distance A string containing the distance measure (or measures) to calculate.
#' Possible values are `ks` or `reweight`. See details for further information.
#' @param pars default `NULL`. A vector or matrix (number of columns equal
#' to the number of parameters) of parameters used to #' optimise over.
#' Otherwise, for each value of `xmin`, the mle will be used, i.e. `estimate_pars(m)`.
#' For small samples, the mle may be biased.
#' @param xmins default `1e5`. A vector of possible values
#' of xmin to explore. When a single value is passed, this represents
#' the maximum value to search, i.e. by default we search from
#' (1, 1e5). See details for further information.
#' @param xmax default `1e5`. The maximum x value calculated when working out the CDF.
#' See details for further
#' information.
#' @param threads number of concurrent threads used during the bootstrap.
#' @param no_of_sims number of bootstrap simulations. When `no_of_sims` is large, this can
#' take a while to run.
#' @details When estimating `xmin` for discrete distributions, the search space when
#' comparing the data-cdf (empirical cdf)
#' and the distribution_cdf runs from xmin to `max(x)`
#' where `x` is the data set. This **can** often be
#' computationally brutal. In particular, when bootstrapping
#' we generate random numbers from the power law distribution,
#' which has a long tail.
#'
#' To speed up computations for discrete distributions it is sensible to put an
#' upper bound, i.e. `xmax` and/or explicitly give values of where to search, i.e. `xmin`.
#'
#' Occassionally bootstrapping can generate strange situations. For example,
#' all values in the simulated data set are less then `xmin`. In this case,
#' the estimated distance measure will be `Inf` and the parameter values, `NA`.
#'
#' There are other possible distance measures that can be calculated. The default is the
#' Kolomogorov Smirnoff statistic (`KS`). This is equation 3.9 in the CSN paper. The
#' other measure currently available is `reweight`, which is equation 3.11.
#' @importFrom parallel makeCluster parSapply
#' @importFrom parallel clusterExport stopCluster
#' @note Adapted from Laurent Dubroca's code
#' @export
#' @examples
#' ###################################################
#' # Load the data set and create distribution object#
#' ###################################################
#' x = 1:10
#' m = displ$new(x)
#'
#' ###################################################
#' # Estimate xmin and pars                          #
#' ###################################################
#' est = estimate_xmin(m)
#' m$setXmin(est)
#'
#' ###################################################
#' # Bootstrap examples                              #
#' ###################################################
#' \dontrun{
#' bootstrap(m, no_of_sims=1, threads=1)
#' bootstrap_p(m, no_of_sims=1, threads=1)
#' }
estimate_xmin = function(m, xmins = NULL, pars = NULL,
                         xmax = 1e5, distance = "ks") {
  ## Flag. Go through a bunch of checks to test whether we
  ## can estimate xmin
  estimate = !is.null(m$getDat())
  if (length(unique(m$dat)) <= m$no_pars + 1) {
    estimate = FALSE
  }

  if (estimate && is.null(xmins))  {
    xmins = unique(m$dat)
  }
  ## Make thread safe
  if (estimate) {
    m_cpy = m$getRefClass()$new(m$dat)
    m_cpy$pars = pars

    if (any(xmins > xmax)) {
      msg = paste0("xmin search space truncated at ", xmax, "
        You have three options
                     1. Increase xmax in estimate_xmins
                     2. Specify xmins explicitly
                     3. Ignore and hope for the best (which may be OK)")
      message(msg)
    }
    xmins = xmins[xmins <= xmax]
  }

  ## Need to have at least no_pars + 1 data points
  ## to estimate parameters.
  ## Find (largest - no_pars)th data point and subset xmins
  if (estimate) {
    unique_dat = unique(m_cpy$dat)
    q_len = length(unique_dat)
    max_data_pt_needed = sort(unique_dat,
                              partial = q_len - m_cpy$no_pars - 1)[q_len - m_cpy$no_pars - 1]
    xmins = xmins[xmins <= max_data_pt_needed]

    ## Initialise xmin scan
    nr = length(xmins)
  }

  ## Bootstrapping may generate strange data
  ## Columns: gof, Pars, xmin, ntail
  if (!estimate || nr < 1) {
    ## Insufficient data to estimate parameters
    dat = matrix(0, nrow = 1, ncol = (2 + m$no_pars))
    if (is.null(m$getDat())) min_xmin = NA
    else min_xmin = min(m$getDat())

    dat[1, ] = c(rep(Inf, length(distance)),
                 min_xmin,
                 rep(NA, m$no_pars))
    estimate = FALSE
  } else {
    dat = matrix(0, nrow = nr, ncol = (2 + m_cpy$no_pars))
    est = estimate_pars(m_cpy)$pars
  }

  xm = 0L
  while (estimate && xm < nr)   {
    m_cpy$xmin = xmins[xm + 1L]
    if (is.null(pars)) m_cpy$mle(initialise = est)
    else m_cpy$pars = pars

    if (!is.null(pars)) {
      L = dist_ll(m_cpy)
      I = which.max(L)
      if (is.matrix(pars)) { # For multi-parameter models
        m_cpy$pars = m_cpy$pars[I, ]
      } else {
        m_cpy$pars = m_cpy$pars[I]
      }
    }
    gof = get_distance_statistic(m_cpy, xmax, distance)
    dat[xm <- xm + 1L, ] = c(gof, m_cpy$pars, get_ntail(m_cpy))
  }

  l = get_xmin_est(dat, xmins)
  l$distance = distance
  l
}
