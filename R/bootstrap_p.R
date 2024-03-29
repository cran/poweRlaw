sample_p_helper = function(i, m, x_lower) {
  ## Total sample size
  n = get_n(m)
  ntail_prop = get_ntail(m, prop = TRUE)

  ## Proportion to sample
  n1 = sum(runif(n) > ntail_prop) # less than xmin

  # q should be of length n
  c(sample(x_lower, n1, replace = TRUE), #less than xmin
    dist_rand(m, n - n1))
}

bootstrap_p_helper = function(i, m, x_lower, xmins, pars, xmax, distance) {

  q = sample_p_helper(i, m, x_lower)
  m_cpy = m$getRefClass()$new(q)

  est = estimate_xmin(m_cpy, xmins = xmins, pars = pars, xmax = xmax, distance = distance)
  ## Remove the character now, since we will change to data frame.
  est["distance"] = NULL
  unlist(est)
}

#' @rdname estimate_xmin
#' @export
get_bootstrap_p_sims = function(m, no_of_sims, seed, threads = 1) {

  if (is.null(m$getPars())) {
    stop("Parameters need to be set. See ?estimate_xmin")
  }
  cl = parallel::makeCluster(threads)
  on.exit(parallel::stopCluster(cl))

  x = m$dat
  x_lower = x[x < m$xmin]
  ## Set cluster seed
  parallel::clusterSetRNGStream(cl, seed)
  parallel::clusterExport(cl, "get_n")
  parallel::parSapply(cl, 1:no_of_sims, sample_p_helper, m = m, x_lower = x_lower)
}

#' @rdname estimate_xmin
#' @export
bootstrap_p = function(m, xmins = NULL, pars = NULL, xmax = 1e5,
                        no_of_sims = 100, threads = 1,
                        seed = NULL, distance = "ks") {

  if (is.null(m$getPars())) {
    message("Parameters will be initially estimated via estimate_xmin")
  }

  m_cpy = m$copy()
  time = timer()
  time$start()
  gof_v = estimate_xmin(m_cpy, xmins = xmins, pars = pars,
                        xmax = xmax, distance = distance)
  time$stop()
  if (is.na(gof_v$gof) || is.infinite(gof_v$gof)) {
    stop("Unable to estimate initial xmin using estimate_xmin(), so we can't bootstrap.")
  }

  if (min(m_cpy$dat) > xmax) {
    stop("The smallest value in your data set is larger than xmax. The xmax
         parameter is the upper limit of the xmin search space.")
  }

  if (max(m_cpy$dat) > xmax) {
    message("Some of your data is larger than xmax. The xmax parameter is
            the upper bound of the xmin search space. You could try increasing
            it. If the estimated values are below xmax, it's probably OK not to
            worry about this.")
  }

  message("Expected total run time for ", no_of_sims,
          " sims, using ", threads, " threads is ",
          signif(time$get() * no_of_sims / threads, 3), " seconds.")

  if (is.null(m$getPars()) || is.null(m$getXmin())) m_cpy$setXmin(gof_v)

  x = m_cpy$dat
  x_lower = x[x < m_cpy$xmin]

  ## Start clock and parallel bootstrap
  time$start()
  cl = makeCluster(threads)
  on.exit(stopCluster(cl))

  ## Set cluster seed
  if (!is.null(seed)) parallel::clusterSetRNGStream(cl, seed)

  clusterExport(cl, c("dist_rand", "estimate_xmin"))
  nof = parSapply(cl, 1:no_of_sims,
                  bootstrap_p_helper, m_cpy,
                  x_lower, xmins, pars, xmax, distance)
  ## Stop clock and cluster
  total_time = time$get(stop = TRUE) * threads

  if (sum(is.na(nof[1, ])) > 1) {
    message(sum(is.na(nof[1, ])), " bootstraps generated NA values.
            These have been removed when calculating the associated p-value.")
  }

  bootstraps = as.data.frame(t(nof))
  l = list(p = sum(nof[1, ] >= gof_v[["gof"]], na.rm = TRUE) / no_of_sims,
           gof = gof_v[["gof"]],
           bootstraps = bootstraps,
           sim_time = total_time[[1]] / no_of_sims,
           seed = seed,
           package_version = packageVersion("poweRlaw"),
           distance = distance)
  class(l) = "bs_p_xmin"
  l
}
