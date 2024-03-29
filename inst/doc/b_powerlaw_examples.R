## ----echo=FALSE, message=FALSE------------------------------------------------
library(knitr)
library(poweRlaw)
options(replace.assign = FALSE)

opts_chunk$set(fig.path = "knitr_figure_poweRlaw/graphicsn-",
               cache.path = "knitr_cache_poweRlaw_b/",
               fig.align = "center",
               dev = "pdf", fig.width = 5, fig.height = 5,
               fig.show = "hold", cache = FALSE, par = TRUE,
               out.width = "0.4\\textwidth")
knit_hooks$set(crop = hook_pdfcrop)

knit_hooks$set(par = function(before, options, envir) {
  if (before && options$fig.show != "none") {
    par(mar = c(3, 4, 2, 1), cex.lab = .95, cex.axis = .9,
        mgp = c(3, .7, 0), tcl = -.01, las = 1)
  }}, crop = hook_pdfcrop)

set.seed(1)
palette(c(rgb(170, 93, 152, maxColorValue = 255),
          rgb(103, 143, 57, maxColorValue = 255),
          rgb(196, 95, 46, maxColorValue = 255),
          rgb(79, 134, 165, maxColorValue = 255),
          rgb(205, 71, 103, maxColorValue = 255),
          rgb(203, 77, 202, maxColorValue = 255),
          rgb(115, 113, 206, maxColorValue = 255)))


## ----echo=FALSE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
#if(!file.exists("blackouts.txt"))
blackouts = c(570, 210.882, 190, 46, 17, 360, 74, 19, 460, 65, 18.351, 25,
25, 63.5, 1, 9, 50, 114.5, 350, 25, 50, 25, 242.91, 55, 164.5,
877, 43, 1140, 464, 90, 2100, 385, 95.63, 166, 71, 100, 234,
300, 258, 130, 246, 114, 120, 24.506, 36.073, 10, 160, 600, 12,
203, 50.462, 40, 59, 15, 1.646, 500, 60, 133, 62, 173, 81, 20,
112, 600, 24, 37, 238, 50, 50, 147, 32, 40.911, 30.5, 14.273,
160, 230, 92, 75, 130, 124, 120, 11, 235, 50, 94.285, 240, 870,
70, 50, 50, 18, 51, 51, 145, 557.354, 219, 191, 2.9, 163, 257.718,
1660, 1600, 1300, 80, 500, 10, 290, 375, 95, 725, 128, 148, 100,
2, 48, 18, 5.3, 32, 250, 45, 38.5, 126, 284, 70, 400, 207.2,
39.5, 363.476, 113.2, 1500, 15, 7500, 8, 56, 88, 60, 29, 75,
80, 7.5, 82.5, 272, 272, 122, 145, 660, 50, 92, 60, 173, 106.85,
25, 146, 158, 1500, 40, 100, 300, 1.8, 300, 70, 70, 29, 18.819,
875, 100, 50, 1500, 650, 58, 142, 350, 71, 312, 25, 35, 315,
500, 404, 246, 43.696, 71, 65, 29.9, 30, 20, 899, 10.3, 490,
115, 2085, 206, 400, 26.334, 598, 160, 91, 33, 53, 300, 60, 55,
60, 66.005, 11.529, 56, 4.15, 40, 320.831, 30.001, 200) * 1000

## ----dis_data-----------------------------------------------------------------
library("poweRlaw")
data("moby", package = "poweRlaw")

## ----cache=TRUE---------------------------------------------------------------
m_pl = displ$new(moby)

## ----cache=TRUE---------------------------------------------------------------
est = estimate_xmin(m_pl)

## ----cache=TRUE---------------------------------------------------------------
m_pl$setXmin(est)

## ----eval=FALSE---------------------------------------------------------------
#  estimate_xmin(m_pl, pars = seq(1.8, 2.3, 0.1))

## ----warning=FALSE, eval=FALSE------------------------------------------------
#  m_ln = dislnorm$new(moby)
#  est = estimate_xmin(m_ln)

## ----echo=FALSE---------------------------------------------------------------
if (file.exists("examples1.rds")) {
  load("examples1.rds")
} else {
  m_ln = dislnorm$new(moby)
  est = estimate_xmin(m_ln)
  est_ln = est
  est_ln$pars = signif(est_ln$pars, 3)
  m_ln$setXmin(est)
  m_pois = dispois$new(moby)
  est = estimate_xmin(m_pois)
  m_pois$setXmin(est)
  save(m_pois, m_ln, est_ln, file = "examples1.rds")
}

## ----fig.keep='none', cache=TRUE----------------------------------------------
plot(m_pl)
lines(m_pl, col = 2)
lines(m_ln, col = 3)
lines(m_pois, col = 4)

## ----echo=FALSE, cache=TRUE---------------------------------------------------
plot(m_pl, xlab = "x", ylab = "CDF",
     panel.first = grid(col = "grey80"),
     pch = 21, bg = 1)

lines(m_pl, col = 2, lwd = 2)
lines(m_ln, col = 3, lwd = 2)
lines(m_pois, col = 4, lwd = 2)

## ----par_uncertainty, echo=FALSE----------------------------------------------
data(bootstrap_moby, package = "poweRlaw")
bs = bootstrap_moby

## ----eval=FALSE, tidy=FALSE---------------------------------------------------
#  ## 5000 bootstraps using two cores
#  bs = bootstrap(m_pl, no_of_sims = 5000, threads = 2)

## ----eval=FALSE---------------------------------------------------------------
#  bootstrap(m_pl, xmins = seq(2, 20, 2))

## ----echo=FALSE, fig.width=8, fig.height=8, out.width='0.7\\textwidth'--------
plot(bs)

## -----------------------------------------------------------------------------
sd(bs$bootstraps[, 2])
sd(bs$bootstraps[, 3])

## ----fig.keep='none'----------------------------------------------------------
## trim=0.1 only displays the final 90% of iterations
plot(bs, trim = 0.1)

## ----echo=FALSE, fig.width=6, fig.height=3, cache=TRUE, out.width='0.8\\textwidth'----
par(mfrow = c(1, 2))
hist(bs$bootstraps[, 2], xlab = expression(x[min]), ylim = c(0, 1600),
     xlim = c(0, 20), main = NULL, breaks = "fd")
grid()
hist(bs$bootstraps[, 3], xlab = expression(alpha),
     ylim = c(0, 500), xlim = c(1.80, 2.05), main = NULL, breaks = "fd")
grid()

## ----fig.keep='none'----------------------------------------------------------
hist(bs$bootstraps[, 2])
hist(bs$bootstraps[, 3])

## ----eval=FALSE---------------------------------------------------------------
#  bs1 = bootstrap(m_ln)

## ----echo=FALSE---------------------------------------------------------------
data(bootstrap_p_moby, package = "poweRlaw")
bs_p = bootstrap_p_moby

## ----echo=FALSE, fig.width=6, fig.height=4, cache=TRUE, out.width='\\textwidth'----
plot(bs_p)

## ----testing_pl, eval=FALSE---------------------------------------------------
#  bs_p = bootstrap_p(m_pl)

## -----------------------------------------------------------------------------
bs_p$p

## ----fig.keep='none', cache=TRUE----------------------------------------------
plot(bs_p)

## ----comp_dist, cache=TRUE----------------------------------------------------
m_ln$setXmin(m_pl$getXmin())

## ----cache=TRUE---------------------------------------------------------------
est = estimate_pars(m_ln)
m_ln$setPars(est)

## ----cache=TRUE---------------------------------------------------------------
comp = compare_distributions(m_pl, m_ln)

## -----------------------------------------------------------------------------
xmins = seq(1, 1001, 5)

## ----est_scan, echo=FALSE-----------------------------------------------------
est_scan = 0 * xmins
for (i in seq_along(xmins)) {
  m_pl$setXmin(xmins[i])
  est_scan[i] = estimate_pars(m_pl)$pars
}

## ----echo=FALSE, cache=TRUE---------------------------------------------------
plot(xmins, est_scan, type = "s",
     panel.first = grid(),
     xlab = expression(x[min]), ylab = expression(alpha),
     ylim = c(1.6, 2.8), col = 1)
abline(h = 1.95, col = 2, lty = 2)

## ----est_scan, echo=1, eval=FALSE---------------------------------------------
#  est_scan = 0 * xmins
#  for (i in seq_along(xmins)) {
#    m_pl$setXmin(xmins[i])
#    est_scan[i] = estimate_pars(m_pl)$pars
#  }

## ----est_scan, echo=-1, eval=FALSE--------------------------------------------
#  est_scan = 0 * xmins
#  for (i in seq_along(xmins)) {
#    m_pl$setXmin(xmins[i])
#    est_scan[i] = estimate_pars(m_pl)$pars
#  }

## -----------------------------------------------------------------------------
data("swiss_prot", package = "poweRlaw")
head(swiss_prot, 3)

## -----------------------------------------------------------------------------
m_sp = displ$new(swiss_prot$Value)
est_sp = estimate_xmin(m_sp)
m_sp$setXmin(est_sp)

## ----sp_plot, echo=FALSE, cache=TRUE, fig.width=4, fig.height=4---------------
plot(m_sp, pch = 21, bg = 2, panel.first = grid(col = "grey80"),
     xlab = "Word Occurance", ylab = "CDF")
lines(m_sp, col = 3, lwd = 3)

## -----------------------------------------------------------------------------
par(mar = c(3, 3, 2, 1), mgp = c(2, 0.4, 0), tck = -.01,
    cex.axis = 0.9, las = 1)

## ----sp_plot, fig.keep="none", eval=FALSE-------------------------------------
#  plot(m_sp, pch = 21, bg = 2, panel.first = grid(col = "grey80"),
#       xlab = "Word Occurance", ylab = "CDF")
#  lines(m_sp, col = 3, lwd = 3)

## ----eval = FALSE-------------------------------------------------------------
#  blackouts = read.table("blackouts.txt")$V1

## ----cache=TRUE---------------------------------------------------------------
m_bl = conpl$new(blackouts)

## ----cache=TRUE---------------------------------------------------------------
est = estimate_xmin(m_bl)

## ----cache=TRUE---------------------------------------------------------------
m_bl$setXmin(est)

## ----fig.keep='none', cache=TRUE----------------------------------------------
plot(m_bl)
lines(m_bl, col = 2, lwd = 2)

## ----m_bl_ln, echo=FALSE, cache=TRUE------------------------------------------
m_bl_ln = conlnorm$new(blackouts)
est = estimate_xmin(m_bl_ln)
m_bl_ln$setXmin(est)

## ----echo=FALSE, fig.width=4, fig.height=4------------------------------------
plot(m_bl, pch = 21, bg = 1,
     panel.first = grid(col = "grey80"),
     xlab = "Blackouts", ylab = "CDF")
lines(m_bl, col = 2, lwd = 3)
lines(m_bl_ln, col = 3, lwd = 3)

## ----m_bl_ln, eval=FALSE------------------------------------------------------
#  m_bl_ln = conlnorm$new(blackouts)
#  est = estimate_xmin(m_bl_ln)
#  m_bl_ln$setXmin(est)

## ----eval=FALSE---------------------------------------------------------------
#  lines(m_bl_ln, col = 3, lwd = 2)

## -----------------------------------------------------------------------------
data("native_american", package = "poweRlaw")
data("us_american", package = "poweRlaw")

## -----------------------------------------------------------------------------
head(native_american, 3)

## ----echo=FALSE, fig.width=6, fig.height=4, out.width="0.8\\textwidth"--------
plot(native_american$Date, native_american$Cas,
     log = "y", pch = 21, bg = 1,
     ylim = c(1, 2000),
     cex = 0.5, panel.first = grid(col = "grey70"),
     xlab = "Date", ylab = "#Casualties")
points(us_american$Date, us_american$Cas,
       pch = 24, bg = 2, cex = 0.5)

## ----cache=TRUE---------------------------------------------------------------
m_na = displ$new(native_american$Cas)
m_us = displ$new(us_american$Cas)

## ----cache=TRUE---------------------------------------------------------------
est_na = estimate_xmin(m_na, pars = seq(1.5, 2.5, 0.01))
est_us = estimate_xmin(m_us, pars = seq(1.5, 2.5, 0.01))

## ----cache=TRUE---------------------------------------------------------------
m_na$setXmin(est_na)
m_us$setXmin(est_us)

## ----fig.keep='none', cache=TRUE, tidy=FALSE----------------------------------
plot(m_na)
lines(m_na)
## Don't create a new plot, just store the output
d = plot(m_us, draw = FALSE)
points(d$x, d$y, col = 2)
lines(m_us, col = 2)

## ----echo=FALSE---------------------------------------------------------------
plot(m_na, bg = 1, pch = 21, cex = 0.5,
     panel.first = grid(col = "grey70"),
     xlab = "#Casualties", ylab = "CDF")
lines(m_na, lwd = 2, col = 1)

d = plot(m_us, draw = FALSE)
points(d$x, d$y, bg = 2, pch = 24, cex = 0.5)
lines(m_us, lwd = 2, col = 2)

## ----clean-up, include=FALSE--------------------------------------------------
# R compiles all vignettes in the same session, which can be bad
rm(list = ls(all = TRUE))

