## ----echo=FALSE---------------------------------
library(knitr)
library(poweRlaw)
options(replace.assign=FALSE,width=50)

opts_chunk$set(fig.path='knitr_figure/graphics-', 
               cache.path='knitr_cache/graphics-', 
               fig.align='center', 
               dev='pdf', fig.width=5, fig.height=5, 
               fig.show='hold', cache=FALSE, par=TRUE)
knit_hooks$set(crop=hook_pdfcrop)

knit_hooks$set(par=function(before, options, envir){
    if (before && options$fig.show!='none') {
        par(mar=c(3.5,4,2,1),cex.lab=.95,cex.axis=.9,
      mgp=c(2.5,.7,0),tcl=-.01, las=1)
}}, crop=hook_pdfcrop)

set.seed(1)
palette(c(rgb(170,93,152, maxColorValue=255),
              rgb(103,143,57, maxColorValue=255),
              rgb(196,95,46, maxColorValue=255),
              rgb(79,134,165, maxColorValue=255),
              rgb(205,71,103, maxColorValue=255),
              rgb(203,77,202, maxColorValue=255),
              rgb(115,113,206, maxColorValue=255)))


## ----echo=FALSE, results='hide', message=FALSE, warning=FALSE, error=FALSE----
if(!file.exists("blackouts.txt"))
  download.file("http://goo.gl/BsqnP", destfile="blackouts.txt")

## -----------------------------------------------
library("poweRlaw")
data("moby")

## ----cache=TRUE---------------------------------
m_pl = displ$new(moby)

## ----cache=TRUE---------------------------------
est = estimate_xmin(m_pl)
m_pl$setXmin(est)

## ----cache=TRUE,results='hide'------------------
estimate_xmin(m_pl, pars=seq(1.5, 2.5, 0.1))

## ----cache=TRUE,warning=FALSE-------------------
m_ln = dislnorm$new(moby)
est = estimate_xmin(m_ln)

## ----echo=FALSE, cache=TRUE---------------------
est_ln = est
est_ln$pars = signif(est_ln$pars, 3)
m_ln$setXmin(est)
m_pois = dispois$new(moby)
est = estimate_xmin(m_pois)
m_pois$setXmin(est)

## ----fig.keep='none', cache=TRUE----------------
plot(m_pl)
lines(m_pl, col=2)
lines(m_ln, col=3)
lines(m_pois, col=4)

## ----echo=FALSE, cache=TRUE---------------------
plot(m_pl, xlab="x", ylab="CDF", 
     panel.first=grid(col="grey80"), 
     pch=21, bg=1)

lines(m_pl, col=2, lwd=2)
lines(m_ln, col=3, lwd=2)
lines(m_pois, col=4, lwd=2)

## ----echo=FALSE---------------------------------
data(bootstrap_moby)
bs = bootstrap_moby

## ----eval=FALSE, tidy=FALSE---------------------
#  ## 5000 bootstraps using two cores
#  bs = bootstrap(m_pl, no_of_sims=5000, threads=2)

## ----eval=FALSE---------------------------------
#  bootstrap(m_pl, xmins = seq(2, 20, 2))

## ----echo=FALSE, fig.width=12, fig.height=9-----
plot(bs)

## -----------------------------------------------
sd(bs$bootstraps[,2])
sd(bs$bootstraps[,3])

## ----fig.keep='none'----------------------------
## trim=0.1 only displays the final 90% of iterations 
plot(bs, trim=0.1)

## ----echo=FALSE, fig.width=4, fig.height=8, cache=TRUE----
par(mfrow=c(2, 1))
hist(bs$bootstraps[,2], xlab=expression(x[min]), ylim=c(0, 1600), 
     xlim=c(0, 20), main=NULL, breaks="fd")
grid()
hist(bs$bootstraps[,3], xlab=expression(alpha), 
     ylim=c(0, 500), xlim=c(1.80, 2.05), main=NULL, breaks="fd")
grid()


## ----fig.keep='none'----------------------------
hist(bs$bootstraps[,2])
hist(bs$bootstraps[,3]) 

## ----eval=FALSE---------------------------------
#  bs1 = bootstrap(m_ln)

## ----echo=FALSE---------------------------------
data(bootstrap_p_moby)
bs_p = bootstrap_p_moby

## ----echo=FALSE, fig.width=12, fig.height=9, cache=TRUE----
plot(bs_p)

## ----eval=FALSE---------------------------------
#  bs_p = bootstrap_p(m_pl)

## -----------------------------------------------
bs_p$p

## ----fig.keep='none', cache=TRUE----------------
plot(bs_p)

## ----cache=TRUE---------------------------------
m_ln$setXmin(m_pl$getXmin())

## ----cache=TRUE---------------------------------
est = estimate_pars(m_ln)
m_ln$setPars(est)

## ----cache=TRUE---------------------------------
comp = compare_distributions(m_pl, m_ln)

## -----------------------------------------------
xmins = 1:1500

## ----est_scan, echo=FALSE-----------------------
est_scan = 0*xmins
for(i in seq_along(xmins)){
  m_pl$setXmin(xmins[i])
  est_scan[i] = estimate_pars(m_pl)$pars
}

## ----echo=FALSE---------------------------------
plot(xmins, est_scan, type="l", 
     panel.first=grid(), 
     xlab=expression(x[min]), ylab=expression(alpha), 
     ylim=c(1.6, 2.8), col=1)
abline(h=1.95, col=2, lty=2)

## ----est_scan, echo=1, eval=FALSE---------------
#  est_scan = 0*xmins
#  for(i in seq_along(xmins)){
#    m_pl$setXmin(xmins[i])
#    est_scan[i] = estimate_pars(m_pl)$pars
#  }

## ----est_scan, echo=2:4-------------------------
est_scan = 0*xmins
for(i in seq_along(xmins)){
  m_pl$setXmin(xmins[i])
  est_scan[i] = estimate_pars(m_pl)$pars
}

## -----------------------------------------------
blackouts = read.table("blackouts.txt")

## ----cache=TRUE---------------------------------
m_bl = conpl$new(blackouts$V1)

## ----cache=TRUE---------------------------------
est = estimate_xmin(m_bl)

## ----cache=TRUE---------------------------------
m_bl$setXmin(est)

## ----fig.keep='none', cache=TRUE----------------
plot(m_bl)
lines(m_bl, col=2, lwd=2)

## ----m_bl_ln, echo=FALSE, cache=TRUE------------
m_bl_ln = conlnorm$new(blackouts$V1)
est = estimate_xmin(m_bl_ln)
m_bl_ln$setXmin(est)

## ----echo=FALSE, fig.width=4, fig.height=4------
plot(m_bl, pch=21, bg=1, 
     panel.first=grid(col="grey80"), 
     xlab="Blackouts", ylab="CDF")
lines(m_bl, col=2, lwd=3)
lines(m_bl_ln, col=3, lwd=3)

## ----m_bl_ln, eval=FALSE------------------------
#  m_bl_ln = conlnorm$new(blackouts$V1)
#  est = estimate_xmin(m_bl_ln)
#  m_bl_ln$setXmin(est)

## ----eval=FALSE---------------------------------
#  lines(m_bl_ln, col=3, lwd=2)

## -----------------------------------------------
data("native_american")
data("us_american")

## -----------------------------------------------
head(native_american, 3)

## ----echo=FALSE, fig.width=6, fig.height=4------
plot(native_american$Date, native_american$Cas, 
     log="y", pch=21, bg=1, 
     ylim=c(1, 2000), 
     cex=0.5, panel.first=grid(col="grey70"), 
     xlab="Date", ylab="#Casualties")

points(us_american$Date, us_american$Cas, 
       pch=24, bg=2, cex=0.5)

## ----cache=TRUE---------------------------------
m_na = displ$new(native_american$Cas)
m_us = displ$new(us_american$Cas)

## ----cache=TRUE---------------------------------
est_na = estimate_xmin(m_na, pars=seq(1.5, 2.5, 0.001))
est_us = estimate_xmin(m_us, pars=seq(1.5, 2.5, 0.001))

## ----cache=TRUE---------------------------------
m_na$setXmin(est_na)
m_us$setXmin(est_us)

## ----fig.keep='none', cache=TRUE, tidy=FALSE----
plot(m_na)
lines(m_na)
## Don't create a new plot
## Just store the output
d = plot(m_us, draw=FALSE)
points(d$x, d$y, col=2)
lines(m_us, col=2)

## ----echo=FALSE, fig.width=4, fig.height=4------
plot(m_na, bg=1, pch=21, cex=0.5,  
     panel.first=grid(col="grey70"), 
     xlab="#Casualties", ylab="CDF")
lines(m_na, lwd=2, col=1)

d = plot(m_us, draw=FALSE)
points(d$x, d$y, bg=2, pch=24, cex=0.5)
lines(m_us, lwd=2, col=2)

## -----------------------------------------------
print(sessionInfo(), locale = FALSE)

## ----clean-up, include=FALSE--------------------
# R compiles all vignettes in the same session, which can be bad
rm(list = ls(all = TRUE))

