#############################################
### Plotting tools for the V4 Lab project ###
#############################################

### !!! <--- Function 1 ---> !!! ### (START)
#######################################################################
### Compute Agresti-Coull confidence interval for a binary variable ###
#######################################################################
acInterval <- function(vec, alpha = 0.05) {
      ### This function take 2 arguments:
      ###   - vec : a vector of numerical binary data
      ###   - alpha : significance level

      ### Check the input data
      stopifnot(is.numeric(vec) | (is.data.frame(vec) & nrow(vec) == 1),
                is.numeric(alpha) & length(alpha) == 1,
                all(levels(as.factor(as.numeric(vec))) == c("0", "1")))
      
      if(is.data.frame(vec)) vec <- as.numeric(vec)
      ### Compute the z parameter
      z <- qnorm(1 - alpha/2)
      n <- length(vec[!is.na(vec)])
      x <- sum(vec, na.rm=TRUE)
      ### Choose the computation approach
      ndash <- n + z^2
      pdash <- (1/ndash) * (x + .5 * z^2)
      wing <- z * sqrt((1/ndash) * pdash * (1 - pdash))
      CI <- pdash + c(-1, 1) * wing
      return(CI)
}
### !!! <--- Function 1 ---> !!! ### (END)


### !!! <--- Function 2 ---> !!! ### (START)
###############################################################
### Plot binary varibales with/without confidence intervals ###
###############################################################
plotBinary <- function(data, ci=TRUE, reverse=FALSE, reorder=TRUE, vline=FALSE, alpha=.05, theme = NULL) {
      ### This function takes 5 arguments:
      ###   - data : a data.frame with the variables
      ###   - ci : flag indicating wheter confidence intervals should plotted (Agresti-Coull)
      ###   - reverse : flag indicating whether 1 - p should be returned instead of p
      ###   - reorder : flag indicating whether variables should be reordered
      ###   - vline : flag indicating whether a vertical line at 0.5 should be plotted
      ###   - alpha : significance level for the confidence interval
            
      ### Check the input data
      stopifnot(is.data.frame(data),
                is.logical(ci), is.logical(reverse), is.logical(reorder), is.logical(vline),
                is.null(theme) | is.list(theme))
      
      ### Load lattice and latticeExtra packages
      stopifnot(library(lattice, logical.return=TRUE),
                library(latticeExtra, logical.return=TRUE))
      
      ### Prepare data
      vars <- names(data)
      p <- apply(data, 2, mean, na.rm=TRUE)
      lo <- vector(mode="numeric", length=length(vars))
      up <- vector(mode="numeric", length=length(vars))
      pdat <- data.frame(vars=vars, p=p, lo=lo, up=up)
      rownames(pdat) <- vars
      if(reverse) pdat[, "p"] <- 1 - pdat[, "p"]
      n <- dim(data)[1]
      ### Get A-C intervals
      for(var in vars) {
            p <- pdat[var, "p"]
            x <- p*n
            if(reverse) cint <- acInterval(vec=1-data[, var], alpha=alpha)
            else cint <- acInterval(vec=data[, var], alpha=alpha)
            veclo <- cint[1]
            vecup <- cint[2]
            pdat[var, "lo"] <- veclo
            pdat[var, "up"] <- vecup
      }
      if(reorder) dplot <- dotplot(reorder(vars, p) ~ p, data = pdat)
      else dplot <- dotplot(vars ~ p, data = pdat)
      if(!is.null(theme)) dplot <- update(dplot, par.settings=theme)
      if(ci) {
            dplot <- update(dplot, lo=pdat$lo, up=pdat$up,
                            panel=function(x, y, lo, up, ...) {
                                  panel.dotplot(x, y, ...)
                                  panel.arrows(x0=lo, y0=y,
                                               x1=up, y1=y, code=3,
                                               angle=90, length=0.05, col="gray3")
                            })
      }
      if(vline) dplot <- dplot + layer(panel.abline(v=0.5, lwd=.8, lty=2))
      if(reverse) dplot <- update(dplot, xlab = "Probability of a Failure")
      else dplot <- update(dplot, xlab = "Probability of a Success")
      return(dplot)
}
### !!! <--- Function 2 ---> !!! ### (END)

# ### !!! <--- Function 3 ---> !!! ### (START)
# ###############################################################################
# ### GENERATE DATASET WITH ITEMS' DIFFICULTY LEVELS AND CONFIDENCE INTERVALS ###
# ###############################################################################
dlevelFrame <- function(dat, vcol=FALSE) {
      ### Takes two arguments:
      ###   - dat: a data.frame with binary variables
      ###   - vcol: logical flag indicating whether varnames should be included in the first column of the frame; useful in plotting

      ### Check the input data
      stopifnot(is.data.frame(dat))
      
      ### Generate data
      vnames <- names(dat)
      m <- ncol(dat)
      n <- nrow(dat)
      
      Frame <- data.frame(matrix(0, nrow=m, ncol=3))
      rownames(Frame) <- vnames
      colnames(Frame) <- c("difficulty", "lower", "upper")
      
      for(v in vnames) {
            Frame[v, 1] <- mean(dat[, v], na.rm=TRUE)
            Frame[v, 2:3] <- acInterval(dat[, v])
      }
      if(vcol) {
            vars <- rownames(Frame)
            Frame <- cbind(vars, Frame)
      }
      return(Frame)
}
