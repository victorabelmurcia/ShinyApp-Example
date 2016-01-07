# function for printing summaries with standard deviations
# only for numerical variables
summarySD <- function(x) {
      stopifnot(is.numeric(x) | is.data.frame(x), is.null(dim(x)) | dim(x)[2] == 1)
      ### cast to simple numerical vector if data.frame (with one row)
      if(is.data.frame(x)) x <- as.numeric(x)
      ### create summary
      S <- round(summary(x), 4)
      S[8] <- round(sd(x, na.rm=TRUE), 4)
      names(S)[8] <- "Std. dev."
      S[9] <- as.integer(sum(!is.na(x)))
      names(S)[9] <- "N"
      return(S)
}

# Map input var to varname in the dataset
mapVarInput <- function(var) {
      varlabs <- c("L.-I. scale", "E-F knowledge: raw score",
                 "E-F knowledge: non-guessed score",
                 "F knowledge: non-guessed score",
                 "Parents' higher education: jointly",
                 "Father education", "Mother education",
                 "Study year", "Work experience",
                 "Gender", "Age",
                 "Hometown size PL", "Hometown size CZ",
                 "Country", "Type of education")
      varnames <- c("L.I.scale", "knowraw", "ngknow", "ngfinance",
                    "phighedu", "father_edu", "mother_edu", "study_year",
                    "work_experience", "gender", "age",
                    "PL_home_size", "CZ_home_size", "country", "typeofedu")
      map <- data.frame(varlabs, varnames)
      var <- as.character(map[map$varlabs==var, 2])
      return(var)
}

# Generate lattice graph formula
makeFormula <- function(var, numeric, crossvars=NULL) {
      stopifnot(is.logical(numeric) & length(numeric) == 1,
                length(crossvars) >= 0 & length(crossvars) <= 2)
      
      # Initialize formula object
      f <- NULL
      # Simplify grouping variables
      cross <- unlist(crossvars)
      
      # Numerical variables
      if(numeric) {
            if(length(cross) == 0) {
                  f <- as.formula(paste("~", var))
            }
            else {
                  f <- as.formula(paste("~", paste(var, paste(cross, collapse="+"), sep="|")))
            }
      }
      # categorical variables
      else {
            if(length(cross) == 0) {
                  f <- as.formula(paste("freq", var, sep="~"))
            }
            else {
                  f <- as.formula(paste(paste("freq", var, sep="~"), paste(cross, collapse="+"), sep="|"))
            }
      }
      return(f)
}

# Generate trellis xyplot regression formula
makeRegFormula <- function(var, crossvars=NULL) {
      stopifnot(length(crossvars) >= 0 & length(crossvars) <= 2)
      
      # Initialize formula object
      f <- NULL
      # Simplify grouping variables
      cross <- unlist(crossvars)
      
      # Generate formula
      if(length(cross) == 0) {
            f <- as.formula(paste(var, "L.I.scale", sep="~"))
      }
      else {
            f <- as.formula(paste(var, paste("L.I.scale", paste(cross, collapse="+"), sep="|"), sep="~"))
      }
      return(f)
}

# Generate lm formula
makeLmFormula <- function(x, y, crossvars=NULL, interact=TRUE) {
      stopifnot(length(crossvars) >= 0 & length(crossvars) <= 2)
      
      # Initialize formula object
      f <- NULL
      # Simplify grouping variables
      cross <- unlist(crossvars)
      
      # Generate formula
      if(length(cross) == 0) {
            f <- as.formula(paste(x, y, sep="~"))
      }
      else {
            if(interact) {
                  f <- as.formula(paste(x, paste(c(y, crossvars), collapse="*"), sep="~"))
            }
            else {
                  f <- as.formula(paste(x, paste(c(y, crossvars), collapse="+"), sep="~"))
            }
      }
      return(f)
}

# Compute Agresti-Coul binomial confidence interval
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

# Generate data.frame of binary variables means and 95% A.-C. confidence intervals
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