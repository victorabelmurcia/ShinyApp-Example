library(shiny)
library(RColorBrewer)
library(lattice)
library(latticeExtra)
library(curl)
library(car)
library(heplots)
library(survival)
library(doBy)
library(grid)
library(vcd)
library(binom)
source("functions.R")
source("plottingFunctions.R")

# Load data from V4 Lab github repo
load(curl("https://raw.githubusercontent.com/sztal/V4Lab_Analyses/master/Data/OfficialData/officialData.RData"))
data <- officialData
rm(officialData)

# Define server logic required to draw charts and compute numerical summaries
shinyServer(function(input, output) {
      # Get input parameters from the user's input
            
      # PANEL: univariate distribution
      output$univarPlot <- renderPlot({
            # Get input data
            indata <- list(var=input$var, cross=input$cross,
                           bins=input$bins + 1, count=input$count)
            
            # set the logic of display
            varlab <- indata$var
            var <- mapVarInput(varlab)
            cross <- unlist(indata$cross)
            cross[cross=="type of studies"] <- "typeofedu"
            x <- data[, var]
            x <- x[!is.na(x)]
            numeric <- is.numeric(data[, var])
            bins <- NULL
            if(numeric) {
                  bins <- seq(min(x), max(x), length.out = indata$bins)
            }
            count <- indata$count
            if(count) count <- "count"
            else count <- "percent"
            # initialize trellis formula
            f <- makeFormula(var, numeric=numeric, crossvars=cross)
            # empty data object (used only for categorical variables)
            dat <- NULL
            vnames <- NULL
            # prepare aggregated data for categorical display
            if(!numeric & length(cross) > 0) {
                  vnames <- c(var, cross)
                  dat <- data[, vnames]
                  dat <- cbind(dat, freq=rep(1, nrow(dat)))
                  dat <- aggregate(freq ~ ., data=dat, FUN=sum)
            }
            else if(!numeric & length(cross) == 0) {
                  dat <- data[, var]
                  dat <- data.frame(dat, freq=rep(1, length(dat)))
                  names(dat)[1] <- var
                  dat <- aggregate(freq ~ ., data=dat, FUN=sum)
            }
            
            # Draw plot
            if(numeric) {
                  histogram(f, data=data, breaks=bins, col="skyblue", border="white",
                            type=count, xlab=varlab, panel=function(x, ...) {
                                  panel.histogram(x, ...)
                                  panel.abline(v=mean(x, na.rm=TRUE), col="red",
                                               lwd=2, lty=2)
                            })
            }
            else {
                  barchart(f, data=dat, col="skyblue", border="white",
                           xlab=varlab, ylab="Count")
            }
      })
      # Render numerical summary
      output$univarSummary <- renderPrint({
            # Get input data
            indata <- list(var=input$var, cross=input$cross)
            
            # set the logic of display
            varlab <- indata$var
            var <- mapVarInput(varlab)
            cross <- unlist(indata$cross)
            cross[cross=="type of studies"] <- "typeofedu"
            numeric <- is.numeric(data[, var])
            
            # empty data object (used only for categorical variables)
            dat <- NULL
            vnames <- NULL
            # prepare aggregated data for categorical display
            if(!numeric & length(cross) > 0) {
                  vnames <- c(var, cross)
                  dat <- data[, vnames]
                  dat <- cbind(dat, freq=rep(1, nrow(dat)))
                  dat <- aggregate(freq ~ ., data=dat, FUN=sum)
            }
            else if(!numeric & length(cross) == 0) {
                  dat <- data[, var]
                  dat <- data.frame(dat, freq=rep(1, length(dat)))
                  names(dat)[1] <- var
                  dat <- aggregate(freq ~ ., data=dat, FUN=sum)
            }
            
            if(numeric) {
                  if(length(cross) == 0) {
                        summarySD(data[, var])
                  }
                  else if(length(cross) == 1) {
                        tapply(data[, var], data[, cross], summarySD)
                  }
                  else {
                        tapply(data[, var], interaction(data[, cross[1]], data[, cross[2]]),
                               summarySD)
                  }
            }
            else {
                  pdat <- dat
                  if(ncol(pdat) == 2) names(pdat) <- c(varlab, "Count")
                  else if(ncol(pdat) == 3) {
                        crosslab <- cross
                        crosslab[cross == "typeofedu"] <- "type of studies"
                        names(pdat) <- c(varlab, crosslab, "Count")
                  }
                  else {
                        crosslab <- cross
                        crosslab[cross == "typeofedu"] <- "type of studies"
                        names(pdat) <- c(varlab, crosslab[1], crosslab[2], "Count")
                  }
                  pdat
            }
      })
      
      
      # PANEL: Bivariate associations
      # Bivariate plots
      output$bivarPlot <- renderPlot({
            # Get input data
            indata <- list(bivar1=input$bivar1, bivar2=input$bivar2)
            
            # set the logic of display
            bivarlab1 <- indata$bivar1
            bivarlab2 <- indata$bivar2
            bivar1 <- mapVarInput(bivarlab1)
            bivar2 <- mapVarInput(bivarlab2)
            numeric1 <- is.numeric(data[, bivar1])
            numeric2 <- is.numeric(data[, bivar2])
            
            # Plot associations
            if(numeric1 & numeric2) {
                  f <- makeLmFormula(bivar1, bivar2)
                  xyplot(f, data=data, xlab=bivarlab1, col="skyblue", pch=16,
                         ylab=bivarlab2, panel=function(x, y, ...) {
                               panel.xyplot(x, y, ..., grid=TRUE)
                               panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="red",
                                             digits=2, offset=1, at=.25, pos=1)
                         })
            }
            else if(numeric1 | numeric2) {
                  numvec <- c(numeric1, numeric2)
                  numind <- which(numvec)
                  contvar <- NULL
                  contvarlab <- NULL
                  catvar <- NULL
                  catvarlab <- NULL
                  if(numind == 1) {
                        contvar <- bivar1
                        contvarlab <- bivarlab1
                        catvar <- bivar2
                        catvarlab <- bivarlab2
                  }
                  else {
                        contvar <- bivar2
                        contvarlab <- bivarlab2
                        catvar <- bivar1
                        catvarlab <- bivarlab1
                  }
                  f <- makeLmFormula(contvar, catvar)
                  mvec <- tapply(data[, contvar], data[, catvar], mean, na.rm=TRUE)
                  ivec <- 1:length(mvec)
                  
                  # Draw plot
                  bwplot(f, data=data, ylab=contvarlab,
                         par.settings=list(box.rectangle=list(fill="skyblue", alpha=0.7))) +
                  xyplot(mvec ~ ivec, type="l", lwd=1.2, lty=2, col="red")
            }
            else {
                  colpal <- colorRampPalette(c("white", "lightgray", "skyblue"), alpha=TRUE)
                  levelplot(table(data[, bivar1], data[, bivar2]), col.regions=colpal, pretty=TRUE,
                            xlab=bivarlab1, ylab=bivarlab2, regions=FALSE)
            }
      })
      # Bivariate tests and summaries
      output$bivarSummary <- renderPrint({
            # Get input data
            indata <- list(bivar1=input$bivar1, bivar2=input$bivar2)
            
            # set the logic of display
            bivarlab1 <- indata$bivar1
            bivarlab2 <- indata$bivar2
            bivar1 <- mapVarInput(bivarlab1)
            bivar2 <- mapVarInput(bivarlab2)
            numeric1 <- is.numeric(data[, bivar1])
            numeric2 <- is.numeric(data[, bivar2])
            
            # Make summaries
            if(numeric1 & numeric2) {
                  f <- makeLmFormula(bivar1, bivar2)
                  summary(lm(f, data=data))
            }
            else if(numeric1 | numeric2) {
                  numvec <- c(numeric1, numeric2)
                  numind <- which(numvec)
                  contvar <- NULL
                  contvarlab <- NULL
                  catvar <- NULL
                  catvarlab <- NULL
                  if(numind == 1) {
                        contvar <- bivar1
                        contvarlab <- bivarlab1
                        catvar <- bivar2
                        catvarlab <- bivarlab2
                  }
                  else {
                        contvar <- bivar2
                        contvarlab <- bivarlab2
                        catvar <- bivar1
                        catvarlab <- bivarlab1
                  }
                  f <- makeLmFormula(contvar, catvar)
                  if(length(levels(data[, catvar])) == 2) {
                        t.test(f, data=data)
                  }
                  else {
                        summaryBy(f, data=data, FUN=c(mean, sd))
                        print(etasq(lm(f, data=data), partial=FALSE, anova=TRUE, type=2))
                  }
            }
            else {
                  t <- table(data[, bivar1], data[, bivar2])
                  print(t[ncol(t):1, ])
                  assocstats(t)
            }
      })
      
      # PANEL: E-F knowledge and the L.-I. scale
      output$LIknowPlot <- renderPlot({
            # Get input data
            indata <- list(var=input$varKL, cross=input$crossKL, interact=input$interact,
                           Ftests=input$Ftests, Mtests=input$Mtests)
            
            # set the logic of display
            varlab <- indata$var
            var <- mapVarInput(varlab)
            cross <- unlist(indata$cross)
            cross[cross=="type of studies"] <- "typeofedu"
            interact <- indata$interact
            
            # Plot association
            # get trellis formula
            f <- makeRegFormula(var, crossvars=cross)
            # draw plot
            xyplot(f, data=data, xlab="L.-I. scale", col="skyblue", pch=16,
                   ylab=varlab, panel=function(x, y, ...) {
                         panel.xyplot(x, y, ..., grid=TRUE)
                         panel.ablineq(lm(y ~ x), r.sq=TRUE, rot=TRUE, lwd=2, lty=2, col="red",
                                       digits=2, offset=1, at=.25, pos=1)
                   })
      })
      # Render numerical summary
      output$regSummary <- renderPrint({
            # Get input data
            indata <- list(var=input$varKL, cross=input$crossKL, interact=input$interact,
                           Ftests=input$Ftests, Mtests=input$Mtests)
            
            # set the logic of display
            varlabRS <- indata$var
            varRS <- mapVarInput(varlabRS)
            varRS2 <- "L.I.scale"
            crossRS <- unlist(indata$cross)
            crossRS[crossRS=="type of studies"] <- "typeofedu"
            Ftests <- indata$Ftests
            Mtests <- indata$Mtests
            interact <- indata$interact
            
            if(Ftests) {
                  print("### F test (III type with partial eta squared) ###")
                  if(interact) {
                        # get formula
                        f <- makeLmFormula(varRS, varRS2, crossvars=crossRS, interact=TRUE)
                        print(etasq(lm(f, data=data), partial=TRUE, anova=TRUE, type=3))
                  }
                  else {
                        # get formula
                        f <- makeLmFormula(varRS, varRS2, crossvars=crossRS, interact=FALSE)
                        print(etasq(lm(f, data=data), anova=TRUE, partial=TRUE, type=3))
                  }
            }
            if(Mtests & Ftests) cat("\n#################\n\n")
            if(Mtests) {
                  if(length(crossRS) == 0) {
                        # get formula
                        f <- makeLmFormula(varRS, varRS2)
                        print("### Marginal tests and model specification")
                        summary(lm(f, data=data))
                  }
                  else {
                        f <- makeLmFormula(varRS, varRS2)
                        lvls <- interaction(data[, crossRS])
                        
                        for(lvl in levels(lvls)) {
                              inds <- lvls == lvl & !is.na(lvls)
                              print(sprintf(
                                    "### %s: marginal tests and subgroup model specification", lvl))
                              print(summary(lm(f, data=data[inds, ])))
                        }
                  }
            }
      })
      
      
      # PANEL: items analysis
      output$itemsPlot <- renderPlot({
            # Get input data
            indata <- list(cvec=input$cntry, stats=input$stats, tests=input$tests,
                           parti=input$parti)
            
            # set the logic of display
            cvec <- unlist(indata$cvec)
            stast <- indata$stats
            tests <- indata$tests
            parti <- indata$parti
            kdat <- data[, grep("^k[0-9]+", names(data), perl=TRUE)]
            kdat <- (kdat - 1) * (-1) # to transform 1 to 0 to model failures instead of successes
            substr(names(kdat), 1, 1) <- "i"
            
            # Generate plot
            if(length(cvec) == 2) {
                  d.pl <- dlevelFrame(kdat[data$country=="PL", ], vcol=TRUE)[, 1:2]
                  d.cz <- dlevelFrame(kdat[data$country=="CZ", ], vcol=FALSE)[, 1]
                  dlev <- cbind(d.pl, d.cz)
                  names(dlev) <- c("Items", "Difficulty.PL", "Difficulty.CZ")
                  dlev$Difference <- dlev$Difficulty.PL - dlev$Difficulty.CZ
                  lo <- vector(mode="numeric", length=nrow(dlev))
                  names(lo) <- rownames(dlev)
                  up <- vector(mode="numeric", length=nrow(dlev))
                  names(up) <- rownames(dlev)
                  significant <- vector(mode="numeric", length=nrow(dlev))
                  names(significant) <- rownames(dlev)
                  for(item in rownames(dlev)) {
                        xs <- rev(tapply(kdat[, item], data$country, sum, na.rm=TRUE))
                        ns <- rev(tapply(kdat[, item], data$country, function(x) sum(!is.na(x))))
                        ptest <- prop.test(xs, ns)
                        lo[item] <- ptest$conf.int[1]
                        up[item] <- ptest$conf.int[2]
                        significant[item] <- ptest$p.value <= 0.05
                  }
                  dlev$lo <- lo
                  dlev$up <- up
                  
                  # Draw plot
                  dotplot(reorder(Items, Difficulty.PL) ~ Difficulty.PL, data=dlev,
                          par.settings=list(superpose.symbol=list(pch=list(1, 17),
                                                                  cex=1.5,
                                                                  col="black")),
                          xlab="Item difficulty",
                          panel=function(x, y, subscripts, ...) {
                                panel.dotplot(x, y, col="black", pch="O", cex=1, ...)
                                panel.abline(v=0.5, lwd=1.5, lty=2, col="red")
                                panel.dotplot(dlev$Difficulty.CZ[subscripts], y,
                                              col="black", pch=17, cex=1, ...)
                          }, subscripts=TRUE, auto.key=list(text=c("PL", "CZ"), rows=2, cex=1,
                                                            space="right"))
            }
            else {
                  dlev.cntry <- dlevelFrame(kdat[data$country==cvec, ], vcol=TRUE)
                  names(dlev.cntry) <- c("Item", "Difficulty", "lo", "up")
                  
                  # Draw plot
                  dotplot(reorder(Item, Difficulty) ~ Difficulty, data=dlev.cntry,
                          xlab="Item difficulty", pch="O", col="black",
                          lo=dlev.cntry$lo, up=dlev.cntry$up,
                          panel=function(x, y, lo, up, ...) {
                                panel.dotplot(x, y, ...)
                                panel.abline(v=0.5, lwd=1.5, lty=2, col="red")
                                panel.arrows(x0=lo, y0=y,
                                             x1=up, y1=y, code=3,
                                             angle=90, length=0.05, col="skyblue")
                          })
            }            
      })
      
      # Item summary
      output$itemsSummary <- renderPrint({
            # Get input data
            indata <- list(cvec=input$cntry, stats=input$stats, tests=input$tests,
                           parti=input$parti, sigonly=input$sigonly, digits=input$digits)
            
            # set the logic of display
            cvec <- unlist(indata$cvec)
            stats <- indata$stats
            tests <- indata$tests
            parti <- indata$parti
            sigonly <- indata$sigonly
            digits <- indata$digits
            kdat <- data[, grep("^k[0-9]+", names(data), perl=TRUE)]
            kdat <- (kdat - 1) * (-1) # to transform 1 to 0 to model failures instead of successes
            substr(names(kdat), 1, 1) <- "i"
            
            # Generate data
            if(length(cvec) == 2) {
                  d.pl <- dlevelFrame(kdat[data$country=="PL", ], vcol=TRUE)[, 1:2]
                  d.cz <- dlevelFrame(kdat[data$country=="CZ", ], vcol=FALSE)[, 1]
                  dlev <- data.frame(cbind(d.pl[, 2], d.cz))
                  rownames(dlev) <- d.pl[, 1]
                  names(dlev) <- c("Difficulty.PL", "Difficulty.CZ")
                  dlev$Difference <- dlev$Difficulty.PL - dlev$Difficulty.CZ
                  lo <- vector(mode="numeric", length=nrow(dlev))
                  names(lo) <- rownames(dlev)
                  up <- vector(mode="numeric", length=nrow(dlev))
                  names(up) <- rownames(dlev)
                  significant <- vector(mode="numeric", length=nrow(dlev))
                  names(significant) <- rownames(dlev)
                  for(item in rownames(dlev)) {
                        xs <- rev(tapply(kdat[, item], data$country, sum, na.rm=TRUE))
                        ns <- rev(tapply(kdat[, item], data$country, function(x) sum(!is.na(x))))
                        ptest <- prop.test(xs, ns)
                        lo[item] <- ptest$conf.int[1]
                        up[item] <- ptest$conf.int[2]
                        significant[item] <- ptest$p.value <= 0.05
                  }
                  dlev$lo <- lo
                  dlev$up <- up
                  dlev$significant <- significant
            }
            else if(length(cvec) == 1) {
                  dlev <- dlevelFrame(kdat[data$country==cvec, ], vcol=FALSE)
                  names(dlev) <- c("Difficulty", "lo", "up")
                  items <- rownames(dlev)
                  sigs <- vector(mode="numeric", length=nrow(dlev))
                  names(sigs) <- items
                  
                  for(item in items) {
                        xs <- sum(kdat[data$country==cvec, item], na.rm=TRUE)
                        ns <- sum(!is.na(kdat[data$country==cvec, item]))
                        ptest <- prop.test(xs, ns)
                        if(ptest$p.value <= 0.05 & all(ptest$conf.int < 0.5)) sigs[item] <- 1
                        else if(ptest$p.value <= 0.05 & all(ptest$conf.int > 0.5)) sigs[item] <- -1
                        else sigs[item] <- 0
                  }
                  
                  tricky <- rownames(dlev)[sigs == -1]
                  medium <- rownames(dlev)[sigs == 0]
                  easy <- rownames(dlev)[sigs == 1]
                  partition <- list(tricky=tricky, medium=medium, easy=easy)
            }
            
            # Print summaries
            if(length(cvec) == 1) {
                  if(parti) {
                        cat("### ITEMS PARTITIONING ###\n\n")
                        print(partition)
                        cat("\n#########\n")
                  }
                  if(stats) print(round(dlev, digits))
            }
            else if(length(cvec) == 2) {
                  if(stats) {
                        if(sigonly) {
                              print(round(
                                    dlev[dlev$significant == 1, -which(names(dlev)=="significant")],
                                    digits))
                        }
                        else print(round(dlev, digits))
                  }
                  if(tests) {
                        items <- NULL
                        if(sigonly) {
                              items <- rownames(dlev)[dlev$significant == 1]
                        }
                        else {
                              items <- rownames(dlev)
                        }
                        for(item in items) {
                              xs <- tapply(kdat[, item], data$country, sum, na.rm=TRUE)
                              ns <- tapply(kdat[, item], data$country, function(x) sum(!is.na(x)))
                              cat(sprintf("### ITEM: %s", item))
                              print(prop.test(xs, ns))
                        }
                  }
            }
      })
})