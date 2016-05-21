#!/usr/local/bin/Rscript

###############################################################################
# FILE: rfPardep_main.R
#
# USAGE:  rfPardep_main.R -c PARDEP.conf
#
# DESCRIPTION:
#       Computes single-variable and two-variable partial dependence plots, and
#       two-variable interaction statistics from a fitted RuleFit model.
#
# ARGUMENTS:
#       PARDEP.conf: specifies path to previously built RuleFit model and plot
#                    control parameters
#
# REQUIRES:
#       REGO_HOME: environment variable pointing to the directory where you 
#                  have placed this file (and its companion ones)
#         RF_HOME: environment variable pointing to appropriate RuleFit
#                  executable -- e.g., export RF_HOME=$REGO_HOME/lib/RuleFit/mac
#           
# AUTHOR: Giovanni Seni <Giovanni_Seni@intuit.com> 
###############################################################################
REGO_HOME <- Sys.getenv("REGO_HOME")
source(file.path(REGO_HOME, "/src/logger.R"))
source(file.path(REGO_HOME, "/src/rfExport.R"))
source(file.path(REGO_HOME, "/src/rfRulesIO.R"))
source(file.path(REGO_HOME, "/lib/RuleFit/rulefit.r"))
source(file.path(REGO_HOME, "/src/rfGraphics.R"))
library(ROCR, verbose = FALSE, quietly=TRUE, warn.conflicts = FALSE)
library(getopt)

ValidateConfigArgs <- function(conf)
{
  # Validates and initializes configuration parameters.
  #
  # Args:
  #      conf: A list of <param name, param value> pairs
  # Returns:
  #   A list of <param name, param value> pairs
  stopifnot("var.name" %in% names(conf))
  stopifnot("out.path" %in% names(conf))

  ## Check Model path
  stopifnot("model.path" %in% names(conf))
  if (!(file.exists(conf$model.path))) {
    stop("Didn't find model directory:", conf$model.path, "\n")
  }

  ## Number of observations to include in averaging calculation
  if (!("num.obs" %in% names(conf))) {
    conf$num.obs <- 500
  } else {
    conf$num.obs <- as.numeric(conf$num.obs)
  }
  
  ## Number of distinct variable evaluation points 
  if (!("var.num.values" %in% names(conf))) {
    conf$var.num.values <- 200
  } else {
    conf$var.num.values <- as.numeric(conf$var.num.values)
  }

  ## Trim extreme values of variable
  if (!("var.trim.qntl" %in% names(conf))) {
    conf$var.trim.qntl <- 0.025
  } else {
    conf$var.trim.qntl <- as.numeric(conf$var.trim.qntl)
  }

  ## Rug quantile to show numeric variable data density
  if (!("var.rug.qntl" %in% names(conf))) {
    conf$var.rug.qntl <- 0.1
  } else {
    conf$var.rug.qntl <- as.numeric(conf$var.rug.qntl)
  }

  ## Text orientation of level names (for categorical variable)
  if (!("var.levels.las" %in% names(conf))) {
    conf$var.levels.las <- 1
  } else {
    conf$var.levels.las <- as.numeric(conf$var.levels.las)
  }
  
  ## Show partial dependence distribution
  if (!("show.pdep.dist" %in% names(conf))) {
    conf$show.pdep.dist <- FALSE
  } else {
    conf$show.pdep.dist <- (as.numeric(conf$show.pdep.dist) == 1)
  }

  ## Show partial dependence mean
  if (!("show.yhat.mean" %in% names(conf))) {
    conf$show.yhat.mean <- FALSE
  } else {
    conf$show.yhat.mean <- (as.numeric(conf$show.yhat.mean) == 1)
  }
  
  ## This determines how far the whiskers of a categorical variable extend out from 
  ## the boxplot's box (this value times interquartile range gives whiskers range)
  if (!("var.boxplot.range" %in% names(conf$var.boxplot.range))) {
    conf$var.boxplot.range <- 1.0e-4
  } else {
    conf$var.boxplot.range <- as.numeric(conf$var.boxplot.range)
  }
  
  ## Auto-detect the platform:
  conf$rf.platform <- switch(.Platform$OS.type
                             , windows = "windows"
                             , unix = switch(Sys.info()["sysname"]
                                   , Linux = "linux"
                                   , Darwin = "mac"))

  if (is.null(conf$rf.platform)) error(logger, "Unable to detect platform")

  ## Did user specified a log level?
  if (is.null(conf$log.level)) {
    conf$log.level <- kLogLevelINFO
  } else {
    conf$log.level <- get(conf$log.level)
  }
  
  ## Did user specified an output file?
  if (is.null(conf$out.fname)) {
    conf$out.fname <- paste(conf$var.name, ".PNG", sep="")
  }

  return(conf)
}

ValidateCmdArgs <- function(opt, args.m)
{
  # Parses and validates command line arguments.
  #
  # Args:
  #      opt: getopt() object
  #   args.m: valid arguments spec passed to getopt().
  #
  # Returns:
  #   A list of <param name, param value> pairs
  kUsageString <- "/path/to/rfPardep_main.R -c <Plot configuration file>"
  
  # Validate command line arguments
  if ( !is.null(opt$help) || is.null(opt$conf) ) {
    self <- commandArgs()[1]
    cat("Usage: ", kUsageString, "\n")
    q(status=1);
  }

  # Read config file (two columns assumed: 'param' and 'value')
  tmp <- read.table(opt$conf, header=T, as.is=T)
  conf <- as.list(tmp$value)
  names(conf) <- tmp$param
  conf <- ValidateConfigArgs(conf)
  
  # Do we have a log file name? "" will send messages to stdout
  if (is.null(opt$log)) {
    opt$log <- ""
  }
  conf$log.fname <- opt$log
  
  return(conf)
}

ComputeSinglePDep <- function(var, x, max.var.vals=40, sample.size=500, qntl=0.025, var.levels=NULL)
{
  # Compute single variable partial dependence plot.
  #
  # Args:
  #               var: variable identifier
  #                 x: training data
  #      max.var.vals: maximum number of abscissa evaluation points for numeric variables
  #       sample.size: number of observations used for averaging calculations
  #              qntl: trimming factor for plotting numeric variables
  #        var.levels: var level names (if var is categorical)
  
  ## Get evaluation points
  if (is.factor(x[,var]) || length(var.levels) > 0) {
    ## Use all levels for categorical variables
    is.fact <- TRUE
    var.vals <- sort(unique(x[,var]))
  } else {
    ## Use 'max.var.vals' percentiles for numeric variables
    is.fact <- FALSE
    var.vals <- quantile(x[,var], na.rm=T, probs=seq(qntl, 1-qntl, 1.0/max.var.vals))
  }
  num.var.vals <- length(var.vals)
  par.dep <- rep(0, num.var.vals)

  ## Get random sample of observations (to speed things up)
  x.sample <- x[sample(1:nrow(x), size=sample.size),]

  ## Compute partial dependence over selected random sample
  y.hat.m <- matrix(nrow=sample.size, ncol=num.var.vals)
  for (i.var.val in 1:num.var.vals) {
     ## Hold x[,var] constant
     x.sample[,var] <- var.vals[i.var.val]
     ## Compute y.hat
     y.hat.m[,i.var.val] <- rfpred(x.sample)
     ## Compute avg(y.hat) 
     par.dep[i.var.val] <- mean(y.hat.m[,i.var.val])
  }

  return(list(var.vals = var.vals, par.dep = par.dep, y.hat.m = y.hat.m))
}

PlotSinglePDep <- function(var, x, max.var.vals=40, sample.size=500, qntl=0.025, rugqnt=0.1, main="",
                       var.levels=NULL, var.boxplot.range=1.0e-4, var.levels.las=1, shift.pdep=FALSE,
                       show.pdep.dist=FALSE, show.yhat.mean=FALSE)
{
  # Generates single variable partial dependence plot.
  #
  # Args:
  #               var: variable identifier
  #                 x: training data
  #      max.var.vals: maximum number of abscissa evaluation points for numeric variables
  #       sample.size: number of observations used for averaging calculations
  #              qntl: trimming factor for plotting numeric variables
  #            rugqnt: quantile for data density tick marks on numeric variables
  #        var.levels: var level names (if var is categorical)
  # var.boxplot.range: 'range' parameter to boxplot
  #    var.levels.las: orientation of var level names (if var is categorical)
  #       shift.pdep: shift pdep so that min is zero
  #    show.pdep.dist: show partial dependence distribution
  #    show.yhat.mean: show line indicating avg(yHat)
  
  ## Compute partial dependence
  pdep <- ComputeSinglePDep(var, x, max.var.vals, sample.size, qntl, var.levels)
  num.var.vals <- length(pdep$var.vals)
  
  ## Do we want to "shift" the partial dependence?
  if (shift.pdep && show.pdep.dist == FALSE) {
    pdep$par.dep <- pdep$par.dep - min(pdep$par.dep)
  }
  
  ## Do we want to show global mean(yhat)?
  if (show.yhat.mean) {
    mean.y.hat <- mean(pdep$y.hat.m)
  }
  
  ## Is var a factor?
  if (is.factor(x[,var]) || length(var.levels) > 0) {
    is.fact <- TRUE
  } else {
    is.fact <- FALSE
  }
  
  ## Plot partial dependence according to variable type
  if (is.fact) {
    if (length(var.levels) > 0) {
      var.names <- var.levels
    } else {
      var.names <- pdep$var.vals
    }
    ## Adjust bar widths according to var's density
    bar.widths <- table(x[,var])/length(x[,var]) 
    if (show.pdep.dist) {
      boxplot(pdep$y.hat.m, names=var.names, width=bar.widths,
              xlab=var, ylab='Partial dependence',
              outline=FALSE, range=var.boxplot.range)
      points(1:num.var.vals, pdep$par.dep, col='blue') ## indicate averages
      if (show.yhat.mean) {
        lines(x=c(0, ncol(pdep$y.hat.m)+1), y=c(mean.y.hat, mean.y.hat), col='blue', lty=3)
      }
    } else {
      barplot(pdep$par.dep, names=var.names, width=bar.widths, 
              xlab=var, ylab='Partial dependence', cex.names=0.75, las=var.levels.las)
    }
  } else {
    if (show.pdep.dist) {
      ## Add boxplot stats (lower whisker, lower hinge, median, upper hinge, upper whisker) 
      ## where
      ##   lower whisker = max(min(x), Q_1 - 1.5 * IQR) 
      ##   upper whisker = min(max(x), Q_3 + 1.5 * IQR) 
      var.vals.bp <- boxplot(pdep$y.hat.m, at = pdep$var.vals, plot = F)
      ymin <- min(var.vals.bp$stats)
      ymax <- max(var.vals.bp$stats)
      plot(c(min(pdep$var.vals), max(pdep$var.vals)), c(0,0), ylim=c(ymin, ymax), xlab=var, ylab='Partial dependence', 
           type='l', lty=3, col='white')
      matlines(pdep$var.vals, t(var.vals.bp$stats), lty=c(3,2,1,2,3), col=c('black','black','white','black','black'))
      lines(pdep$var.vals, pdep$par.dep, type='l', col='red')
      if (show.yhat.mean) {
        lines(c(min(pdep$var.vals), max(pdep$var.vals)), c(mean.y.hat, mean.y.hat), col='blue', lty=3)
      }
    } else {
      ymin <- min(pdep$par.dep)
      ymax <- max(pdep$par.dep)
      plot(c(min(pdep$var.vals), max(pdep$var.vals)), c(0,0), ylim=c(ymin, ymax), xlab=var, ylab='Partial dependence', type='l', lty=3)
      lines(pdep$var.vals, pdep$par.dep, type='l', col='red')
    }
    ## Add var's density rug at top
    axis(3, quantile(x[,var], probs=seq(qntl, 1-qntl, rugqnt)), labels=F)
  }
  title(main)
}

ComputePairPDep <- function(var1, var2, x, max.var.vals=40, sample.size=500, qntl=0.025, var1.levels=NULL, 
                            var2.levels=NULL)
{
  # Compute two variable partial dependence plot.
  #
  # Args:
  #               var1: variable identifier for the first variable to be plotted
  #               var2: variable identifier for the second variable to be plotted
  #                 x: training data
  #      max.var.vals: maximum number of abscissa evaluation points for numeric variables
  #       sample.size: number of observations used for averaging calculations
  #              qntl: trimming factor for plotting numeric variables
  #        var1.levels: var1 level names (if var1 is categorical)
  #        var2.levels: var2 level names (if var2 is categorical)
  
  ## Get evaluation points
  if (is.factor(x[,var1]) || length(var1.levels) > 0) {
    ## Use all levels for categorical variables
    is.var1.fact <- TRUE
    var1.vals <- sort(unique(x[,var1]))
  } else {
    ## Use 'max.var.vals' percentiles for numeric variables
    is.var1.fact <- FALSE
    var1.vals <- quantile(x[,var1], na.rm=T, probs=seq(qntl, 1-qntl, 1.0/max.var.vals))
  }
  if (is.factor(x[,var2]) || length(var2.levels) > 0) {
    ## Use all levels for categorical variables
    is.var2.fact <- TRUE
    var2.vals <- sort(unique(x[,var2]))
  } else {
    ## Use 'max.var.vals' percentiles for numeric variables
    is.var2.fact <- FALSE
    var2.vals <- quantile(x[,var2], na.rm=T, probs=seq(qntl, 1-qntl, 1.0/max.var.vals))
  }
  num.var1.vals <- length(var1.vals)
  num.var2.vals <- length(var2.vals)
  par.dep <- matrix(0, nrow = num.var1.vals, ncol = num.var2.vals)
  
  ## Get random sample of observations (to speed things up)
  x.sample <- x[sample(1:nrow(x), size=sample.size),]
  
  ## Compute partial dependence over selected random sample
  y.hat.a <- array(dim=c(sample.size, num.var1.vals, num.var2.vals))
  for (i.var1.val in 1:num.var1.vals) {
    for (i.var2.val in 1:num.var2.vals) {
      ## Hold x[,var] constant
      x.sample[,var1] <- var1.vals[i.var1.val]
      x.sample[,var2] <- var2.vals[i.var2.val]
      ## Compute y.hat
      y.hat.a[, i.var1.val, i.var2.val] <- rfpred(x.sample)
      ## Compute avg(y.hat) 
      par.dep[i.var1.val, i.var2.val] <- mean(y.hat.a[, i.var1.val, i.var2.val])
    }
  }
  
  return(list(var1.vals = var1.vals, var2.vals = var2.vals, par.dep = par.dep, y.hat.m = y.hat.a))
}

PlotPairPDep <- function(var1, var2, x, max.var.vals=40, sample.size=500, qntl=0.025, rugqnt=0.1, main="",
                     var1.levels=NULL, var2.levels=NULL, var.boxplot.range=1.0e-4, var.levels.las=1,
                     shift.pdep=FALSE, show.pdep.dist=FALSE, show.yhat.mean=FALSE)
{
  # Generates two variable partial dependence plot.
  #
  # Args:
  #               var1: variable identifier for the first variable to be plotted
  #               var2: variable identifier for the second variable to be plotted
  #                  x: training data
  #       max.var.vals: maximum number of abscissa evaluation points for numeric variables
  #        sample.size: number of observations used for averaging calculations
  #               qntl: trimming factor for plotting numeric variables
  #             rugqnt: quantile for data density tick marks on numeric variables
  #        var1.levels: var1 level names (if var1 is categorical)
  #        var2.levels: var2 level names (if var2 is categorical)
  #  var.boxplot.range: 'range' parameter to boxplot
  #     var.levels.las: orientation of var level names (if var is categorical)
  #         shift.pdep: shift pdep so that min is zero
  #     show.pdep.dist: show partial dependence distribution
  #     show.yhat.mean: show line indicating avg(yHat)
  
  ## Compute partial dependence
  pdep <- ComputePairPDep(var1, var2, x, max.var.vals, sample.size, qntl, var1.levels, var2.levels)
  num.var1.vals <- length(pdep$var1.vals)
  num.var2.vals <- length(pdep$var2.vals)
  
  ## Do we want to "shift" the partial dependence?
  pdep.lim <- NULL
  if (shift.pdep && show.pdep.dist == FALSE) {
    for (i.var1.val in 1:num.var1.vals) {
      pdep$par.dep[i.var1.val,] <- pdep$par.dep[i.var1.val,] - min(pdep$par.dep[i.var1.val,])
    }
    pdep.lim <- c(0, max(pdep$par.dep))
  }
  
  ## Is either var a factor?
  if (is.factor(x[,var1]) || length(var1.levels) > 0) {
    is.var1.fact <- TRUE
  } else {
    is.var1.fact <- FALSE
  }
  if (is.factor(x[,var2]) || length(var2.levels) > 0) {
    is.var2.fact <- TRUE
  } else {
    is.var2.fact <- FALSE
  }
  
  ## Plot partial dependence according to variable type
  ## Figures arranged in nrow rows and 2 columns
  if (is.var1.fact) {
    nrow <- ceiling(num.var1.vals/2)
    oldparams <- par(mfrow=c(nrow, 2))
    if (is.var2.fact) {
      ## Both variables are factors
      if (length(var2.levels) > 0) {
        var2.names <- var2.levels
      } else {
        var2.names <- pdep$var2.vals
      }
      for (i.var1.val in 1:num.var1.vals) {
        barplot(pdep$par.dep[i.var1.val,], names = var2.names, 
                xlab = var2, ylab = 'Partial dependence',
                main = paste0(var1, " = ", ifelse(length(var1.levels)>0, var1.levels[i.var1.val], as.character(pdep$var1.vals[i.var1.val]))),
                ylim = pdep.lim, cex.names = 0.75, las = var.levels.las)
      }
    } else {
      ## Var1 is a factor; var2 is continuous
      for (i.var1.val in 1:num.var1.vals) {
        ymin <- min(pdep$par.dep[i.var1.val,])
        ymax <- max(pdep$par.dep[i.var1.val,])
        plot(c(min(pdep$var2.vals), max(pdep$var2.vals)), c(0,0), ylim = c(ymin, ymax),
             main = paste0(var1, " = ", ifelse(length(var1.levels)>0, var1.levels[i.var1.val], as.character(pdep$var1.vals[i.var1.val]))),
             xlab = var2, ylab = 'Partial dependence', type = 'l', lty = 3)
        lines(pdep$var2.vals, pdep$par.dep[i.var1.val,], type='l', col='red')
      }
    }
    par(oldparams)
  } else {
    if (is.var2.fact) {
      ## Var1 is continuous; var2 is a factor
      nrow <- ceiling(num.var2.vals/2)
      oldparams <- par(mfrow=c(nrow, 2))
      if (length(var2.levels) > 0) {
        var2.names <- var2.levels
      } else {
        var2.names <- pdep$var2.vals
      }
      for (i.var2.val in 1:num.var2.vals) {
        ymin <- min(pdep$par.dep[,i.var2.val])
        ymax <- max(pdep$par.dep[,i.var2.val])
        plot(c(min(pdep$var1.vals), max(pdep$var1.vals)), c(0,0), ylim = c(ymin, ymax),
             main = paste0(var2, " = ", ifelse(length(var2.levels)>0, var2.levels[i.var2.val], as.character(pdep$var2.vals[i.var2.val]))),
             xlab = var1, ylab = 'Partial dependence', type = 'l', lty = 3)
        lines(pdep$var1.vals, pdep$par.dep[,i.var2.val], type = 'l', col = 'red')
      }
      par(oldparams)
    } else {
      persp(pdep$par.dep, xlab = var1, ylab = var2, zlab = 'Partial dependence', 
            col = 'blue', ticktype = 'detailed', shade = 0.5, ltheta = 30, lphi = 15)
    }
  }
  title(main)
}

PairInteract <- function(var1, var2, x, var1.levels=NULL, var2.levels=NULL, seed=135711)
{
  # Computes two-variable interaction strength.
  #
  # Args:
  #    var1: variable identifier for the first variable to be plotted
  #    var2: variable identifier for the second variable to be plotted
  #       x: training data
  #    seed: random number seed (for sampling from x)

  ## Compute the single and pair (centered) partial dependences
  ## X1
  set.seed(seed)
  pdep.var1 <- ComputeSinglePDep(var1, x, var.levels = var1.levels)
  pdep.var1$par.dep <- pdep.var1$par.dep - mean(pdep.var1$par.dep)
  num.var1.vals <- length(pdep.var1$var.vals)
  ## X2
  set.seed(seed)
  pdep.var2 <- ComputeSinglePDep(var2, x, var.levels = var2.levels)
  pdep.var2$par.dep <- pdep.var2$par.dep - mean(pdep.var2$par.dep)
  num.var2.vals <- length(pdep.var2$var.vals)
  ## X1, X2
  set.seed(seed)
  pdep.var1.var2 <- ComputePairPDep(var1, var2, x, var1.levels = var1.levels, var2.levels = var2.levels)
  pdep.var1.var2$par.dep <- pdep.var1.var2$par.dep - mean(pdep.var1.var2$par.dep)
  
  ## Compute interaction statistic
  accum.num = 0
  accum.den = 0
  for (i.var1.val in 1:num.var1.vals) {
    for (i.var2.val in 1:num.var2.vals) {
      accum.num =  accum.num + (pdep.var1.var2$par.dep[i.var1.val, i.var2.val] -
                                  pdep.var1$par.dep[i.var1.val] - pdep.var2$par.dep[i.var2.val])^2
      accum.den = accum.den + (pdep.var1.var2$par.dep[i.var1.val, i.var2.val])^2
    }
  }
  return(accum.num / accum.den)
}

PairInteract2 <- function(var1, var2, x, var1.levels=NULL, var2.levels=NULL)
{
  # Computes two-variable interaction strength.
  #
  # Args:
  #    var1: variable identifier for the first variable to be plotted
  #    var2: variable identifier for the second variable to be plotted
  #       x: training data
  
  ## Compute the centered partial dependences
  ## X1
  set.seed(1357)
  pdep.var1 <- ComputeSinglePDep(var1, x, var.levels = var1.levels)
  pdep.var1$par.dep <- pdep.var1$par.dep - mean(pdep.var1$par.dep)
  num.var1.vals <- length(pdep.var1$var.vals)
  ## X2
  set.seed(1357)
  pdep.var2 <- ComputeSinglePDep(var2, x, var.levels = var2.levels)
  pdep.var2$par.dep <- pdep.var2$par.dep - mean(pdep.var2$par.dep)
  num.var2.vals <- length(pdep.var2$var.vals)
  ## X1, X2
  set.seed(1357)
  pdep.var1.var2 <- ComputePairPDep(var1, var2, x, var1.levels = var1.levels, var2.levels = var2.levels)
  pdep.var1.var2$par.dep <- pdep.var1.var2$par.dep - mean(pdep.var1.var2$par.dep)
  
  ## Compute interaction statistic
  accum.num = 0
  accum.den = 0
  for (i in 1:nrow(x)) {
    i.var1.val = which(pdep.var1$var.vals == x[i, var1])
    i.var2.val = which(pdep.var2$var.vals == x[i, var2])
    accum.num =  accum.num + (pdep.var1.var2$par.dep[i.var1.val, i.var2.val] -
                                pdep.var1$par.dep[i.var1.val] - pdep.var2$par.dep[i.var2.val])^2
    accum.den = accum.den + (pdep.var1.var2$par.dep[i.var1.val, i.var2.val])^2
  }
  return(accum.num / accum.den)
}

PlotPairInteract <- function(var1, vars2, x, var1.levels=NULL, vars2.levels=NULL, seed=135711)
{
  # Generates plot of two-variable interaction strengths of given variable with selected 
  # other variables.
  #
  # Args:
  #               var1: variable identifier for the target variable to be plotted
  #              vars2: list of variable identifiers for the other variables to be plotted
  #                  x: training data
  #        var1.levels: var1 level names (if var1 is categorical)
  #       vars2.levels: list of var2 level names (if var2 is categorical)
  two.var.int <- c()
  for (iVar2 in 1:length(vars2)) {
    if (length(vars2.levels) > 0) {
      var2.levels <- vars2.levels[[iVar2]]
    } else {
      var2.levels <- NULL
    }
    two.var.int[iVar2] <- PairInteract(var1, vars2[iVar2], x, var1.levels, var2.levels, seed)
  }
  names(two.var.int) <- vars2
  barplot(two.var.int, ylab  = paste0("Interaction strength with ", var1), names = vars2, cex.names = 0.75)
  return(two.var.int)
}

##############
## Main
#

# Grab command-line arguments
args.m <- matrix(c(
        'conf'        ,'c', 1, "character",
        'log'         ,'l', 1, "character",
        'help'        ,'h', 0, "logical"
    ), ncol=4,byrow=TRUE)
opt <- getopt(args.m)
conf <- ValidateCmdArgs(opt, args.m)

# Set global env variables required by RuleFit
platform <- conf$rf.platform
RF_HOME <- Sys.getenv("RF_HOME")
RF_WORKING_DIR <- conf$rf.working.dir

# Create logging object
logger <- new("logger", log.level = conf$log.level, file.name = conf$log.fname)
info(logger, paste("rfPardep_main args:", 'model.path =', conf$model.path,
                   ', log.level =', conf$log.level, ', out.fname =', conf$out.fname))

# Use own version of png() if necessary:
if (isTRUE(conf$graph.dev == "Bitmap")) {
  png <- png_via_bitmap
  if (!CheckWorkingPNG(png)) error(logger, "cannot generate PNG graphics")
} else {
  png <- GetWorkingPNG()
  if (is.null(png)) error(logger, "cannot generate PNG graphics")
}

# Load & restore model
mod <- LoadModel(conf$model.path)
ok <- 1
tryCatch(rfrestore(mod$rfmod, mod$x, mod$y, mod$wt), error = function(err){ok <<- 0})
if (ok == 0) {
  error(logger, "rfPardep_main.R: got stuck in rfrestore")
}
rf.mode <- ifelse(length(unique(mod$y)) == 2, "class", "regress")
x.levels <- ReadLevels(file.path(conf$model.path, kMod.x.levels.fname))
var.levels <- Filter(function(x) {x$var == conf$var.name}, x.levels)[[1]]$levels
  
# Check desired var is in columns used to build model
if (!(conf$var.name %in% colnames(mod$x))) {
  error(logger, paste("rfPardep_main.R: don't know about", conf$var.name))
}

# Generate partial dependence plot
kPlotWidth <- 620
kPlotHeight <- 480
png(file = file.path(conf$out.path, conf$out.fname), width = kPlotWidth, height = kPlotHeight)
SinglePlot(conf$var.name, mod$x, var.levels = var.levels, var.levels.las = conf$var.levels.las,
           max.var.vals = conf$var.num.values, sample.size = conf$num.obs,
           qntl = conf$var.trim.qntl, rugqnt = conf$var.rug.qntl,
           show.pdep.dist = conf$show.pdep.dist, var.boxplot.range = conf$var.boxplot.range,
           show.yhat.mean = conf$show.yhat.mean)
dev.off()

q(status=0)
