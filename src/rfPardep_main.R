#!/usr/bin/Rscript

###############################################################################
# FILE: rfPardep_main.R
#
# USAGE:  rfPardep_main.R -c PARDEP.conf
#
# DESCRIPTION:
#       Computes partial dependence plot from a fitted RuleFit model.
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

SinglePlot <- function(var, x, max.var.vals=40, sample.size=500, qntl=0.025, rugqnt=0.1, main="",
                       var.levels=NULL, var.boxplot.range=1.0e-4, var.levels.las=1, show.pdep.dist=FALSE)
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
  #    show.pdep.dist: show partial dependence distribution
  
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

  ## Compute partial dependence
  y.hat.m <- matrix(nrow=sample.size, ncol=num.var.vals)
  for (i.var.val in 1:num.var.vals) {
     ## Hold x[,var] constant
     x.sample[,var] <- var.vals[i.var.val]
     ## Compute avg(y.hat) over selected random sample
     y.hat.m[,i.var.val] <- rfpred(x.sample)
     par.dep[i.var.val] <- mean(y.hat.m[,i.var.val])
  }
  mean.y.hat <- mean(y.hat.m)
  
  if (is.fact) {
    ## Adjust bar widths according to var's density
    bar.widths <- table(x[,var])/length(x[,var]) 
    if (show.pdep.dist) {
      boxplot(y.hat.m, names=switch(length(var.levels)>0, var.levels, as.character(var.vals)),
              width=bar.widths, xlab=var, ylab='Partial dependence',
              outline=FALSE, range=var.boxplot.range)
      points(1:num.var.vals, par.dep, col='blue') ## indicate averages
      if (show..dist) {
      lines(c(0, ncol(y.hat.m)+1), c(mean.y.hat, mean.y.hat), col='blue', lty=3)
    } else {
      barplot(par.dep, names=switch(length(var.levels)>0, var.levels, as.character(var.vals)), 
             width=bar.widths, xlab=var, ylab='Partial dependence', cex.names=0.75, las=var.levels.las)
   }
  } else {
    if (show.pdep.dist) {
      ## Add boxplot stats (lower whisker, lower hinge, median, upper hinge, upper whisker) 
      ## where
      ##   lower whisker = max(min(x), Q_1 - 1.5 * IQR) 
      ##   upper whisker = min(max(x), Q_3 + 1.5 * IQR) 
      var.vals.bp <- boxplot(y.hat.m, at=var.vals, plot=F)
      ymin <- min(var.vals.bp$stats)
      ymax <- max(var.vals.bp$stats)
      plot(c(min(var.vals), max(var.vals)), c(0,0), ylim=c(ymin, ymax), xlab=var, ylab='Partial dependence', 
           type='l', lty=3, col='white')
      matlines(var.vals, t(var.vals.bp$stats), lty=c(3,2,1,2,3), col=c('black','black','red','black','black'))
      lines(c(0, max(var.vals)), c(mean.y.hat, mean.y.hat), col='blue', lty=3)
    } else {
      ymin <- min(par.dep)
      ymax <- max(par.dep)
      plot(c(min(var.vals), max(var.vals)), c(0,0), ylim=c(ymin, ymax), xlab=var, ylab='Partial dependence', type='l', lty=3)
      lines(var.vals, par.dep, type='l', col='red')
    }
    ## Add var's density rug at top
    axis(3, quantile(x[,var], probs=seq(qntl, 1-qntl, rugqnt)), labels=F)
  }
  title(main)
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
           show.pdep.dist = conf$show.pdep.dist, var.boxplot.range = conf$var.boxplot.range)
dev.off()

q(status=0)
