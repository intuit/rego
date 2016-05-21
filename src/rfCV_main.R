#!/usr/local/bin/Rscript

###############################################################################
# FILE: rfCV_main.R
#
# USAGE:  rfCV_main.R -c CV.conf
#
# DESCRIPTION:
#       Cross-validate a fitted RuleFit model.
#
# ARGUMENTS:
#    CV.conf: specifies data & model definition location .
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
source(file.path(REGO_HOME, "/src/rfTrain.R"))
source(file.path(REGO_HOME, "/src/rfGraphics.R"))
source(file.path(REGO_HOME, "/lib/RuleFit/rulefit.r"))
library(ROCR, verbose = FALSE, quietly=TRUE, warn.conflicts = FALSE)
library(getopt)

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
  kUsageString <- "/path/to/rfCV_main.R -c CV.conf -d <Data configuration file> -m <RF model configuration file> [-l <Log file name>]"
  
  # Validate command line arguments
  if ( !is.null(opt$help) || is.null(opt$conf) || is.null(opt$data_conf) || is.null(opt$model_conf) ) {
    self <- commandArgs()[1]
    cat("Usage: ", kUsageString, "\n")
    q(status=1);
  }

  # Do we have a log file name? "" will send messages to stdout
  if (is.null(opt$log)) {
    opt$log <- ""
  }

  # Read config file (two columns assumed: 'param' and 'value')
  tmp <- read.table(opt$conf, header=T, as.is=T)
  conf <- as.list(tmp$value)
  names(conf) <- tmp$param
  
  # Must have a valid data source type
  stopifnot("data.source.type" %in% names(conf))
  stopifnot(conf$data.source.type %in% c("csv", "db"))
  if (conf$data.source.type == "db") {
    stopifnot("db.dsn" %in% names(conf) && "db.name" %in% names(conf) &&
              "db.type" %in% names(conf) && "db.tbl.name" %in% names(conf))
  } else {
    stopifnot("csv.path" %in% names(conf) && "csv.fname" %in% names(conf))
  }
  
  # Did user specified a log level?
  conf$log.fname <- opt$log  
  if (is.null(conf$log.level)) {
    conf$log.level <- kLogLevelINFO
  } else {
    conf$log.level <- get(conf$log.level)
  }
  
  # Did user specified an output file?
  if (is.null(conf$out.fname)) {
    conf$out.fname <- "rfCV_out.csv"
  }

  return(conf)
}


##############
## Main
#

# Grab command-line arguments
args.m <- matrix(c(
        'conf'       ,'c', 1, "character",
        'data_conf'  ,'d', 1, "character",
        'model_conf' ,'m', 1, "character",
        'log'        ,'l', 1, "character",                   
        'help'       ,'h', 0, "logical"
    ), ncol=4,byrow=TRUE)
opt <- getopt(args.m)
conf <- ValidateCmdArgs(opt, args.m)

# Create logging object
logger <- new("logger", log.level = conf$log.level, file.name = conf$log.fname)

# Read data config file (same one used to build original model)
tmp <- read.table(opt$data_conf, header=T, as.is=T)
train.data.conf <- as.list(tmp$value)
names(train.data.conf) <- tmp$param
train.data.conf <- ValidateConfigArgs(train.data.conf)

# Load model specification parameters file (same one used to build original model)
train.rf.ctxt <- IntiRFContext(opt$model_conf)

# Set global env variables required by RuleFit
platform <- conf$rf.platform
RF_HOME <- Sys.getenv("RF_HOME")
RF_WORKING_DIR <- conf$rf.working.dir

# Create logging object
logger <- new("logger", log.level = conf$log.level, file.name = "")
info(logger, paste("rfCV_main args:", 'model.path =', conf$model.path,
                   ', log.level =', conf$log.level, ', out.fname =', conf$out.fname))

## Use own version of png() if necessary:
if (isTRUE(conf$graph.dev == "Bitmap")) {
  png <- png_via_bitmap
  if (!CheckWorkingPNG(png)) error(logger, "cannot generate PNG graphics")
} else {
  png <- GetWorkingPNG()
  if (is.null(png)) error(logger, "cannot generate PNG graphics")
}

# Load x, y, w used to build original model
load(file.path(conf$mod.path, kMod.xyw.fname))
info(logger, paste("Data loaded: dim =", nrow(x), "x", ncol(mod$x), "; NAs =",
                   length(which(x == 9.0e30)), "(",
                   round(100*length(which(x == 9.0e30))/(nrow(x)*ncol(x)), 2),
                   "%)"))

# Get list of categorical variables
x.levels <- as.data.frame(do.call("rbind", ReadLevels(file.path(conf$mod.path, kMod.x.levels.fname))))
x.cat.vars <- which(sapply(x.levels[1:10,'levels'], function(x) {ifelse(length(x) > 0,1,0)}) == 1)

# Do CV
cv.out <- rfCV(x, y, wt, x.cat.vars, train.rf.ctxt, conf$nfold, conf$yHat.return, conf$seed)

# Compute CV error
if (train.rf.ctxt$task == "class") {
  # "classification" model... convert from log-odds to probability estimates
  cv.out$oos.yHat <- 1.0/(1.0+exp(-cv.out$oos.yHat))
  # Confusion matrix
  conf.m <- table(y, sign(y.hat - 0.5))
  test.acc <- 100*sum(diag(conf.m))/sum(conf.m)
  info(logger, paste("CV acc:", round(test.acc, 2)))
  info(logger, sprintf("CV confusion matrix - 0/0: %d, 0/1: %d, 1/0: %d, 1/1: %d",
                       conf.m[1, 1], conf.m[1, 2], conf.m[2, 1], conf.m[2, 2]))
    
  # Generate ROC plot
  kPlotWidth <- 620
  kPlotHeight <- 480
  plot.fname <- "ROC.png"
  pred <- prediction(y.hat, y)
  perf <- performance(pred, "tpr", "fpr")
  png(file = file.path(conf$out.path, plot.fname), width=kPlotWidth, height=kPlotHeight)
  plot(perf, colorize=T, main="")
  lines(x=c(0, 1), y=c(0,1))
  dev.off()
} else {
    re.test.error <- sum(abs(y.hat - y))/nrow(x)
    med.test.error <- sum(abs(y - median(y)))/nrow(x)
    aae.test <- re.test.error / med.test.error
    info(logger, sprintf("CV AAE: %f (RE:%f, Med:%f)", aae.test, re.test.error, med.test.error))
  }  
}

# Dump <id, y, yHat> tuples, as appropriate
if ("col.id" %in% names(conf)) {
  obs.id <- data[,conf$col.id]
} else {
  obs.id <- rep(NA, nrow(data))
}
WriteObsIdYhat(out.path = conf$out.path, obs.id = obs.id, y = y, y.hat = y.hat, file.name = conf$out.fname)

q(status=0)
