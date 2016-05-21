#!/usr/local/bin/Rscript

###############################################################################
# FILE: rfPredict_main.R
#
# USAGE:  rfPredict_main.R -m <model.dir> -d DATA.conf
#
# DESCRIPTION:
#       Computes predictions from a fitted RuleFit model.
#
# ARGUMENTS:
#       model.dir: path to RuleFit model exported files
#       DATA.conf: specifies test data location.
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
source(file.path(REGO_HOME, "/lib/RuleFit/rulefit.r"))
source(file.path(REGO_HOME, "/src/rfGraphics.R"))
source(file.path(REGO_HOME, "/src/rfRulesIO.R"))
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

  ## Must have a valid data source type
  stopifnot("data.source.type" %in% names(conf))
  stopifnot(conf$data.source.type %in% c("csv", "db", "rdata"))
  if (conf$data.source.type == "db") {
    stopifnot("db.dsn" %in% names(conf) && "db.name" %in% names(conf) &&
              "db.type" %in% names(conf) && "db.tbl.name" %in% names(conf))
  } else if (conf$data.source.type == "csv") {
    stopifnot("csv.path" %in% names(conf) && "csv.fname" %in% names(conf))
    if ("csv.sep" %in% names(conf)) {
      conf$csv.sep <- as.character(conf$csv.sep)
    } else {
      conf$csv.sep <- ","
    }
  } else {  # rdata
    stopifnot(c("rdata.path", "rdata.fname") %in% names(conf))
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
  
  ## Did user specified an output file name?
  if (is.null(conf$out.fname)) {
    conf$out.fname <- "rfPredict_out.csv"
  }
  ## ... field separator?
  if (is.null(conf$out.sep)) {
    conf$out.sep <- ","
  } else if (nchar(conf$out.sep) == 2 && (conf$out.sep == paste("\\", "t", sep=""))) {
    conf$out.sep <- "\t"
  }
  
  ## Generate plots? 
  if (is.null(conf$graph.plots.ROC)) {
    conf$graph.plots.ROC <- TRUE
  } else {
    conf$graph.plots.ROC <- (as.numeric(conf$graph.plots.ROC) == 1)
  }
  ## ... LIFT, Gain, etc. plots?
  if (is.null(conf$graph.plots.extra)) {
    conf$graph.plots.extra <- FALSE
  } else {
    conf$graph.plots.extra <- (as.numeric(conf$graph.plots.extra) == 1)
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
  kUsageString <- "/path/to/rfPredict_main.R -m <model dir> -d <Data configuration file>"
  
  # Validate command line arguments
  if ( !is.null(opt$help) || is.null(opt$model_path) || is.null(opt$data_conf) ) {
    self <- commandArgs()[1]
    cat("Usage: ", kUsageString, "\n")
    q(status=1);
  }

  # Read config file (two columns assumed: 'param' and 'value')
  tmp <- read.table(opt$data_conf, header=T, as.is=T)
  conf <- as.list(tmp$value)
  names(conf) <- tmp$param
  conf <- ValidateConfigArgs(conf)

  # Check Model path
  if (!(file.exists(opt$model_path))) {
    stop("Didn't find model directory:", opt$model_path, "\n")
  } else {
    conf$model.path <- opt$model_path
  }
  
  # Do we have a log file name? "" will send messages to stdout
  if (is.null(opt$log)) {
    opt$log <- ""
  }
  conf$log.fname <- opt$log
  
  return(conf)
}

CheckFactorsEncoding  <- function(x.test, x.train.levels, x.train.levels.lowcount=NULL)
{
  # Check that the integer codes given to factors in x.test are the same as the ordering
  # used when the model was built. Otherwise, when the RuleFit::rfpred() function casts  
  # the data frame to matrix, a different ordering would lead to incorrect predictions.
  #
  # Args:
  #                  x.test : data frame
  #          x.train.levels : {<var.name, low count levels>} list
  # x.train.levels.lowcount : {<var.name, low count levels>} df (if exists)
  # Returns:
  #      A copy of the given test data frame with transformed columns
  for (iVar in 1:length(x.train.levels)) {
    var.levels.train <- x.train.levels[[iVar]]$levels
    # Was iVar a factor at train time? 
    if (!(is.null(var.levels.train))) {
      factor.name <- x.train.levels[[iVar]]$var
      factor.vals <- as.character(x.test[, factor.name])
      
      # Were there low-count levels at train time? If so, replace them in x.test too
      if (!is.null(x.train.levels.lowcount)) {
        i.recoded.var <- grep(paste("^", factor.name, "$", sep=""), x.train.levels.lowcount$var, perl=T)
        if (length(i.recoded.var) > 0) {
          warn(logger, paste("CheckFactorsEncoding: replacing low-count levels for '", factor.name))
          low.count.levels <- unlist(x.train.levels.lowcount$levels[i.recoded.var])
          factor.vals <- ifelse(factor.vals %in% low.count.levels, kLowCountLevelsName, factor.vals)
        }
      }
      # Check for presence of new levels and replace them with NA (if any)
      levels.diff <- setdiff(unique(factor.vals), var.levels.train)
      if (length(levels.diff) > 0) {
        warn(logger, paste("CheckFactorsEncoding: new levels found for '", factor.name, "' : ", 
                           paste(lapply(levels.diff, sQuote), collapse=","),
                           "; replacing with NA"))
        factor.vals <- ifelse(factor.vals %in% levels.diff, NA, factor.vals)
      }
      
      # Lastly, make sure we have the same level ordering
      x.test[, factor.name] <- factor(factor.vals, levels = var.levels.train)
    }
  }
  return(x.test)
}

##############
## Main
#

# Grab command-line arguments
args.m <- matrix(c(
        'model_path'  ,'m', 1, "character",
        'data_conf'   ,'d', 1, "character",
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
info(logger, paste("rfPredict_main args:", 'model.path =', conf$model.path,
                   ', log.level =', conf$log.level, ', out.fname =', conf$out.fname))

## Use own version of png() if necessary:
if (isTRUE(conf$graph.dev == "Bitmap")) {
  png <- png_via_bitmap
  if (!CheckWorkingPNG(png)) error(logger, "cannot generate PNG graphics")
} else {
  png <- GetWorkingPNG()
  if (is.null(png)) error(logger, "cannot generate PNG graphics")
}

# Load data
if (conf$data.source.type == "db") {
  error(logger, paste("rfPredict_main.R: not yet implemented data source type ", conf$data.source.type))
} else if (conf$data.source.type == "csv") {
  data <- read.csv(file.path(conf$csv.path, conf$csv.fname), na.strings = "", sep=conf$csv.sep, check.names = FALSE)
} else if (conf$data.source.type == "rdata") {
  envir <- new.env()
  load(file.path(conf$rdata.path, conf$rdata.fname), envir = envir)
  if (is.null(conf$rdata.dfname)) {
    dfname <- ls(envir)
    stopifnot(length(dfname) == 1)
  } else {
    dfname <- conf$rdata.dfname
  }
  data <- get(dfname, envir, inherits = FALSE)
  stopifnot(is.data.frame(data))
  rm(envir)
} else {
  error(logger, paste("rfPredict_main.R: unknown data source type ", conf$data.source.type))
}
info(logger, paste("Data loaded: dim =", nrow(data), "x", ncol(data), "; NAs =",
                   length(which(is.na(data) == T)), "(",
                   round(100*length(which(is.na(data) == T))/(nrow(data)*ncol(data)), 2),
                   "%)"))

# Load & restore model
mod <- LoadModel(conf$model.path)
ok <- 1
tryCatch(rfrestore(mod$rfmod, mod$x, mod$y, mod$wt), error = function(err){ok <<- 0})
if (ok == 0) {
  error(logger, "rfPredict_main.R: got stuck in rfrestore")
}
rf.mode <- ifelse(length(unique(mod$y)) == 2, "class", "regress")

# Extract columns used to build model
ok <- 1
tryCatch(x.test <- data[,colnames(mod$x)], error = function(err){ok <<- 0})
if (ok == 0) {
  error(logger, "rfPredict_main.R: train/test column mismatch")
} 

# Any preprocessing needed?
# ... Ensure factor levels are encoded in the same order used at model building time
# ... and substitute low-count levels (if appropriate)
x.levels.fname <- file.path(conf$model.path, kMod.x.levels.fname)
x.levels <- ReadLevels(x.levels.fname)
x.levels.lowcount.fname <- file.path(conf$model.path, kMod.x.levels.lowcount.fname)
if (file.exists(x.levels.lowcount.fname)) {
  x.levels.lowcount <- as.data.frame(do.call("rbind", ReadLevels(x.levels.lowcount.fname)))
  x.test <- CheckFactorsEncoding(x.test, x.levels, x.levels.lowcount)
} else {
  x.test <- CheckFactorsEncoding(x.test, x.levels)
}

# Predict
y.hat <- rfpred(x.test)
if (rf.mode == "class") {
  # "classification" model... convert from log-odds to probability estimates
  y.hat <- 1.0/(1.0+exp(-y.hat))
}

# Compute test error (if y is known)
if ("col.y" %in% names(conf)) {
  y <- data[,conf$col.y]
  if (rf.mode == "class") {
    conf.m <- table(y, sign(y.hat - 0.5))
    stopifnot("0" %in% rownames(conf.m))
    stopifnot("1" %in% rownames(conf.m))
    TN <- ifelse("-1" %in% colnames(conf.m), conf.m["0", "-1"], 0)
    FP <- ifelse("1" %in% colnames(conf.m), conf.m["0","1"], 0)
    FN <- ifelse("-1" %in% colnames(conf.m), conf.m["1", "-1"], 0)
    TP <- ifelse("1" %in% colnames(conf.m), conf.m["1","1"], 0)    
    test.acc <- 100*(TN+TP)/length(y.hat)
    info(logger, paste("Test acc:", round(test.acc, 2)))
    info(logger, sprintf("Test confusion matrix - 0/0: %d, 0/1: %d, 1/0: %d, 1/1: %d",
                         TN, FP, FN, TP))
    # AUC
    pred <- prediction(y.hat, y)
    perf <- performance(pred, "auc")
    info(logger, paste("Area under the ROC curve:", as.numeric(perf@y.values)))

    # Generate ROC plot
    if (conf$graph.plots.ROC) {
      kPlotWidth <- 620
      kPlotHeight <- 480
      plot.fname <- "ROC.png"
      pred <- prediction(y.hat, y)
      perf <- performance(pred, "tpr", "fpr")
      png(file = file.path(conf$out.path, plot.fname), width=kPlotWidth, height=kPlotHeight)
      plot(perf, colorize=T, main="")
      lines(x=c(0, 1), y=c(0,1))
      dev.off()
    }
    # Generate extra plots
    if (conf$graph.plots.extra) { 
      # Generate LIFT plot
      perf <- performance(pred,"lift","rpp")
      plot.fname <- "LIFT.png"
      png(file = file.path(conf$out.path, plot.fname), width=kPlotWidth, height=kPlotHeight)
      plot(perf, main="Lift curve", colorize=T)
      lines(x=c(0, 1), y=c(1,1))
      dev.off()
    
      # Generate Cumulative Gains plot
      perf <- performance(pred,"tpr","rpp")
      plot.fname <- "Gains.png"
      png(file = file.path(conf$out.path, plot.fname), width=kPlotWidth, height=kPlotHeight)
      plot(perf, main="Cumulative Gains curve", colorize=T)
      lines(x=c(0, 1), y=c(0,1))
      dev.off()

      # Score histogram colored by y
      library(ggplot2)
      plot.fname = "score_hist1.png"
      hist.yresult <- data.frame(x = y.hat, category = factor(y))
      p <- ggplot(hist.yresult, aes(x, colour = category))
      p <- p + geom_freqpoly()
      p <- p + xlab('predicted score colored by true category')
      ggsave(file.path(conf$out.path, plot.fname), width = kPlotWidth/100, height = kPlotHeight/100, units = "in")

      # Score histogram (stacked)
      library(ggplot2)
      plot.fname = "score_hist2.png"
      p <- ggplot(hist.yresult, aes(x, fill = category))
      p <- p + geom_bar(position = "fill") + stat_bin(binwidth=0.1)
      p <- p + xlab('predicted score colored by true category') + ylab('percentage')
      ggsave(file.path(conf$out.path, plot.fname), width = kPlotWidth/100, height = kPlotHeight/100, units = "in")
    }
  } else {
    re.test.error <- sum(abs(y.hat - y))/nrow(x.test)
    med.test.error <- sum(abs(y - median(y)))/nrow(x.test)
    aae.test <- re.test.error / med.test.error
    info(logger, sprintf("Test AAE: %f (RE:%f, Med:%f)", aae.test, re.test.error, med.test.error))
  }  
} else {
  y <- rep(NA, nrow(data))
}

# Plot histogram of yHat
kPlotWidth <- 620
kPlotHeight <- 480
plot.fname <- "yHat_hist.png"
png(file = file.path(conf$out.path, plot.fname), width=kPlotWidth, height=kPlotHeight)
hist(y.hat, main="")
dev.off()

# Dump <id, y, yHat> tuples, as appropriate
if ("col.id" %in% names(conf)) {
  obs.id <- data[,conf$col.id]
} else {
  obs.id <- rep(NA, nrow(data))
}
WriteObsIdYhat(out.path = conf$out.path, obs.id = obs.id, y = y, y.hat = y.hat, field.sep = conf$out.sep, file.name = conf$out.fname)

q(status=0)
