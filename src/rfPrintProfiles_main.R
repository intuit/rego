#!/usr/local/bin/Rscript

###############################################################################
# FILE: printProfiles.R
#
# USAGE: printProfiles.R -c printProfiles.cfg
#
# DESCRIPTION:
#     Prints a summary of the training data population (for a given RF model)
# for the "extreme" values of yHat.
###############################################################################
REGO_HOME <- Sys.getenv("REGO_HOME")
source(file.path(REGO_HOME, "/src/rfRulesIO.R"))
source(file.path(REGO_HOME, "/src/rfExport.R"))
source(file.path(REGO_HOME, "/src/rfGraphics.R"))
library(getopt)
kPlotWidth <- 620
kPlotHeight <- 480

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
  kUsageString <- "/path/to/printProfiles.R -c printProfiles.cfg"

  # Validate command line arguments
  if ( !is.null(opt$help) || is.null(opt$conf)) {
    self <- commandArgs()[1];
    cat("Usage: ", kUsageString, "\n");
    q(status=1);
  }
  
  # Read config file (two columns assumed: 'param' and 'value')
  tmp <- read.table(opt$conf, header=T, as.is=T)
  conf <- as.list(tmp$value)
  names(conf) <- tmp$param

  # Must have a valid model and output path
  stopifnot("mod.path" %in% names(conf))
  stopifnot("out.path" %in% names(conf))

  # Set defaults for the options that were not specified
  if (!("very.unlikely.thresh" %in% names(conf))) {
    conf$v.u.thresh <- -1.0
  } else {
    conf$v.u.thresh <- as.numeric(conf$very.unlikely.thresh)
  }
  if (!("very.likely.thresh" %in% names(conf))) {
    conf$v.l.thresh <- 1.0
  } else {
    conf$v.l.thresh <- as.numeric(conf$very.likely.thresh)
  }
  
  if (!("html.min.var.imp" %in% names(conf))) {
    conf$html.min.var.imp <- 5
  } else {
    conf$html.min.var.imp <- as.numeric(conf$html.min.var.imp)
  }
  if (!("yHat.hist.fname" %in% names(conf))) {
    conf$yHat.hist.fname <- "yHat.png"
  }
  if (!("yHat.hist.title" %in% names(conf))) {
    conf$yHat.hist.title <- ""
  }

  return(conf)
}

PrintSegmentProfile <- function(x, x.levels, i.segment, vars2print=NULL)
{
  if (is.null(vars2print)) {
    vars2print <- colnames(x)
  } else {
    nvars <- length(vars2print)
    if (length(intersect(vars2print, colnames(x))) != nvars) {
      error(logger, "PrintSegmentProfile: variable name mismatch")
    }
  }
  
  for (var.name in vars2print) {
    cat(var.name, ":\n")
    var.x <- x[, var.name]
    i.NA <- which(var.x == kMissing)
    var.x[i.NA] <- NA
    var.levels <- NULL
    # Fetch level names (if appropriate)
    for (iVar in 1:length(x.levels)) {
      if ( x.levels[[iVar]]$var == var.name ) {
        var.levels <- x.levels[[iVar]]$levels
        break;
      }
    }
    # Print summary according to type
    if (!is.null(var.levels)) {
      # Categorical variable
      var <- as.factor(var.x)
      levels(var) <- var.levels
      tmp <- summary(var[i.segment], maxsum = 10)
      print(tmp)
      print(paste(round(100.0*summary(var[i.segment], maxsum = 10)/sum(tmp), 2), "%", sep = ""))
    } else {
      # Numeric variable
      print(summary(var.x[i.segment]))
    }
  }
}

##############
## Main

# Grab command-line arguments
args.m <- matrix(c(
        'conf'        ,'c', 1, "character",
        'help'        ,'h', 0, "logical"
    ), ncol=4,byrow=TRUE)
opt <- getopt(args.m)
conf <- ValidateCmdArgs(opt, args.m)

## Load x, y, yhat, levels
load(file.path(conf$mod.path, kMod.yHat.fname))
load(file.path(conf$mod.path, kMod.xyw.fname))
x.levels <- ReadLevels(file.path(conf$mod.path, kMod.x.levels.fname))

## Plot score histogram
plot.fname <- conf$yHat.hist.fname

## Use own version of png() if necessary:
if (isTRUE(conf$html.graph.dev == "Bitmap")) {
  png <- png_via_bitmap
  if (!CheckWorkingPNG(png)) stop("cannot generate PNG graphics")
} else {
  png <- GetWorkingPNG()
  if (is.null(png)) stop("cannot generate PNG graphics")
}

png(file = file.path(conf$out.path, plot.fname), width=kPlotWidth, height=kPlotHeight)

hist(y.hat, main=conf$yHat.hist.title, cex.main=0.8, xlab="score", breaks=22)
abline(v = conf$v.u.thresh, col = "red", lty = "dashed")
abline(v = conf$v.l.thresh, col = "blue", lty = "dashed")
dev.off()

# Which variables are to be summarized (and in what order)?
# ... read variable importance table
vi.df <- read.table(file.path(conf$mod.path, kMod.varimp.fname), header = F, sep = "\t")
colnames(vi.df) <- c("Importance", "Variable")
# ... pick subset from varimp list
# ... ... first, filter out low importance entries
min.var.imp <- max(conf$html.min.var.imp, 1.0) 
i.zero.imp <- which(vi.df$Importance < min.var.imp)
if ( length(i.zero.imp == 1) ) {
  vi.df <- vi.df[-i.zero.imp, ]
}
nvars <- min(conf$html.singleplot.nvars, nrow(vi.df))
vars2print <- vi.df$Variable[1:nvars]

## "Very Unlikely" subgroup
i.yHat.low <- which(y.hat <= conf$v.u.thresh)
cat("'Very Unlikely' subgroup:", length(i.yHat.low), "(",
    round(100*length(i.yHat.low)/nrow(x), 2), "% )\n")
## ... Accuracy
tbl <- table(y[i.yHat.low], sign(y.hat[i.yHat.low]))
print(tbl)
cat("Accuracy:", round(100*diag(tbl)/sum(tbl), 2), "\n")
## ... Profile
PrintSegmentProfile(x, x.levels, i.yHat.low, vars2print)

    
## "Very Likely" subgroup
i.yHat.hi <- which(y.hat >= conf$v.l.thresh)
cat("'Very Likely' subgroup:", length(i.yHat.hi), "(",
    round(100*length(i.yHat.hi)/nrow(x), 2), "% )\n")
## ... Accuracy
tbl <- table(y[i.yHat.hi], sign(y.hat[i.yHat.hi]))
print(tbl)
cat("Accuracy:", round(100 - 100*diag(tbl)/sum(tbl), 2), "\n")
## ... Profile
PrintSegmentProfile(x, x.levels, i.yHat.hi, vars2print)
q(status=0)
