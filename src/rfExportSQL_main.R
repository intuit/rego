#!/usr/local/bin/Rscript

###############################################################################
# FILE: rfExportSQL_main.R
#
# USAGE:  rfExportSQL_main.R -m <model.dir> -c EXPORT.conf
#
# DESCRIPTION:
#       Exports a previously built RuleFit model to SQL -- i.e., generates a 
# SQL expression corresponding to the scoring function defined by the model.
#
# ARGUMENTS:
#       model.dir: path to RuleFit model exported files
#     EXPORT.conf: the configuration file specifying export options such as
#                  desired sql dialect, type of scoring clause, etc.
#
# REQUIRES:
#       REGO_HOME: environment variable pointing to the directory where you 
#                  have placed this file (and its companion ones)
#           
# AUTHOR: Giovanni Seni <Giovanni_Seni@intuit.com> 
###############################################################################
REGO_HOME <- Sys.getenv("REGO_HOME")
source(file.path(REGO_HOME, "/src/logger.R"))
source(file.path(REGO_HOME, "/src/rfExportSQL.R"))
library(getopt)

ValidateConfigArgs <- function(conf)
{
  # Validates and initializes configuration parameters.
  #
  # Args:
  #      conf: A list of <param name, param value> pairs
  # Returns:
  #   A list of <param name, param value> pairs
  
  # SQl variant?
  if (is.null(conf$sql.dialect)) {
    conf$sql.dialect <- "SQLServer"
  } else {
    stopifnot(conf$sql.dialect %in% c("SQLServer", "HiveQL", "Netezza", "MySQL"))
  }
  
  # Do we need to dedup rule set?
  if (is.null(conf$do.dedup)) {
    conf$do.dedup <- TRUE
  } else {
    conf$do.dedup <- (as.numeric(conf$do.dedup) == 1)
  }
  
  # How do we expand low-count levels?
  #  1: replace _LowCountLevels_ in SQL scoring expression with corresponding levels
  #  2: keep _LowCountLevels_ in SQL scoring expression (keeps it shorter, easier to read)
  #     and generate an extra sql clause with logic to substitute low count levels with
  #     _LowCountLevels_ in a data preparation step prior to scoring
  if (is.null(conf$expand.lcl.mode)) {
    conf$expand.lcl.mode <- 1
  } else {
    conf$expand.lcl.mode <- as.numeric(conf$expand.lcl.mode)
  }
  
  # log level?
  if (is.null(conf$log.level)) {
    conf$log.level <- kLogLevelINFO
  } else {
    conf$log.level <- get(conf$log.level)
  }

  # Output type?
  if (is.null(conf$out.type)) {
    conf$out.type <- "score"
  } else {
    stopifnot(conf$out.type %in% c("score", "rulesonly", "rulesscore", "rulescoeff"))
  }

  # Max sql expression length (in number of characters)
  if (is.null(conf$max.sql.length)) {
    conf$max.sql.length <- 500
  }

  # Levels file?
  if (is.null(conf$in.fname)) {
    conf$levels.fname <- "xtrain_levels.txt"
  }

  # Output file?
  if (is.null(conf$out.path)) {
    conf$out.path <- conf$model_path
  }
  if (is.null(conf$out.fname)) {
    conf$out.fname <- "rules_forSQL.txt"
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
  kUsageString <- "/path/to/rfExportSQL_main.R -m <model dir> [-c <config file with export options>] [-l <Log file name>]"

  # Validate command line arguments
  if ( !is.null(opt$help) || is.null(opt$model_path) ) {
    self <- commandArgs()[1]
    cat("Usage: ", kUsageString, "\n")
    q(status=1);
  }

  # Do we have export options? If so, read them from given file (two columns assumed: 'param' and 'value')
  if (is.null(opt$export_conf)) {
    conf <- list()
  } else {
    tmp <- read.table(opt$export_conf, header=TRUE, as.is=T)
    conf <- as.list(tmp$value)
    names(conf) <- tmp$param
  }
  conf <- ValidateConfigArgs(conf)

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

##############
## Main
#

# Grab command-line arguments
args.m <- matrix(c(
        'model_path'  ,'m', 1, "character",
        'export_conf' ,'c', 1, "character",
        'log'         ,'l', 1, "character",
        'help'        ,'h', 0, "logical"
    ), ncol=4,byrow=TRUE)
opt <- getopt(args.m)
conf <- ValidateCmdArgs(opt, args.m)

# Create logging object
logger <- new("logger", log.level = conf$log.level, file.name = conf$log.fname)
info(logger, paste("rfExportSQL_main args:", 'model.path =', conf$model.path, ', do.dedup =', conf$do.dedup,
                   ', expand.lcl.mode =', conf$expand.lcl.mode, ', sql =', conf$sql.dialect, ', log.level =', conf$log.level,  
                   ', out.type =', conf$out.type, ', out.fname =', conf$out.fname, ', max.sql.length =', conf$max.sql.length))

# Run export
ExportModel2SQL(model.path = conf$model.path, merge.dups = conf$do.dedup, expand.lcl.mode = conf$expand.lcl.mode, 
                db.type = conf$sql.dialect, export.type = conf$out.type, levels.fname = conf$levels.fname,
                out.path = conf$out.path, out.fname = conf$out.fname,
                max.sql.length = conf$max.sql.length)

q(status=0)
