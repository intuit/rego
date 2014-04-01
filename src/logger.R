###############################################################################
# Simple logging class.
#
# Usage:
#   REGO_HOME = Sys.getenv("REGO_HOME")
#   source(file.path(REGO_HOME, "src/logger.R"))
#   logger <- new("logger")
#   OR logger <- new("logger", file.name="foo.log")
#   OR logger <- new("logger", log.level = kLogLevelWARNING, file.name="foo.log")
#   info(logger, "hello")
#   warn(logger, "world")
# 
# Author: Giovanni Seni <Giovanni_Seni@intuit.com> 
###############################################################################
library(methods) # not automatically loaded by Rscript

# Standard logging levels that can be used to control logging output (borrowed
# definitions from python)
kLogLevelNOTSET   <- 0
kLogLevelDEBUG    <- 10
kLogLevelINFO     <- 20
kLogLevelWARNING  <- 30
kLogLevelERROR    <- 40
kLogLevelFATAL    <- 50
kLogLevelCRITICAL <- 50

# Logging class and functions
setClass("logger",
         representation(file.name = "character",
                        log.level = "numeric"),
         prototype = list(file.name = "",
                          log.level = kLogLevelDEBUG))
                  
setMethod("initialize", "logger",
    def = function(.Object) {
      .Object@file.name <- ""
      .Object@log.level <- kLogLevelDEBUG
      .Object
    }
)
                  
setMethod("initialize", "logger",
    def = function(.Object, file.name) {
      if (file.exists(file.name)) {
        stop(paste("Log file '", file.name, "' already exists", sep=""),
            call. = FALSE)
      }
      .Object@file.name <- file.name
      .Object@log.level <- kLogLevelDEBUG
      .Object
    }
)

setMethod("initialize", "logger",
          def = function(.Object, file.name, log.level) {
            if (file.exists(file.name)) {
              stop(paste("Log file '", file.name, "' already exists", sep=""),
                   call. = FALSE)
            }            
            .Object@file.name <- file.name
            .Object@log.level <- log.level
            .Object
          }
)

setGeneric("loggerWrite",
           def = function(this, level, msg, levelName) {
             standardGeneric("loggerWrite")
           },
           useAsDefault = function(this, level, msg, levelName) {
             ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
             if (level >= this@log.level) {
               write(paste(ts, ' ', levelName, ' - ', msg, sep=''),
                     file = this@file.name, append = T)
             }
           }
)

setGeneric("dbg",
           def = function(this, msg) {
             standardGeneric("dbg")
           },
           useAsDefault = function(this, msg) {
             loggerWrite(this, kLogLevelDEBUG, msg, " DEBUG")
           }
)

setGeneric("info",
           def = function(this, msg) {
             standardGeneric("info")
           },
           useAsDefault = function(this, msg) {
             loggerWrite(this, kLogLevelINFO, msg, " INFO")
           }
)

setGeneric("warn",
           def = function(this, msg) {
             standardGeneric("warn")
           },
           useAsDefault = function(this, msg) {
             loggerWrite(this, kLogLevelWARNING, msg, " WARN")
           }
)

setGeneric("error",
           def = function(this, msg) {
             standardGeneric("error")
           },
           useAsDefault = function(this, msg) {
             loggerWrite(this, kLogLevelERROR, msg, " ERROR")
             stop("Ending program...", call. = FALSE)
           }
)
