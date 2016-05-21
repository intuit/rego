source(file.path(REGO_HOME, "/src/winsorize.R"))

# Constants
kMod.fname                   <- "rfmod.Rdata"
kMod.xyw.fname               <- "xywtrain.Rdata"
kMod.x.levels.fname          <- "xtrain_levels.txt"
kMod.x.levels.lowcount.fname <- "xtrain_levels_lowcount.txt"
kMod.x.trim.fname            <- "xtrain_trim.txt"
kMod.yHat.fname              <- "xtrain_yHat.Rdata"   
kMod.varimp.fname            <- "varimp.txt"
kMod.rules.fname             <- "rules.txt"
kMod.intercept.fname         <- "intercept.txt"

GetColType <- function(cname, col.types)
{
  # Returns an integer from the vector of column types matching the given column name.
  i.col.type <- grep(paste("^", cname, "$", sep=""), names(col.types), perl=T, ignore.case=T)
  if ( length(i.col.type) == 1 ) {
    return(col.types[i.col.type])
  } else {
    return(NA)
  }
}  

WriteLevels <- function (x.df, col.types, out.fname)
{
  # Writes out "levels" for each column of type categorical in the given data.
  #
  # Args:
  #          x.df : data frame
  #     col.types : vector indicating column types -- 1:continuous
  #                 2:categorical
  #     out.fname : ouput file name
  #
  # Returns:
  #     None.
  stopifnot(is.character(out.fname))
  stopifnot(class(x.df) == "data.frame")
  stopifnot(length(col.types) >= ncol(x.df))

  # Make sure we have the appropriate quotation mark (non-directional single
  # quotation mark)
  op <- options("useFancyQuotes")
  options(useFancyQuotes = FALSE)
  
  outF <- file(out.fname, "w")
  for (i in 1:ncol(x.df)) {
    cname <- colnames(x.df)[i]
    ctype <- GetColType(cname, col.types)
    if (is.na(ctype)) {
      error(logger, paste("WriteLevels: don't know type for this var: ", cname))
    } else {
      if ( ctype == 2 ) {
        clevels <- levels(as.factor(x.df[, i]))
        cat(sQuote(cname), sQuote(clevels), file = outF, sep=",", append = T)
        cat("\n", file = outF, append = T)
      } else if ( ctype == 1 ) {
        cat(sQuote(cname), file = outF, append = T)
        cat("\n", file = outF, append = T)
      } else {
        error(logger, paste("WriteLevels: don't know about this var type: ", ctype))
      }
    }
  }
  close(outF)

  # Restore quotation mark
  options(useFancyQuotes = op)
}

WriteLevelsLowCount <- function (x.recoded.cat.vars, out.fname)
{
  # Writes out low-count "levels" for columns of type categorical in the given data.
  #
  # Args:
  #   x.recoded.cat.vars : list of <var, low-count level list> pairs
  #            out.fname : ouput file name
  #
  # Returns:
  #     None.
  stopifnot(is.character(out.fname))
  stopifnot(class(x.recoded.cat.vars) == "list")

  # Make sure we have the appropriate quotation mark (non-directional single
  # quotation mark)
  op <- options("useFancyQuotes")
  options(useFancyQuotes = FALSE)
  
  outF <- file(out.fname, "w")
  for (i.var in 1:length(x.recoded.cat.vars)) {
     var.name <- (x.recoded.cat.vars[[i.var]])$var
     var.lcount.levels <- (x.recoded.cat.vars[[i.var]])$low.count.levels
     cat(sQuote(var.name), sQuote(var.lcount.levels), file = outF, sep=",", append = T)
     cat("\n", file = outF, append = T)
  }
  close(outF)
  
  # Restore quotation mark
  options(useFancyQuotes = op)
}

WriteTrimQuantiles <- function(x.df, col.types, beta, out.fname, feat2winz)
{
  # Writes out "trim" quantiles for each column of type numeric in the given
  # data. 
  #
  # Args:
  #          x.df : data frame
  #     col.types : vector indicating column types -- 1:continuous
  #                 2:categorical
  #     out.fname : ouput file name
  #          beta : the beta and (1-beta) quantiles of the data distribution
  #                 {x_ij} for each continuous variable x_j will be written out.
  # Returns:
  #     None.
  stopifnot(is.character(out.fname)) 
  stopifnot(length(col.types) >= ncol(x.df)) 

  if (!is.na(beta)) {
    if (beta < 0 || beta > 0.5) {
      error(logger, paste("WriteTrimQuantiles: Invalid 'beta' value:", beta))
    }

    outF <- file(out.fname, "w")
    for (i in 1:ncol(x.df)) {
      cname <- colnames(x.df)[i]
      ctype <- GetColType(cname, col.types)
      if (is.na(ctype)) {
        error(logger, paste("WriteTrimQuantiles: don't know type for this var:", cname))
      } else {
        if (ctype == 1) {
          if (is.numeric(x.df[, i])) { 
            l.x <- winsorize(x.df[, i], beta)
            if (length(l.x) != 3) {
              error(logger, paste("WriteTrimQuantiles: expecting 3 values from call to winsorize with this var:", cname))
            }
            x.min2keep <- l.x$min2keep
            x.max2keep <- l.x$max2keep
            x.mean <- mean(l.x$x, na.rm = T)
            cat(sQuote(cname), x.min2keep, x.max2keep, x.mean, file = outF, sep=",", append = T)
            cat("\n", file = outF, append = T)
          } else {
            dbg(logger, paste("WriteTrimQuantiles: can't winsorize this var:", cname))
            cat(sQuote(cname), NA, NA, NA, file = outF, sep=",", append = T)
            cat("\n", file = outF, append = T)
          } 
        } else if (ctype == 2) {
          cat(sQuote(cname), NA, NA, NA, file = outF, sep=",", append = T)
          cat("\n", file = outF, append = T)
        } else {
          error(logger, paste("WriteTrimQuantiles: don't know about this var type: ", ctype))
        }
      }
    }
    close(outF)
  } else {
    # trims have already been computed
    if ( missing(feat2winz)) {
      error(logger, "WriteTrimQuantiles: 'feat2winz' must not be missing when 'beta' isn't specified")
    }
    if (class(feat2winz) != "data.frame" ||
        length(which((names(feat2winz) == c("vname", "beta", "min2keep", "max2keep", "mean"))==T)) != 5) {
      error(logger, "WriteTrimQuantiles: 'feat2winz' must not be missing when 'beta' isn't specified")
    }
    outF <- file(file, "w")
    for (i in 1:ncol(x.df)) {
      cname <- colnames(x.df)[i]
      ctype <- GetColType(cname, col.types)
      if (is.na(ctype)) {
        error(logger, paste("WriteTrimQuantiles: don't know type for this var: ", cname))
      } else {
        if (ctype == 1) {
          iRow <- grep(paste("^", cname, "$", sep=""), feat2winz$vname, perl=T)
          if ( length(iRow) == 1 ) {
            cat(sQuote(cname), feat2winz$min2keep[iRow], feat2winz$max2keep[iRow], feat2winz$mean[iRow],
                file = outF, sep=",", append = T)
            cat("\n", file = outF, append = T)
          } else {
            error(logger, paste("WriteTrimQuantiles: Didn't find: ", cname, "in trim list"))
          }
        } else if (ctype == 2) {
          cat(sQuote(cname), NA, NA, NA, file = outF, sep=",", append = T)
          cat("\n", file = outF, append = T)
        } else {
          error(logger, paste("WriteTrimQuantiles: don't know about this var type: ", ctype))
        }
      }
    }
    close(outF)
  }
}

ReadTrimQuantiles <- function(in.fname)
{
  # Reads in "trim" quantiles, tuples <var-name, min, max, mean>, from the specified file. 
  trims.df <- read.csv(in.fname, header = FALSE, stringsAsFactors = FALSE, quote="'")
  colnames(trims.df) <- c("vname", "min2keep", "max2keep", "mean")
  return(trims.df)
}

WriteObsIdYhat <- function(out.path, obs.id, y, y.hat, field.sep = ",", file.name = "id_y_yHat.csv")
{
  # Writes out tuples <id, y, yHat> to a text file. Useful for loading score into a db.
  #
  # Args:
  #    out.path : where output file will be written
  #      obs.id : row-id values from training data-frame
  #           y : training target vector
  #       y.hat : predicted response on training data
  # Returns:
  #     None.
  stopifnot(is.character(out.path))
  stopifnot(length(y) == length(y.hat))
  stopifnot(length(y) == length(obs.id))
  
  out.df <- data.frame(cbind(obs.id, y, y.hat))
  write.table(out.df, file = file.path(out.path, file.name), row.names = F, quote = F, na ="", sep = field.sep) 
}

ExportModel <- function(rfmod, rfmod.path, x, y, wt, y.hat, out.path, x.df, col.types, winz=0.025, x.recoded.cat.vars=NULL)
{
  # Writes out all components of a given RuleFit model required for a "restore"
  # within R, or an export as ASCII text rules.
  #
  # Args:
  #                rfmod : rulefit model object
  #           rfmod.path : path to rfmod related files
  #                    x : training data matrix 
  #                    y : training target vector
  #                   wt : training weights
  #                y.hat : predicted response on training data
  #             out.path : where output (exported) files will be written
  #                 x.df : training data frame
  #            col.types : vector indicating column types -- 1:continuous
  #                        2:categorical
  #                 winz : a data-frame, if column-specific beta was used for winsorizing;
  #                        otherwise the 'global' beta used
  #   x.recoded.cat.vars : list of <categorical var, low count levels> 
  # Requires:
  #   - REGO__HOME to be already set
  #
  # Returns:
  #     None.
  stopifnot(is.character(out.path))
  stopifnot(class(x) == "matrix")
  stopifnot(length(y) == nrow(x)) 
  stopifnot(length(y) == length(wt))
  stopifnot(length(y) == length(y.hat)) 
  stopifnot(class(x.df) == "data.frame")
  stopifnot(nrow(x.df) == nrow(x) && ncol(x.df) == ncol(x)) 
  stopifnot(length(col.types) >= ncol(x.df))

  GetRules <- function(rfmod.path) {
    # A simplified version of RuleFit's rules() function to retrieve
    # the model rules as text strings.
    mod.stats <- scan(file.path(rfmod.path, 'rfout'), what='',quiet=T)
    if (!"terms" %in% mod.stats) {
      error(logger, "GetRules: can't find #terms in 'rfout'")
    }
    n.rules <- as.numeric(mod.stats[13])
    zz <- file(file.path(rfmod.path, 'intrules'), 'wb')
    writeBin(as.integer(c(0, 1, n.rules)), zz, size=4); close(zz)
    wd <- getwd(); setwd(rfmod.path)
    status <- rfexe('rules')
    if (status != 'OK') { rfstat(); stop()}
    ruleList <- readLines('rulesout.hlp')
    unlink('rulesout.hlp')
    setwd(wd)
    return(ruleList)
  }
  
  GetIntercept <- function(rfmod.path) {
    # Returns the intercept of the RuleFit model in the given directory.
    if (GetRF_WORKING_DIR() == rfmod.path) {
      c0 <- getintercept()
    } else {
      warn(logger, paste("GetIntercept: Failed to retrieve intercept from: ", rfmod.path))
      c0 <- NA
    }
    return(c0)
  }

  # Save model
  save(rfmod, file = file.path(out.path, kMod.fname))

  # Save train <x, y, wt> data; required to restore model
  save(x, y, wt, file = file.path(out.path, kMod.xyw.fname))

  # Save yHat (on x train) -- optional
  save(y.hat, file = file.path(out.path, kMod.yHat.fname))

  # Write out var imp statistic
  vi <- varimp(plot = FALSE)
  sink(file.path(out.path, kMod.varimp.fname))
  for (i in 1:length(vi$ord)) {
    cat(vi$imp[i], "\t", colnames(x)[vi$ord[i]], "\n", sep = "")
  }
  sink()

  # Write out rules as text -- includes coefficients. 
  rulesStr <- GetRules(rfmod.path)
  sink(file.path(out.path, kMod.rules.fname))
  for (i in 4:length(rulesStr)) { # skip header
    cat(rulesStr[i], "\n")
  }
  sink()
  
  # Write out intercept
  c0 <- GetIntercept(rfmod.path)
  sink(file.path(out.path, kMod.intercept.fname))
  cat(c0, "\n")
  sink()
  
  # Write out levels for categ vars -- needed to decode rules later on
  WriteLevels(x.df, col.types, file.path(out.path, kMod.x.levels.fname))
  
  # Write out recoded categorical variables (if any)
  if (!is.null(x.recoded.cat.vars) && length(x.recoded.cat.vars) > 0) {
    WriteLevelsLowCount(x.recoded.cat.vars, file.path(out.path, kMod.x.levels.lowcount.fname))
  }

  # Write out trim-quantiles -- needed to run model outside R
  if ( class(winz) == "data.frame") {
    WriteTrimQuantiles(x.df, col.types, beta=NA, file.path(out.path, kMod.x.trim.fname), winz)
  } else {
    WriteTrimQuantiles(x.df, col.types, beta=winz, file.path(out.path, kMod.x.trim.fname))
  }
}    

LoadModel <- function(model.path)
{
  # Loads previously exported RuleFit model components required for a "restore"
  # operation. 
  #
  # Args:
  #  model.path : path to RuleFit model exported files
  #
  # Returns:
  #     Tuple <rfmod, x, y, wt>
  #
  # Notes:
  #   - Should be followed by rfrestore(rfmod, x=x, y=y, wt=wt).
  stopifnot(is.character(model.path))

  if (file.access(model.path, mode=4) != 0) {
    error(logger, paste("LoadModel: Path: '", model.path, "' is not accessible"))
  }
  if (!file.exists(file.path(model.path, kMod.xyw.fname))) {
    error(logger, paste("LoadModel: Can't find file: ", kMod.xyw.fname))
  }
  if (!file.exists(file.path(model.path, kMod.fname))) {
    error(logger, paste("LoadModel: Can't find file: ", kMod.fname))
  }
  
  # Load <x, y> data
  saved.objs <- load(file = file.path(model.path, kMod.xyw.fname))
  if (any(saved.objs != c("x", "y", "wt"))) {
    error(logger, paste("LoadModel: Failed to find required objects in: ", file.path(model.path, kMod.xyw.fname)))
  }
  
  # Load RuleFit model
  saved.objs <- load(file = file.path(model.path, kMod.fname))
  if ( length(which((saved.objs == c("rfmod")) == T)) != 1 ) {
    error(logger, paste("LoadModel: Failed to find required objects in: ", file.path(model.path, kMod.fname)))
  }
  
  return(list(rfmod = rfmod, x = x, y = y, wt = wt))
}
