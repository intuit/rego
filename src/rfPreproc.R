source(file.path(REGO_HOME, "/src/logger.R"))
source(file.path(REGO_HOME, "/src/winsorize.R"))

# Constants
kLowCountLevelsName  <- "_LowCountLevels_"

PruneFeatures <- function(x, na.thresh = 0.95, col.skip.fname = "")
{
  # Prune df to contain only useful predictor variables -- e.g. remove
  # singletons, all NAs.
  #
  # Args:
  #              x : data frame
  #      na.thresh : max percentage of NAs allowed in a column of x
  # col.skip.fname : character string naming the file with user-specifed
  #                  column to remoce from x
  #
  # Returns:
  #      A new data frame without pruned columns
  dbg(logger, "PruneFeatures:")
  stopifnot(class(x) == "data.frame")

  colNum2Remove<- vector()
  N <- nrow(x)
  ## 0) all identical values?
  colNum2Remove.0 <- vector()
  for (i in 1:ncol(x)) {
    if (length(unique(x[,i])) == 1) {
      dbg(logger, paste("  i = ", i, " name = ", colnames(x)[i], "is constant"))
      colNum2Remove.0 <- c(colNum2Remove.0, i)
    }
  }
  info(logger, paste("Singletons:", length(colNum2Remove.0)))
  colNum2Remove <- c(colNum2Remove, colNum2Remove.0)
  
  ## 1) only two values, one of which is NA?
  colNum2Remove.1 <- vector()
  for (i in 1:ncol(x)) {
    if (length(unique(x[,i])) == 2 && length(which(is.na(x[,i]) == T)) > 0) {
      dbg(logger, paste("  i = ", i, " name = ", colnames(x)[i], "is NA+constant"))
      colNum2Remove.1 <- c(colNum2Remove.1, i)
    }
  }
  info(logger, paste("Quasi-Singletons:", length(colNum2Remove.1)))
  colNum2Remove <- c(colNum2Remove, colNum2Remove.1)
  
  ## 2) columns w "many" NA's?
  max.num.NAs <- na.thresh*N
  dbg(logger, paste("Max Num NAs:", max.num.NAs))
  colNum2Remove.2 <- vector()
  for (i in 1:ncol(x)) {
    if ( length(which(is.na(x[,i]) == T)) > max.num.NAs ) {
      dbg(logger, paste("  i =", i, " name =", colnames(x)[i], "is mostly NA"))
      colNum2Remove.2 <- c(colNum2Remove.2, i)
    }
  }
  info(logger, paste("Mostly NAs:", length(colNum2Remove.2)))
  colNum2Remove <- c(colNum2Remove, colNum2Remove.2)
  
  ## 3) user-specified columns?
  if (nchar(col.skip.fname) > 0) {
    if (file.exists(col.skip.fname)) {
      colNum2Remove.3 <- vector()
      feat2skip <- read.table(col.skip.fname, sep = ",", header = F, col.names = c("var.name"), as.is = T)[,1]
      for (cname in feat2skip) {
        iCol <- grep(paste("^", cname, "$", sep=""), colnames(x), perl=T, ignore.case=T)
        if ( length(iCol == 1) ) {
          colNum2Remove.3 <- c(colNum2Remove.3, iCol)
        } else {
          dbg(logger, paste("Didn't find: ", cname))
        }
      }
      info(logger, paste("User specified:", length(colNum2Remove.3)))
      dbg(logger, paste(colnames(x)[colNum2Remove.3]))
      colNum2Remove <- c(colNum2Remove, colNum2Remove.3)    
    } else {
      warn(logger, paste("File ", col.skip.fname, "doesn't exist"))
    }
  }
  
  # Columns w "some" NA's? Just for info...
  num.some.NA <- 0
  for (i in 1:ncol(x)) {
    if (!(i %in% colNum2Remove)) {
      num.NA <- length(which(is.na(x[,i]) == T))
      if (num.NA > 0) {
        dbg(logger, paste("  i =", i, " NA-rate =", round(100.0*num.NA/N),
                           "\tname =", colnames(x)[i]))
        num.some.NA <- num.some.NA + 1
      }
    }
  }
  info(logger, paste("Some NAs:", num.some.NA))

  if (length(colNum2Remove) > 0) {
    if (ncol(x) - length(colNum2Remove) == 1) {
      # Only one column left... avoid coercion to vector
      tmp.df <- data.frame(x[,-colNum2Remove])
      colnames(tmp.df) <- setdiff(colnames(x), colnames(x)[colNum2Remove])
      return(tmp.df)
    } else {
      return(x[, -colNum2Remove])
    }
  } else {
    return(x)
  }
}

WinsorizeFeatures <- function(x, feat2winz.fname)
{
  # Applies Winsorization transformation to the specified variables.
  #
  # Args:
  #                  x : data frame
  #    feat2winz.fname : text file with names of columns to Winsorize
  #                      (pairs <vname, beta> expected)
  # Returns:
  #      A list with
  #                  x : copy of input data frame with transformed columns
  #              trims : data frame with used trim values
  stopifnot(class(x) == data.frame)
  stopifnot(file.access(feat2winz.fname, mode = 4) == 0)

  dbg(logger, "WinsorizeFeatures:")
  feat2winz <- read.table(feat2winz.fname, sep =",", as.is=T)
  dbg(logger, paste(nrow(feat2winz), "columns to Winsorize"))
  
  # Augment <vname, beta> data-frame to remember computed trims
  feat2winz <- cbind(feat2winz, rep(NA, nrow(feat2winz)),
                     rep(NA, nrow(feat2winz)), rep(NA, nrow(feat2winz)))
  names(feat2winz) <- c("vname", "beta", "min2keep", "max2keep", "mean")

  # Loop over columns to winsorize
  for (i in 1:nrow(feat2winz)) {
    if (is.finite(feat2winz$beta[i]) && feat2winz$beta[i] >= 0 && feat2winz$beta[i] < 1) {
      iCol <- grep(paste("^",feat2winz$vname[i],"$", sep=""), colnames(x), perl=T)
      if ( length(iCol) == 1 ) {
        if (class(x[, iCol]) %in% c("numeric", "integer")) {
          l.x <- winsorize(x[, iCol], feat2winz$beta[i])
          x[, iCol] <- l.x$x
          feat2winz$min2keep[i] <- l.x$min2keep
          feat2winz$max2keep[i] <- l.x$max2keep
          feat2winz$mean[i] <- mean(l.x$x, na.rm = T)
        } else {
          warn(logger, paste("Can't trim non-continuous var: ", feat2winz$vname[i]))
        }
      } else {
        warn(logger, paste("Didn't find: ", feat2winz$vname[i]))
      }
    }
  }

  return(list(x=x, trims=feat2winz))
}

CheckColumnTypes <- function(x)
{
  # Checks if there are columns which are not numeric, factor or logical
  #
  # Args:          x : data frame
  # Returns:       boolean
  dbg(logger, "CheckColumnTypes:")
  good_class <- sapply(x, inherits, what = c("numeric", "integer", "factor", "logical"))
  all.good <- all(good_class)
  sapply(which(!good_class), function(i) {
    warn(logger, paste("  i = ", i, " name = ", colnames(x)[i], "type: ", paste(class(x[,i]), collapse = ", ")))
  })
  return(all.good)
}

EnforceFactors <- function(x, col.types, min.level.count = 0)
{
  # Make sure categorical variables are factors in given data-frame.
  #
  # Args:
  #                x : data frame
  #        col.types : vector indicating column types -- 1:continuous
  #                    2:categorical
  #  min.level.count : merge levels with fewer than this count
  # Returns:
  #      A copy of input data frame with transformed columns, and a
  #      list of categorical variable indices
  dbg(logger, "EnforceFactors:")
  stopifnot(ncol(x) <= length(col.types))

  cat.vars <- c()
  recoded.cat.vars <- list()
  i.recoded <- 1
  for (i in 1:ncol(x)) {
    cname <- colnames(x)[i]
    i.col.type <- grep(paste("^", cname, "$", sep=""), names(col.types), perl=T, ignore.case=T)
    if ( length(i.col.type == 1) ) {
      if (col.types[i.col.type] == 2) {
        cat.vars <- c(cat.vars, i)
        if ( ! inherits(x[,i], "factor") ) {
          dbg(logger, paste("  Converting i =", i, " name =", cname,
                            "from: '", class(x[, i]), "' to 'factor'"))
          x[, i] <- as.factor(x[, i])
        }
        # Check for low count levels?
        if (min.level.count > 0) {
          low.count.levels <- c()
          level.hist <- summary(x[, i], maxsum = nlevels(x[, i]))
          for (i.level in names(level.hist)) {
            if (level.hist[i.level] < min.level.count) {
              low.count.levels <- c(low.count.levels, i.level)
            }
          }
          # Recode factor if necessary
          if (length(low.count.levels) > 0) {
            x[, i] <- factor(ifelse(x[, i] %in% low.count.levels, kLowCountLevelsName, as.character(x[, i])))
            recoded.cat.vars[[i.recoded]] <- list(var=cname, low.count.levels = low.count.levels)
            i.recoded <- i.recoded + 1
            dbg(logger, paste("  Collapsing low-count levels for '", cname, "' ; ", paste(low.count.levels, collapse = ', '))) 
          }
        }
      }
    } else {
      error(logger, paste("Didn't find type for:", cname))
    }
  }
  
  return(list(x = x, cat.vars = cat.vars, recoded.cat.vars = recoded.cat.vars))
}
