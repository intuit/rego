source(file.path(REGO_HOME, "/src/logger.R"))
source(file.path(REGO_HOME, "/src/rfPreproc.R"))
source(file.path(REGO_HOME, "/src/rfAPI.R"))
source(file.path(REGO_HOME, "/src/rfExport.R"))

ReadColumnTypes <- function(fname)
{
  # Reads a text file of <column-name, column-type> pairs.
  # 
  # Args:
  #        fname: character string naming the file
  # Returns:
  #        A numeric vector 
  stopifnot(!file.access(fname, mode = 4))
  dbg(logger, "ReadColumTypes:")

  col.types.df <- read.table(fname, sep = ",", header = F)
  if (ncol(col.types.df) != 2) {
    error(logger, paste("Expecting 2 columns in", fname, "file!"))
  }
  col.types <- col.types.df[, 2]
  names(col.types) <- col.types.df[, 1] 
  dbg(logger, paste("  ", length(col.types), "elements read"))
  if (!all(col.types %in% c(1, 2))) {
    error(logger, paste("Invalid column types found"))
  }
  return(col.types)
}

TrainModel <- function(data, config, rf.ctxt)
{
  # Builds a RuleFit model on the given data set.
  #
  # Args:
  #         data: data-frame with <x, y> rows
  #       config: configuration parameters
  #      rf.ctxt: RuleFit-specific configuration parameters
  #
  # Returns:
  #         A tuple <rfmod, train.stats>.  
  stopifnot(!is.na(config$col.y))
  dbg(logger, "TrainModel:")
  
  # Pop the target column from the feature space
  i <- grep(paste("^", config$col.y, "$", sep=""), colnames(data), perl=T)
  if (length(i) == 0) {
    error(logger, paste("Target column", config$col.y, "not found in input data-frame!"))
  }
  y <- data[, i]
  data <- data[, -i] # pop
  
  # If classification task, check y values
  if (rf.ctxt$rfmode == "class") {
    if (length(unique(y)) != 2) {
      error(logger, paste("Target column", config$col.y, "must have 2 values only"))
    }
    y.summary <- summary(as.factor(y))
    info(logger, "Y summary:")    
    info(logger, sprintf("  %s: %d, %s: %d", names(y.summary)[1], y.summary[1],
                         names(y.summary)[2], y.summary[2]))
    if (min(y) != -1 || max(y) != 1) {
      if (min(y) == 0 && max(y) == 1) {
        y <- 2 * y - 1 # switch from (0,1) boolean to (-1,1) boolean
      } else {
        error(logger, paste("Target column", config$col.y, "must be 0/1"))        
      }
    } # else, leave it as is
  }

  # Set observation weights
  if (!is.na(config$col.weights)) {
    obs.wt <- data[[config$col.weights]]
    # Pop weight column from data
    i <- grep(paste("^", config$col.weights, "$", sep=""), colnames(data), perl=T);
    if (length(i) > 0) {
      dbg(logger, paste("Pop weight Column: ", i))
      data <- data[, -i] # pop
    } else {
      error(logger, paste("Weight column", config$col.weights, "not found!"))
    }
  } else if (conf$do.class.balancing && rf.ctxt$rfmode == "class") {
    # Balance classes via observation weights - equal ratio
    i.pos <- which(y == 1)
    obs.wt <- rep(1, nrow(data))
    obs.wt[-i.pos] <- nrow(data[i.pos, ]) / nrow(data[-i.pos, ])
    obs.wt.summary <- summary(as.factor(obs.wt))
    info(logger, "Class weights:")    
    info(logger, paste(names(obs.wt.summary), obs.wt.summary, sep=":", collapse=" "))
  } else if ("row.weights.fname" %in% names(conf)) {
    # Customized weights for each row
    obs.wt <- read.table(conf$row.weights.fname, header = F)[,1]
    obs.wt.summary <- summary(obs.wt)
    info(logger, "Row weights (from weight file):")
    info(logger, paste(names(obs.wt.summary), obs.wt.summary, sep=":", collapse=" "))    
  } else {
    obs.wt <- rep(1, nrow(data))
  }
  
  # Pop the row-id column from the feature space, if present
  if (!is.na(config$col.id)) {
    i <- grep(paste("^", config$col.id, "$", sep=""), colnames(data), perl=T)
    if (length(i) == 0) {
      error(logger, paste("ID column", config$col.id, "not found in input data-frame!"))
    }
    obs.id <- data[, i]
    dbg(logger, paste("Pop ID Column: ", i))
    data <- data[, -i] # pop
  } else {
    obs.id <- NULL
  }
  
  # Prune data to contain only useful predictor variables
  xDF <- PruneFeatures(data, conf$na.threshold, conf$col.skip.fname)
  if (ncol(xDF) == 0) {
    error(logger, paste("PruneFeatures pruned everything!  Nothing to do!"));
  }

  # Make sure categorical variables are factors
  if ("col.types.fname" %in% names(conf)) {
    col.types <- ReadColumnTypes(config$col.types.fname)
  } else {
    col.types <- ifelse(sapply(data, inherits, what = "factor"), 2, 1)
  }
  ef.out <- EnforceFactors(xDF, col.types, config$min.level.count)
  xDF <- ef.out$x
  x.cat.vars <- ef.out$cat.vars
  x.recoded.cat.vars <- ef.out$recoded.cat.vars
  rm(ef.out)
    
  # Check column types
  if (!CheckColumnTypes(xDF)) {
    error(logger, paste("Invalid column type(s) found!"));
  }

  # Any numeric column preprocessing?
  x.trims <- NULL
  if (nchar(config$col.winz.fname) > 0) {
    wf.out <- WinsorizeFeatures(xDF, config$col.winz.fname)
    xDF <- wf.out$x
    x.trims <- wf.out$trims
    # Overwrite trim.qntl param
    rf.ctxt$trim.qntl <- 0.0
  }

  # Coerce to numeric matrix & mark NAs
  x <- data.matrix(xDF)
  x[is.na(x)] <- 9.0e30

  # Save workspace (before training - mostly for debugging purposes)?
  if (config$save.workspace) {
    dbg(logger, "Saving workspace")
    save(xDF, x, y, obs.wt, rf.ctxt, x.cat.vars, file = file.path(rf.ctxt$export.dir , "workspace.rdata"))
  }
  
  # Build model
  set.seed(config$rand.seed)
  rfmod <- TrainRF(x, y, obs.wt, rf.ctxt, x.cat.vars)
  # ... Log (estimated generalization) model error and size
  rfmod.stats <- runstats(rfmod)
  info(logger, paste("Estimated criterion value:", rfmod.stats$cri, paste("(+/- ", rfmod.stats$err, "),", sep=""),
                     "Num terms:", rfmod.stats$terms))

  # Training error... just for info
  y.hat.train <- rfpred(x)
  train.stats <- list()
  if (rf.ctxt$rfmode == "class") {
    conf.m <- table(y, sign(y.hat.train))    
    stopifnot("-1" %in% rownames(conf.m))
    stopifnot("1" %in% rownames(conf.m))
    TN <- ifelse("-1" %in% colnames(conf.m), conf.m["-1", "-1"], 0)
    FP <- ifelse("1" %in% colnames(conf.m), conf.m["-1","1"], 0)
    FN <- ifelse("-1" %in% colnames(conf.m), conf.m["1", "-1"], 0)
    TP <- ifelse("1" %in% colnames(conf.m), conf.m["1","1"], 0)
    train.acc <- 100*(TN+TP)/length(y.hat.train)   
    info(logger, paste("Training acc:", round(train.acc, 2)))
    info(logger, sprintf("Training confusion matrix - 0/0: %d, 0/1: %d, 1/0: %d, 1/1: %d",
                         TN, FP, FN, TP))
    train.stats$type <- "class"
    train.stats$acc <- train.acc
    train.stats$conf.m <- conf.m
  } else {
    re.train.error <- sum(abs(y.hat.train - y))/nrow(x)
    med.train.error <- sum(abs(y - median(y)))/nrow(x)
    aae.train <- re.train.error / med.train.error
    info(logger, sprintf("Training AAE: %f (RE:%f, Med:%f)", aae.train, re.train.error, med.train.error))
    train.stats$type <- "regress"
    train.stats$aae <- aae.train
    train.stats$re.error <- re.train.error
    train.stats$med.error <- med.train.error
  }
  
  # Export (save) model
  ExportModel(rfmod = rfmod, rfmod.path = rf.ctxt$working.dir,
              x = x, y = y, wt = obs.wt, y.hat = y.hat.train,
              out.path = rf.ctxt$export.dir,
              x.df = xDF, col.types = col.types,
              winz = ifelse(is.null(x.trims), rf.ctxt$trim.qntl, x.trims),
              x.recoded.cat.vars)
          
  # Dump <id, y, yHat> tuples, if appropriate
  if (!is.null(obs.id)) {
    WriteObsIdYhat(out.path = rf.ctxt$export.dir, obs.id = obs.id, y = y, y.hat = y.hat.train)
  }

  return(list(rfmod = rfmod, train.stats = train.stats))
}
