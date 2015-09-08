source(file.path(REGO_HOME, "/src/logger.R"))
source(file.path(REGO_HOME, "/lib/RuleFit/rulefit.r"))

DefaultRFContext <- function()
{
  # Populates a list of configuration parameters, with names and values as used
  # by RuleFit3.
  rf.ctxt <- vector(mode='list')
  rf.ctxt$xmiss      <- 9.0e30
  rf.ctxt$rfmode     <- "regress"
  rf.ctxt$sparse     <- 1
  rf.ctxt$test.reps  <- 0
  rf.ctxt$test.fract <- 0.2
  rf.ctxt$mod.sel    <- 2
  rf.ctxt$model.type <- "both"
  rf.ctxt$tree.size  <- 4
  rf.ctxt$max.rules  <- 2000
  rf.ctxt$max.trms   <- 500
  rf.ctxt$costs      <- c(1,1)
  rf.ctxt$trim.qntl  <- 0.025
  rf.ctxt$inter.supp <- 3.0
  rf.ctxt$memory.par <- 0.01
  rf.ctxt$conv.thr   <- 1.0e-3
  rf.ctxt$mem.tree.store <- 10000000
  rf.ctxt$mem.cat.store  <- 1000000
  return(rf.ctxt)
}

IntiRFContext <- function(conf.fname)
{
  # Parses a given configuration file, and uses it to initialize an RF "context."
  # An "RF Context" is just a verbose version of RuleFit's config param set needed
  # to run it.
  #
  # Args:
  #   conf.fname: configuration file name
  #
  # Returns:
  #   A list of <param name, param value> pairs
  kKnownParamNames <- c("rf.platform", "rf.working.dir", "rf.export.dir", "task", "model.type",
                        "model.max.rules", "model.max.terms", "te.tree.size", "te.sample.fraction", 
                        "te.interaction.suppress", "te.memory.param", "sparsity.method", 
                        "score.criterion", "crossvalidation.num.folds", "crossvalidation.fold.size", 
                        "misclassification.costs", "data.trim.quantile", "data.NA.value", 
                        "convergence.threshold", "mem.tree.store", "mem.cat.store")
  
  rf.ctxt <- DefaultRFContext()
  
  # Read config file (two columns assumed: 'param' and 'value')
  tmp <- read.table(conf.fname, header=T, as.is=T)
  conf <- as.list(tmp$value)
  names(conf) <- tmp$param

  # Do we recognize all given param names?
  for (param in names(conf)) {
    if (!(param %in% kKnownParamNames)) {
      warn(logger, paste("Unrecognized parameter name:", param))
    }
  }

  # Installation info
  # ...Auto-detect the platform:
  rf.ctxt$platform <- switch(.Platform$OS.type
                             , windows = "windows"
                             , unix = switch(Sys.info()["sysname"]
                                   , Linux = "linux"
                                   , Darwin = "mac"))

  if (is.null(rf.ctxt$platform)) error(logger, "Unable to detect platform")

  # ...path to RF working directory
  if (!("rf.working.dir" %in% names(conf))) {
    # Try to get parameter from the env
    conf$rf.working.dir <- Sys.getenv("RF_WORKING_DIR")
  }
  if (substr(conf$rf.working.dir, 1, 2) == "./") {
    # Avoid relative paths
    conf$rf.working.dir <- file.path(getwd(), conf$rf.working.dir)
  }
  if (file.access(conf$rf.working.dir) != 0) {
    error(logger, paste("You need to specify a valid RF working dir...", conf$rf.working.dir, "isn't good"))
  } else {
    rf.ctxt$working.dir <- conf$rf.working.dir
  }  
  
  # ...path to RF export directory
  if (!("rf.export.dir" %in% names(conf))) {
    conf$rf.export.dir <- file.path(conf$rf.working.dir, "export")
  }
  if (!(file.exists(conf$rf.export.dir))) {
    dir.create(conf$rf.export.dir)
  }
  if (file.access(conf$rf.export.dir) != 0) {
    error(logger, paste("You need to specify a valid RF export dir...", conf$rf.export.dir, "isn't good"))
  } else {
    rf.ctxt$export.dir <- conf$rf.export.dir
  }  
  
  # Regression or classification task?
  if (!("task" %in% names(conf))) {
    error(logger,"You need to specify a 'task' type: 'regression' or 'classification'")
  } else if (conf$task == "regression") {
    rf.ctxt$rfmode <- "regress"
  } else if (conf$task == "classification") {
    rf.ctxt$rfmode <- "class"
  } else {
    error(logger, paste("Unrecognized 'task' type:", conf$task,
                        " ... expecting: 'regression' or 'classification'"))
  }

  # Model specification
  # ... type
  if (!("model.type" %in% names(conf)) |
      !(conf$model.type %in% c("linear", "rules", "both"))) {
    error(logger, "You need to specify a model type: 'linear', 'rules' or 'both'")
  } else {
    rf.ctxt$model.type <- conf$model.type
  }
  # ...number of rules generated for regression posprocessing
  if ("model.max.rules" %in% names(conf)) {
    rf.ctxt$max.rules <- as.numeric(conf$model.max.rules)
  }
  # ...maximum number of terms selected for final model
  if ("model.max.terms" %in% names(conf)) {
    rf.ctxt$max.trms <- as.numeric(conf$model.max.terms)
  }

  # Tree Ensemble control
  # ...average number of terminal nodes in generated trees    
  if ("te.tree.size" %in% names(conf)) {
    rf.ctxt$tree.size <- as.numeric(conf$te.tree.size)
  }
  # ...fraction of randomly chosen training observations used to produce each tree
  if ("te.sample.fraction" %in% names(conf)) {
    rf.ctxt$samp.fract <- as.numeric(conf$te.sample.fraction)
  }
  # ...incentive factor for using fewer variables in tree based rules
  if ("te.interaction.suppress" %in% names(conf)) {
    rf.ctxt$inter.supp <- as.numeric(conf$te.interaction.suppress)
  }
  # ... learning rate applied to each new tree when sequentially induced
  if ("te.memory.param" %in% names(conf)) {
    rf.ctxt$memory.par <- as.numeric(conf$te.memory.param)
  }
    
  # Regularization (postptocessing) control
  if (!("sparsity.method" %in% names(conf)) |
      !(conf$sparsity.method %in% c("Lasso", "Lasso+FSR", "FSR"))) {
    error(logger, "You need to specify a sparsity method: 'Lasso', 'Lasso+FSR' or 'FSR'")
  } else if (conf$sparsity.method == "Lasso") {
    rf.ctxt$sparse <- 1
  } else if (conf$sparsity.method == "Lasso+FSR") {
    rf.ctxt$sparse <- 2
  } else if (conf$sparsity.method == "FSR") {
    rf.ctxt$sparse <- 3
  }

  # Model selection
  # ...loss/score criterion
  if (!("score.criterion" %in% names(conf)) |
      !(conf$score.criterion %in% c("1-AUC", "AAE", "LS", "Misclassification"))) {
    error(logger, "You need to specify a score criterion: '1-AUC', 'AAE', 'LS' or 'Misclassification'")
  } else if (conf$score.criterion == "1-AUC" & conf$task == "classification") {
    rf.ctxt$mod.sel <- 1
  } else if (conf$score.criterion == "AAE" & conf$task == "regression") {
    rf.ctxt$mod.sel <- 1    
  } else if (conf$score.criterion == "LS") {
    rf.ctxt$mod.sel <- 2
  } else if (conf$score.criterion == "Misclassification" & conf$task == "classification") {
    rf.ctxt$mod.sel <- 3
  } else {
    error(logger, paste("Invalid score criterion specification -- task: '",
            conf$task, "', score.criterion: '", conf$score.criterion, "'", sep = ""))
  }
  # ...number of cross-validation replications
  if ("crossvalidation.num.folds" %in% names(conf)) {
    rf.ctxt$test.reps <- as.numeric(conf$crossvalidation.num.folds)
  }
  # ...fraction of observations used it test group
  if ("crossvalidation.fold.size" %in% names(conf)) {
    rf.ctxt$test.fract <- as.numeric(conf$crossvalidation.fold.size)
  }
  # ...misclassificarion costs
  if ("misclassification.costs" %in% names(conf)) {
    rf.ctxt$costs <- c()
    rf.ctxt$costs[1] <- as.numeric(strsplit(conf$misclassification.costs, ",")[[1]][1])
    rf.ctxt$costs[2] <- as.numeric(strsplit(conf$misclassification.costs, ",")[[1]][2])
  }  

  # Data preprocessing
  # ...linear variable winsorizing factor 
  if ("data.trim.quantile" %in% names(conf)) {
    rf.ctxt$trim.qntl <- as.numeric(conf$data.trim.quantile)
  }
  # ...numeric value indicating missingness in predictors
  if ("data.NA.value" %in% names(conf)) {
    rf.ctxt$xmiss <- as.numeric(conf$data.NA.value)
  }

  # Iteration Control
  # ...convergence threshold for regression postprocessing
  if ("convergence.threshold" %in% names(conf)) {
    rf.ctxt$conv.thr <- as.numeric(conf$convergence.threshold)
  }

  # Memory management
  # ...size of internal tree storage (decrease in response to allocation error;
  #    increase value for very large values of max.rules and/or tree.size)
    if ("mem.tree.store" %in% names(conf)) {
    rf.ctxt$mem.tree.store <- as.numeric(conf$mem.tree.store)
  }
  # ... size of internal categorical value storage (decrease in response to
  #     allocation error; increase for very large values of max.rules and/or
  #     tree.size in the presence of many categorical variables with many levels)
  if ("mem.cat.store" %in% names(conf)) {
    rf.ctxt$mem.cat.store <- as.numeric(conf$mem.cat.store)
  }

  return(rf.ctxt)
}

TrainRF <- function(x, y, wt, rf.context, cat.vars=NULL, not.used=NULL)
{
  # Invokes RuleFit model building procedure.
  #
  # Args:
  #            x: input data frame
  #            y: response vector
  #           wt: observation weights
  #     cat.vars: categorical variables (column numbers or names)
  #   rf.context: configuration parameters
  #
  # Returns:
  #   RuleFit model object
  dbg(logger, "TrainRF:")

  ok <- 1
  if ("samp.fract" %in% names(rf.context)) {
    # User-specified "samp.fract"
    tryCatch(rfmod <- rulefit(x, y, wt, cat.vars, not.used
                             ,xmiss      = rf.context$xmiss      
                             ,rfmode     = rf.context$rfmode     
                             ,sparse     = rf.context$sparse     
                             ,test.reps  = rf.context$test.reps  
                             ,test.fract = rf.context$test.fract 
                             ,mod.sel    = rf.context$mod.sel    
                             ,model.type = rf.context$model.type 
                             ,tree.size  = rf.context$tree.size  
                             ,max.rules  = rf.context$max.rules  
                             ,max.trms   = rf.context$max.trms   
                             ,costs      = rf.context$costs      
                             ,trim.qntl  = rf.context$trim.qntl
                             ,samp.fract = rf.context$samp.fract
                             ,inter.supp = rf.context$inter.supp 
                             ,memory.par = rf.context$memory.par 
                             ,conv.thr   = rf.context$conv.thr
                             ,quiet      = TRUE
                             ,tree.store = rf.context$mem.tree.store 
                             ,cat.store  = rf.context$mem.cat.store),
             error = function(err){ok <<- 0})
    if (ok == 0) {
      error(logger, "TrainRF: got stuck in rulefit")
    }  
  } else {
    # No mention of "samp.fract"... let rulefit set it based on data size 
    tryCatch(rfmod <- rulefit(x, y, wt, cat.vars, not.used
                             ,xmiss      = rf.context$xmiss      
                             ,rfmode     = rf.context$rfmode     
                             ,sparse     = rf.context$sparse     
                             ,test.reps  = rf.context$test.reps  
                             ,test.fract = rf.context$test.fract 
                             ,mod.sel    = rf.context$mod.sel    
                             ,model.type = rf.context$model.type 
                             ,tree.size  = rf.context$tree.size  
                             ,max.rules  = rf.context$max.rules  
                             ,max.trms   = rf.context$max.trms   
                             ,costs      = rf.context$costs      
                             ,trim.qntl  = rf.context$trim.qntl  
                             ,inter.supp = rf.context$inter.supp 
                             ,memory.par = rf.context$memory.par 
                             ,conv.thr   = rf.context$conv.thr
                             ,quiet      = TRUE
                             ,tree.store = rf.context$mem.tree.store 
                             ,cat.store  = rf.context$mem.cat.store),
             error = function(err){ok <<- 0})
    if (ok == 0) {
      error(logger, "TrainRF: got stuck in rulefit")
    }  
  }

  return(rfmod)
}
