rfCV <- function(x, y, wts, x.cat.vars, rf.ctxt, nfold=5, yHat.return=FALSE, seed=135711)
{
  # TODO: config$rand.seed
  #
  # Performs outer cross-validation of RuleFit models.
  #
  # Args:
  #   x: predictor matrix
  #   y: response vector
  #   wts: observation weights
  #   x.cat.vars: list of categorical variable indices
  #   rf.ctxt: RuleFit-specific configuration parameters
  #   nfold: number of cross-validation runs to do
  #   yHat.return: whether to return out-of-sample yHat
  #   seed: random number seed (for partitioning x's rows)
  #
  # Returns:
  #   A list with:
  #     stats: a nfold-by-10 matrix with the following columns:
  #            ECV, ECV std: Estimated criterion value (e.g., AAE) reported by RuleFit
  #            terms: model size reported by RuleFit
  #            train.*: in-sample error
  #            test.*: out-of-sample error
  #     oos.yHat: out-of-sample yHat (if requested)
  dbg(logger, "rfCV:")

  # Init return structures
  cv.stats <- matrix(NA, nrow = nfold, ncol = 10)
  oos.yHat <- NULL
  oos.y.idx <- NULL
  colnames(cv.stats) <- c("ECV", "ECV_std", "terms",
                          "train.error", "train.med.error", "train.aae",
                          "test.error", "test.med.error", "test.aae",
                          "cor.test")
  # Generate data splits
  set.seed(seed)
  group <- sample(rep(1:nfold, length = nrow(x)))

  # Build 'nfold' models
  for (i.cv in 1:nfold) {
    # Subset data
    test <- which(group == i.cv)
    x.train <- x[-test,]
    x.test  <- x[test,]
    y.train <- y[-test]
    y.test  <- y[test]
    wt.train <- wts[-test]
    wt.test <- wts[test]
      
    # Build model-i (which internally also uses cv for stopping param)
    set.seed(config$rand.seed)
    rfmod <- TrainRF(x, y, obs.wt, rf.ctxt, x.cat.vars)
    # ... Log (estimated generalization) model error and size
    rfmod.stats <- runstats(rfmod)
    info(logger, paste("Estimated criterion value:", rfmod.stats$cri, paste("(+/- ", rfmod.stats$err, "),", sep=""),
                       "Num terms:", rfmod.stats$terms))
    
    # Collect model stats: "Criterion", "terms"
    cv.stats[i.cv, 1] <- rfmod.stats$cri
    cv.stats[i.cv, 2] <- rfmod.stats$err
    cv.stats[i.cv, 3] <- rfmod.stats$terms
      
    # Collect in-sample accuracy 
    yHat.train <- rfpred(x.train)
    re.train.error <- sum(wt.train*abs(yHat.train - y.train))/sum(wt.train)
    train.med.error <- sum(wt.train*abs(y.train - wMed(y.train, wt.train)))/sum(wt.train)
    train.aae <- re.train.error / train.med.error
    cv.stats[i.cv, 4] <- re.train.error
    cv.stats[i.cv, 5] <- train.med.error
    cv.stats[i.cv, 6] <- train.aae
    # Collect out-of-sample accuracy 
    yHat.test <- rfpred(x.test)
    re.test.error <- sum(wt.test*abs(yHat.test - y.test))/sum(wt.test)
    test.med.error <- sum(wt.test*abs(y.test - wMed(y.train, wt.train)))/sum(wt.test)
    test.aae <- re.test.error / test.med.error
    cv.stats[i.cv, 7] <- re.test.error
    cv.stats[i.cv, 8] <- test.med.error
    cv.stats[i.cv, 9] <- test.aae
    cv.stats[i.cv, 10] <- cor(yHat.test, y.test)
    # Collect out-of-sample yHat
    if (yHat.return) {
      if (is.null(oos.yHat) && is.null(oos.y.idx)) {
        oos.yHat <- list(yHat.test)
        oos.y.idx <- list(test)
      } else {
        oos.yHat <- c(oos.yHat, list(yHat.test))
        oos.y.idx <- c(oos.y.idx, list(test))
      }
    }
  }

  return(list(stats = cv.stats, oos.y.idx = oos.y.idx, oos.yHat = oos.yHat))                                   
}
