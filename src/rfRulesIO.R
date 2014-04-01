source(file.path(REGO_HOME, "/src/rfPreproc.R"))

# Constants
kRuleTypeLinear       <- "linear"
kRuleTypeSplit        <- "split"
kSplitTypeContinuous  <- "continuous"
kSplitTypeCategorical <- "categorical"
kMinusInf             <- -9.9e+35
kPlusInf              <- 9.9e+35
kMissing              <- 9e+30
kCondIN               <- "in"
kCondNotIN            <- "not"

ReadRules <- function(file)
{
  # Reads the given text file, assumed to contain RuleFit-generated rules,
  # and returns a corresponding list representation.
  #
  # Args:
  #        file: a character string or connection (this file is typically
  #              created by the ExportModel() function)
  # Returns:
  #        A list of <type, supp|std, coeff, imp, splits|var> tuples,
  #   where:
  #         type: "split" or "linear" term
  #         supp: rule support, if "split" term
  #          std: standard deviation of predictor, if "linear" term
  #        coeff: term's coefficient
  #          imp: term's importance
  #       splits: list of one or more splits defining "split" term; can be
  #               <type, var, cond, levels> or <type, var, min, max>
  #          var: predictor name, if "linear" term
  stopifnot(is.character(file) || inherits(file, "connection"))

  NextRuleFitToken <- function(file) {
    # Fetches the next "token" from the given RuleFit rules text file.
    #
    # Args:
    #        file: an open file or connection
    # Returns:
    #        A string.
    kSep <- "="     # value separator

    # Skip "space" until beginning of token
    begunToken <- FALSE 
    aChar <- readChar(file, 1)
    while (length(aChar) != 0 && begunToken == FALSE && aChar != "") {
      # skip whitespace
      while (regexpr("[[:space:]]", aChar) != -1 || aChar == kSep) {
        aChar <- readChar(file, 1)
        if ( length(aChar) == 0) break
      }
      aToken <- aChar
      begunToken <- TRUE
    }

    if (begunToken) {
      aChar <- readChar(file, 1)
      # Until a brace, whitespace, comma, or EOF
      while (length(aChar) != 0
             && aChar != "" 
             && regexpr("[[:space:]]", aChar) == -1
             && !aChar %in% c(":", kSep) )
        {
          aToken <- paste(aToken, aChar, sep="")
          aChar <- readChar(file, 1)
        }
      return(list(token=aToken))
    } else {
      return(list(token=NULL))
    }
  }

  ParseLinearRule <- function(file) {
    # Parses and returns a "linear" rule from the given RuleFit rules text file.
    #
    # Args:
    #        file: an open file or connection
    # Returns:
    #        A tuple <type="linear", std, coeff, imp, var>.
    kStdStr <- "std"
    kCoefficientStr <- "coeff"
    kImportanceStr <- "impotance"  # misspelling is correct

    # Get variable name
    res <- NextRuleFitToken(file)
    ruleVarName <- res$token

    # Get rule 'std'
    res <- NextRuleFitToken(file)
    if (res$token != kStdStr) {
      error(logger, paste("ParseLinearRule: '", support.str, "' token expected, got: ", res$token))
    }
    res <- NextRuleFitToken(file)
    ruleStd <- as.numeric(res$token)
    
    # Get rule 'coefficient'
    res <- NextRuleFitToken(file)
    if (res$token != kCoefficientStr) {
      error(logger, paste("ParseLinearRule: '", kCoefficientStr, "' token expected, got: ", res$token))
    }
    res <- NextRuleFitToken(file)
    ruleCoeff <- as.numeric(res$token)
    
    # Get rule 'importance'
    res <- NextRuleFitToken(file)
    if (res$token != kImportanceStr) {
      error(logger, paste("ParseLinearRule: '", kImportanceStr, "' token expected, got: ", res$token))
    }
    res <- NextRuleFitToken(file)
    ruleImp <- as.numeric(res$token)

    return(list(type="linear", std = ruleStd, coeff = ruleCoeff, imp = ruleImp, var = ruleVarName))
  }

  ParseSplitRule <- function(file, ruleLgth) {
    # Parse and return a "split" rule from the given RuleFit rules text file.
    #
    # Args:
    #        file: an open file or connection
    #    ruleLght: how many vars are in this rule?
    #
    # Returns:
    #        A tuple <type="split", supp, coeff, imp, splits> where 'splits' is a 
    #        list with tuples of the form <type="continuous", var, min, max> or 
    #        <type="categorical", var, cond, levels>
    support.str          <- "support"
    kCoefficientStr      <- "coeff"
    kImportanceStr       <- "importance"
    kCont.split.id.str   <- "range"
    kCateg.split.id1.str <- "in"
    kCateg.split.id2.str <- "not"
    kCateg.missing       <- "0.9000E+31"

    stopifnot(ruleLgth > 0)

    # Get rule 'support'
    res <- NextRuleFitToken(file)
    if (res$token != support.str) {
      error(logger, paste("ParseSplitRule: '", support.str, "' token expected, got: ", res$token))
    }
    res <- NextRuleFitToken(file)
    ruleSupp <- as.numeric(res$token)
    
    # Get rule 'coefficient'
    res <- NextRuleFitToken(file)
    if (res$token != kCoefficientStr) {
      error(logger, paste("ParseSplitRule: '", kCoefficientStr, "' token expected, got: ", res$token))
    }
    res <- NextRuleFitToken(file)
    ruleCoeff <- as.numeric(res$token)
    
    # Get rule 'importance'
    res <- NextRuleFitToken(file)
    if (res$token != kImportanceStr) {
      error(logger, paste("ParseSplitRule: '", kImportanceStr, "' token expected, got: ", res$token))
    }
    res <- NextRuleFitToken(file)
    ruleImp <- as.numeric(res$token)

    # Get splits 
    splits = vector(mode = "list")
    splitVarsSeen <- c()
    iSplit <- 1
    res <- NextRuleFitToken(file)
    while (length(res$token) > 0 && res$token != "Rule") {
      # Get variable name
      splitVarName <- res$token
      # Parse split according to 'type'
      res <- NextRuleFitToken(file)
      if (res$token == kCont.split.id.str) {
        # "Continuous" split... Got range... need min & max
        res <- NextRuleFitToken(file)
        splitRangeMin <- as.numeric(res$token)
        res <- NextRuleFitToken(file)
        splitRangeMax <- as.numeric(res$token)
        split <- list(type=kSplitTypeContinuous, var = splitVarName, min = splitRangeMin, max = splitRangeMax)
      } else if (res$token == kCateg.split.id1.str || res$token == kCateg.split.id2.str) {
        # "Categorical" split... need levels
        if (res$token == kCateg.split.id2.str) {
          categ.cond <- kCateg.split.id2.str
        } else {
          categ.cond <- kCateg.split.id1.str
        }
        # ...skip until the end of the line
        readLines(file, n=1, ok=TRUE)
        # ... soak all levels, assumed to be in one single line
        level.line <- sub("^[ ]+", "", readLines(file, n = 1), perl=T)
        level.list.raw <- strsplit(level.line, "[ ]+", perl=T)[[1]]
        level.list <- c()
        for (iLevel in level.list.raw) {
          if (iLevel == kCateg.missing) {
            level.list <- c(level.list, NA)
          } else {
            level.list <- c(level.list, as.integer(iLevel))
          }
        }
        split <- list(type=kSplitTypeCategorical, var = splitVarName, cond = categ.cond, levels = level.list)
      } else {
        error(logger, paste("ParseSplitRule: One of '", kCont.split.id.str, "', '", kCateg.split.id1.str, "', '",
                            kCateg.split.id2.str, "' token expected, got: ", res$token))
      }
      # Save split
      splits[[iSplit]] <- split
      iSplit <- iSplit + 1

      # Did we get a "new" variable, or one we had seen before?
      if (length(grep(paste("^", splitVarName, "$", sep = ""), splitVarsSeen, perl=T)) == 0) {
        splitVarsSeen <- c(splitVarsSeen, splitVarName)
      }

      # Get next token 
      res <- NextRuleFitToken(file)
    }

    # Check distinct vars found matched input param
    if (length(splitVarsSeen) != ruleLgth) {
      error(logger, paste("ParseSplitRule: ruleLgth = ", ruleLgth, " given... found ", length(splitVarsSeen), " variables!"))
    }

    rule <- list(type="split", supp = ruleSupp, coeff = ruleCoeff, imp = ruleImp, splits = splits)
    return(list(rule=rule, lastToken=res))
  }

  # ----------------------------------------------------------------------- 
  # Open input file
  if (is.character(file)) {
    file <- file(file, "r")
    on.exit(close(file))
  }
  if (!isOpen(file)) {
    open(file, "r")
    on.exit(close(file))
  }

  # Read in the header information
  res <- NextRuleFitToken(file)

  # Read in rules
  rules = vector(mode = "list")
  while (length(res$token) > 0 && res$token == "Rule") {
    # Get rule number
    ruleNum <- as.integer(NextRuleFitToken(file)$token)

    # Parse rule according to 'type'
    res <- NextRuleFitToken(file)
    if (res$token == kRuleTypeLinear) {
      # "Linear" rule
      rule <- ParseLinearRule(file)
      # Get next token 
      res <- NextRuleFitToken(file)
    } else {
      # "Split" rule
      ruleLgth <- res$token
      # ...skip until the end of the line
      readLines(file, n=1, ok=TRUE)
      # ... get rule info
      parseRes <- ParseSplitRule(file, as.integer(ruleLgth))
      rule <- parseRes$rule
      # Get next token... 'ParseSplitRule' already advanced it, so just grab it
      res <- parseRes$lastToken
    }
    rules[[ruleNum]] <- rule
  }
  
  return(rules)
}

SplitRule2Char <- function(rule, x.levels, x.levels.lowcount)
{
  # Turns a 'split' rule into a human 'readable' string.
  #
  # Args:
  #       rule : list of <type="split", supp, coeff, imp, splits= <...>>
  #              tuples, where the split sublist has elements of the form
  #              <type="categorical", var, cond, levels> or
  #              <type="continuous", var, min, max>
  #   x.levels : level info used when rules were built so we can translate
  #              level codes in categorical splits (optional).
  #  x.levels.lowcount : low-count levels collapsed into a single one when
  #                      rules were built (optional).
  # Returns:
  #      A character vector with representation of the rule.
  kSplit.and.str <- "AND"
  stopifnot(length(rule) > 0)
  stopifnot(rule$type == "split") 
  old.o <- options("useFancyQuotes" = FALSE)
  
  ContSplit2Char <- function(split) {
    # Turns a 'continuous' split into a human 'readable' string
    #
    # Args:
    #       split : list of the form <type="continuous", var, min, max>
    #
    # Returns:
    #      A character vector with one of these strings: "var == NA",
    #      "var != NA", "var <= split-value", "var > split-value", or
    #      "var between split-value-low and split-value-high"
    if (split$max == kPlusInf) {
      if (split$min == kMissing) {
        # range = kMissing  plus_inf"  ==>  "== NA"
        str <- paste(split$var, "== NA")
      } else {
        # range = xxx  plus_inf"  ==>  "> xxx"
        str <- paste(split$var, ">", split$min)
      }
    } else if (split$min == kMinusInf) {
      if (split$max == kMissing) {
        # range = kMinusInf  kMissing"  ==>  "!= NA"
        str <- paste(split$var, "!= NA")
      } else {
        # "range = kMinusInf  xxx"  ==>  "<= xxx"
        str <- paste(split$var, "<=", split$max)
      }
    } else if (split$max == kMissing) {
      # range = xxx  kMissing"  ==>  "> xxx"        
      str <- paste(split$var, ">", split$min)
    } else if (split$max != kPlusInf && split$min != kMinusInf &&
             split$max != kMissing && split$min != kMissing) {
      # "range = xxx  yyy"  ==>  ">= xxx and < yyy"
      str <- paste(split$var, ">=", split$min, "and", split$var, "<", split$max)
    } else {
      error(logger, paste("ContSplit2Char: don't know how to print split: ", split))
    }

    return(str)
  }

  CategSplit2Char <- function (split, x.levels, x.levels.lowcount) {
    # Turns a 'categorical' split into a human 'readable' string.
    #
    # Args:
    #       split : list of the form <type="categorical", var, cond, levels>
    #    x.levels : (optional) - {<var.name, var.levels>} list
    #  x.levels.lowcount : (optional) - {<var.name, low count levels>} df
    #
    # Returns:
    #     A character vector with one of these strings: "var IN (level set)",
    #     or "var NOT IN (level set)"
    if (is.null(x.levels)) {
      split.levels.str <- paste(split$levels, collapse=", ")
    } else {
      # Substitute level-code... locate var's possible values
      var.levels <- NULL
      for (iVar in 1:length(x.levels)) {
        if (x.levels[[iVar]]$var == split$var) {
          var.levels <- x.levels[[iVar]]$levels
        }
      }
      if (is.null(var.levels)) {
        error(logger, paste("CategSplit2Char: Failed to find level data for: '", split$var, "'"))
      }
      
      # Replace each level-code by corresponding level-string
      for (iLevel in 1:length(split$levels)) {
        if (is.na(split$levels[iLevel])) {
          level.str <- NA
        } else {
          level.str <- var.levels[split$levels[iLevel]]
          # Is this a factor with recoded levels?
          if (!is.null(x.levels.lowcount)) {
            i.recoded <- grep(paste("^", split$var, "$", sep=""), x.levels.lowcount$var, perl=T)
            if (length(i.recoded) == 1 && level.str == kLowCountLevelsName) {
              low.count.levels <- unlist(x.levels.lowcount$levels[i.recoded])
              level.str <- paste(lapply(low.count.levels, sQuote), collapse=",")
            } else {
              level.str <- sQuote(level.str)
            }
          } else {
            level.str <- sQuote(level.str)
          }
        }
        if (iLevel == 1) {
          split.levels.str <- level.str
        } else {
          split.levels.str <- paste(split.levels.str, level.str, sep = ",")
        }
      }
    }
    
    if (split$cond == kCondIN) {
      str <- paste(split$var, "IN", "(", split.levels.str, ")")
    } else if (split$cond == kCondNotIN) {
      str <- paste(split$var, "NOT IN", "(", split.levels.str, ")")
    } else { 
      error(logger, paste("CategSplit2Char: don't know how to print split: ", split))
    }
  
    return(str)
  }

  # ----------------------------------------------------------------------- 
  splits <- rule$splits
  nSplits <- length(splits)
  stopifnot(nSplits > 0)
  
  # Build string representation of the rule: conjunction of splits
  splitStr <- ""
  for (iSplit in 1:nSplits) {
    split <- splits[[iSplit]]
    if (split$type == kSplitTypeContinuous) {
      if (iSplit == 1) {
        splitStr <- ContSplit2Char(split)
      } else {
        splitStr <- paste(splitStr, kSplit.and.str, ContSplit2Char(split))
      }
    } else if (split$type == kSplitTypeCategorical) {
      if (iSplit == 1) {
        splitStr <- CategSplit2Char(split, x.levels, x.levels.lowcount)
      } else {
        splitStr <- paste(splitStr, kSplit.and.str, CategSplit2Char(split, x.levels, x.levels.lowcount))
      }
    } else {
      error(logger, paste("SplitRule2Char: unknown split type: ", split$type))
    }
  }
  
  options("useFancyQuotes" = old.o)
  return(splitStr)
}

LinearRule2Char <- function(rule)
{
  # Turns a 'linear' rule into a human 'readable' string.
  #
  # Args:
  #    rule : list of the form <type="linear", std, coeff, imp, var>
  #
  # Returns:
  #     A character vector with just the variable name
  if (length(rule) == 0) {
    error(logger, "LinearRule2Char: 'rule' must not be empty")
  }
  if (rule$type != "linear") {
    error(logger, paste("LinearRule2Char: unexpected rule type: ", rule$type))
  }
  # Simply return var name
  return(rule$var)
}

ReadLevels <- function(file)
{
  # Parse and return a list of "levels" for each categorical variable.
  #
  # Args:
  #      file - a file name or an open file or connection
  # Returns:
  #      A list of <variable name, variable levels> pairs
  if (is.character(file)) {
    file <- file(file, "r")
    on.exit(close(file))
  }
  if (!inherits(file, "connection"))
    error(logger, "ReadLevels: argument `file' must be a character string or connection")
  
  if (!isOpen(file)) {
    open(file, "r")
    on.exit(close(file))
  }

  # Read in level info
  levels <- vector(mode = "list")
  iVar <- 1

  while (TRUE) {
    ## Read one line, which has one of these two forms:
    ## varname
    ## varname, level1, level2, ..., leveln
    ## (the varname and levels are optionally quoted)
    v <- scan(file, what = character(), sep = ",", nlines = 1, quiet = TRUE)
    if (length(v) == 0) break
    if (length(v) == 1) {
      levels[[iVar]] <- list(var = v, levels = NULL)
    } else {
      levels[[iVar]] <- list(var = v[1], levels = v[-1])
    }
    iVar <- iVar + 1
  }
  return(levels)
}

PrintRules <- function(rules, x.levels.fname = "", x.levels.lowcount.fname = "", file = "")
{
  # Outputs a 'readable' version of the given RuleFit rules.
  #
  # Args:
  #                   rules : list of <type, supp|std, coeff, imp, splits|var> tuples,
  #                           as generated by the ReadRules() function
  #           x.levels.fname: (optional) - text file with <var.name, var.levels> pairs
  #  x.levels.lowcount.fname: (optional) - text file with <var.name, low-count var.levels> pairs
  #                     file: connection, or a character string naming the file to print 
  #                           to; if "" (the default), prints to the standard output; if
  #                           NULL, prints to a data.frame
  # Returns:
  #          None, or a data.frame with <type, supp.std, coeff, importance> cols
  stopifnot(length(rules) > 0)
  nRules <- length(rules)

  # Were we given data to translate categorical split levels?
  x.levels <- NULL
  x.levels.lowcount <- NULL
  if (nchar(x.levels.fname) > 0) {
    x.levels <- ReadLevels(x.levels.fname)
    if (nchar(x.levels.lowcount.fname) > 0) {
      x.levels.lowcount <- as.data.frame(do.call("rbind", ReadLevels(x.levels.lowcount.fname)))
    }
  }

  if (is.null(file)) {
    # Print to a data-frame instead
    out.df <- data.frame(type = rep(NA, nRules), supp.std = rep(NA, nRules),
                         coeff = rep(NA, nRules), importance = rep(NA, nRules),
                         def = rep(NA, nRules))
  }

  # Print one rule at a time according to type
  for (iRule in 1:nRules) {
    rule <- rules[[iRule]]
    if (is.null(file)) {
      out.df$type[iRule] <- rule$type
      out.df$coeff[iRule] <- rule$coeff
      out.df$importance[iRule] <- rule$imp
    }
    if (rule$type == kRuleTypeLinear) {
      ruleStr <- LinearRule2Char(rule)
      if (is.null(file)) {
        out.df$supp.std[iRule] <- rule$std
        out.df$def[iRule] <- ruleStr
      } else {
        cat(ruleStr, "\n", file = file, append = T)
      }
    } else if (rule$type == kRuleTypeSplit) {
      ruleStr <- SplitRule2Char(rule, x.levels, x.levels.lowcount)
      if (is.null(file)) {
        out.df$supp.std[iRule] <- rule$supp
        out.df$def[iRule] <- ruleStr
      } else {
        cat(ruleStr, "\n", file = file, append = T)
      }
    } else {
      error(logger, paste("PrintRules: unknown rule type: ", rule$type))
    }
  }

  if (is.null(file)) {
    return(out.df)
  }
}
