Rego (Rule Ensembles Go!)
=========================

Rego provides a command-line batch interface to the [RuleFit](http://statweb.stanford.edu/~jhf/R_RuleFit.html) statistical model building program, making it possible to:

* Declaratively train and run a model using ensemble methods
* Export a model as production-ready SQL (for MySQL, SQLServer, Hive, and Netezza)
* Incorporate best practices for data pre-processing and model interpretation
* Provide a detailed, human-readable model explanation in HTML

Rego uses RuleFit, Professor Jerome Friedman's implementation of Rule Ensembles, an interpretable type of ensemble model where the base-learners consists of conjunctive rules derived from decision trees.

Rego is sponsored by [Intuit, Inc](http://intuit.com), and developed and maintained in Intuit's Data Enginering and Analytics group (IDEA) by Dr. Giovanni Seni.

## What is Rego?

Predictive learning plays an important role in many areas of science, finance and industry. Here are some examples of learning problems:

* Predict whether a customer would be attracted to a new service offering. Recognizing such customers can reduce the cost of a campaign by reducing the number of contacts.
* Predict whether a web site visitor is unlikely to become a customer. The prediction allows prioritization of customer support resources.
* Identify the risk factors for churn, based on the content of customer support messages.

Rego is a collection of R-based scripts intended to facilitate the process of building, interpreting, and deploying state-of-art predictive learning models. Rego can:

* Enable rapid experimentation
* Increase self-service capability
* Support easy model deployment into a production environment


Under the hood Rego uses [RuleFit](http://statweb.stanford.edu/~jhf/R_RuleFit.html), a statistical model building program created by Prof. Jerome Friedman. RuleFit was written in Fortran but has an R interface. RuleFit implements a model building methodology known as ["ensembling"](http://www.amazon.com/Ensemble-Methods-Data-Mining-Predictions/dp/1608452840), where multiple simple models (base learners) are combined into one usually more accurate than the best of its components. This type of model can be described as an additive expansion of the form:

F(x) = a<sub>0</sub> + a<sub>1</sub>*b<sub>1</sub>(x) + a<sub>2</sub>*b<sub>2</sub>(x) + ... + a<sub>M</sub>*b<sub>M</sub>(x) where the b<sub>j</sub>(x)'s are the base-learners.

In the case of RuleFit, the b<sub>j</sub>(x) terms are conjunctive rules of the form 

if x<sub>1</sub> > 22 and x<sub>2</sub> > 27 then 1 else 0

or linear functions of a single variable -- e.g., b<sub>j</sub>(x) = x<sub>j</sub>. 

Using base-learners of this type is attractive because they constitute easily interpretable statements about attributes x<sub>j</sub>. They also preserve the desirable characteristics of Decision Trees such as easy handling of categorical attributes, robustness to outliers in the distribution of x, etc.

RuleFit builds model F(x) in a three-step process:

1. build a tree ensemble (one where the b<sub>j</sub>(x)'s are decision trees), 
2. generate candidate rules from the tree ensemble, and 
3. fit coefficients a<sub>j</sub> via regularized regression.

Rego consists of additional R code that we've written to make working with RuleFit easier, including:

* The ability to have multiple rulefit batch jobs running simultaneously
* Easily specifying a data source
* Automatically executing common preprocessing operations
* Automatically generating a model summary report with interpretation plots and quality assessment
* Exporting a model from R to SQL for deployment in a production environment


## Getting Started

### Dependencies

Install [R](http://cran.us.r-project.org/) and the following R packages: R2HTML, ROCR, RODBC, getopt

### Environment variables

* ```REGO_HOME```: environment variable pointing to where you have checked out the Rego code
* ```RF_HOME```: environment variable pointing to appropriate RuleFit executable -- e.g., export ```RF_HOME=$REGO_HOME/lib/mac```

### RuleFit binaries

Before using Rego, you must download the [RuleFit](http://statweb.stanford.edu/~jhf/R_RuleFit.html) binary appropriate to your platform:

#### Windows (64-bit)

Place the following files in ```$REGO_HOME/lib/RuleFit/windows/```

* [rf_go.exe for Windows](http://statweb.stanford.edu/~jhf/r-rulefit/rulefit3/windows/windows64/rf_go.exe)
* [move.bat](http://statweb.stanford.edu/~jhf/r-rulefit/rulefit3/windows/move.bat)

#### Linux (64-bit)

Place the following file in ```$REGO_HOME/lib/RuleFit/linux/```

* [rf_go.exe for Linux](http://statweb.stanford.edu/~jhf/r-rulefit/rulefit3/linux/linux64/rf_go.exe)

Run ```chmod u+x ${REGO_HOME}/lib/RuleFit/linux/rf_go.exe``` to make the file executable.

#### Mac (64-bit)

Place the following file in ```$REGO_HOME/lib/RuleFit/mac/```

* [rf_go.exe for Mac OS X](http://statweb.stanford.edu/~jhf/r-rulefit/rulefit3/mac/mac64/rf_go.exe)

Run ```chmod u+x $REGO_HOME/lib/RuleFit/mac/rf_go.exe``` to make the file executable.

## Commands

### Build a model

* ```$REGO_HOME/bin/trainModel.sh --d=DATA.conf --m=MODEL.conf [--l LOG.txt]```
* Input:
    * ```DATA.conf```: data configuration file specifying options such as where the data is coming from, what column corresponds to the target, etc.
    * ```MODEL.conf```: model configuration file specifying options such as the type of model being fit, the criteria being optimized, etc.
    * ```LOG.txt```: optional file name where to write logging messages
* Output:
    * ```model_summary.html```: model summary and assessment
    * ```model_singleplot.html```: interpretation plots
    * ```<Model definition files>```: for later export or prediction

### Export a Model

* ```$REGO_HOME/bin/exportModel.sh --m=MODEL.dir [--c=EXPORT.conf]```
* Input
    * ```MODEL_DIR```: path to model definition files
    * ```EXPORT.conf```: the configuration file specifying export options such as desired sql dialect, type of scoring clause, etc.
* Output:
    * ```SQL_FILE```: output file name containing model as a SQL expression

### Predict on New Data

* ```$REGO_HOME/bin/runModel.sh --m=MODEL.dir --d=DATA.conf```
* Input:
    * ```MODEL_DIR```: path to model definition files
    * ```DATA.conf```: specifies test data location
* Output:
    * Text file with ```<id, y, yHat>``` tuples
    
### Deploy a Model

* ```$REGO_HOME/bin/runModelSQL.sh --host --dbIn --tblIn=<Feature table> --pk=<Primary Key> --model=<Model Definition SQL File> --dbOut --tblOut=<Score table>```
* Input
    * ```dbIn.tblIn```: new data to be scored
    * ```model```: previously built (and exported) model
* Output:
    * ```dbOut.tblOut```: Computed scores


## Examples

These examples show how to use a CSV or RData file as a data source, using the R diamonds dataset.

The CSV and RData files were created in R as follows:

    X <- ggplot2::diamonds
    write.csv(X, file = "diamonds.csv", na = "", row.names = FALSE)
    save(X, file = "diamonds.RData")

CSV:

    # training
    $REGO_HOME/bin/trainModel.sh --d=data_csv.conf --m=model.conf
    
    # prediction on csv data file
    $REGO_HOME/bin/runModel.sh  --m=/tmp/REgo/Diamonds_wd/export --d=predict_csv.conf
    
    # export model to SQL    $REGO_HOME/bin/exportModel.sh --m=/tmp/REgo/Diamonds_wd/export --c=$REGO_HOME/conf/EXPORT.conf   
    # prediction on db table
    $REGO_HOME/bin/runModelSQL.sh --host=  --dbIn=  --tblIn=diamond_test --pk=id --model=rules_forSQL.txt --sql=HiveQL --typeOut=1
    
RData:

    # training
    $REGO_HOME/bin/trainModel.sh --d=data_rdata.conf --m=model.conf

    # prediction or RData data file
    $REGO_HOME/bin/runModel.sh  --m=/tmp/REgo/Diamonds_wd/export --d=predict_rdata.conf
    
    
    
#### Warning!

If using RData as the data source type for either training or prediction, then it must be used for both. This is because the order of factor levels may be different for the two source types.


## Project Details

* Author: Giovanni Seni
* Maintainer: Giovanni Seni <Giovanni_Seni@intuit.com>
* License: Eclipse Public License 1.0
