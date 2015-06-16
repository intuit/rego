# Rego Partial Dependence Plot Configuration File
| parameter  | Value|
| -------------- |:-----:|
| var.name | Name of variable to be considered. |
| ## *Partial Dependence plot control*  || 
| num.obs | Number of observations to include in averaging calculation (default is 500).|
| var.num.values | Number of distinct variable evaluation points (default is 200).|
| var.trim.qntl | Trim extreme values of variable (default is 0.025) |
| var.rug.qntl | Rug quantile to show numeric variable data density (default is 0.1). |
| var.levels.las |Text orientation of level names (for categorical variable). Default is 1. |
| show.pdep.dist | Show partial dependence distribution (default is 0 -- i.e FALSE)|
| show.yhat.mean | Show partial dependence mean|
| var.boxplot.range | This determines how far the whiskers of a categorical variable extend out from the boxplot's box (this value times interquartile range gives whiskers range). Default is 1.0e-4.|
| ## *Output*  || 
| out.path | |
| out.fname | output file name (default is var.name.PNG) |
| ## *Model and installation info*  || 
| model.path | |
| rf.working.dir | path to working directory where model will be saved to. If not specified, an attempt to read environment variable RF_WORKING_DIR will be made.| 
|log.level | one of **kLogLevelDEBUG**, **kLogLevelINFO**, **kLogLevelWARNING**, **kLogLevelERROR**, **kLogLevelFATAL**, **kLogLevelCRITICAL**. Controls the verbosity of the logging messages|



