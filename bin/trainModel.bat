@echo off 
REM Provides a "batch" interface to the RuleFit statistical model building
REM program. To invoke it use:
REM
REM     %REGO_HOME%/bin/trainModel.bat DATA.conf MODEL.conf [LOGGER.txt]
REM   
REM You need to set two environment variables: 
REM - RS_PATH: path to R installation where R's Rscript is installed
REM            E.g., set RS_PATH=D:/R/R-2.15.1/bin/x64
REM - REGO_HOME: path to where the REgo scripts are located
REM         E.g., set REGO_HOME=D:/rego

IF %3.==. (
  %RS_PATH%/Rscript %REGO_HOME%/src/rfTrain_main.R -d %1 -m %2
) ELSE (
  %RS_PATH%/Rscript %REGO_HOME%/src/rfTrain_main.R -d %1 -m %2 -l %3
)
