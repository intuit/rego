#! /bin/sh
#===================================================================================
# FILE: trainModel.sh
#
# USAGE: trainModel.sh --d=<Data spec file> --m=<Model config file>
#
# DESCRIPTION: Builds a RuleFit model with the given data source and model spec files.
#===================================================================================

USAGESTR="usage: trainModel.sh --d=<Data spec file> --m=<Model config file>"

# Parse arguments
for i in $*
do
  case $i in
      --d=*)
      DATA_CONF=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --m=*)
      MODEL_CONF=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      *)
      # unknown option
      echo $USAGESTR
      exit 1
      ;;
  esac
done

# Validate command-line arguments
if [ -z "$DATA_CONF" -o -z "$MODEL_CONF" ]; then
    echo $USAGESTR
    exit 1
fi

# Invoke R code
$REGO_HOME/src/rfTrain_main.R -d ${DATA_CONF} -m ${MODEL_CONF}
