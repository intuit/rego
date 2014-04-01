#! /bin/sh
#===================================================================================
# FILE: runModel.sh
#
# USAGE: runModel.sh --m=<model path> --d=<Data spec file>
#
# DESCRIPTION: Computes predictions using a previously built RuleFit model on the
#              specified data.
#===================================================================================

USAGESTR="usage: runModel.sh --m=<Model path> --d=<Data spec file>"

# Parse arguments
for i in $*
do
  case $i in
      --m=*)
      MODEL_PATH=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --d=*)
      DATA_CONF=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      *)
      # unknown option
      echo $USAGESTR
      exit 1
      ;;
  esac
done

# Validate command-line arguments
if [ -z "$MODEL_PATH" -o -z "$DATA_CONF" ]; then
    echo $USAGESTR
    exit 1
fi

# Invoke R code
$REGO_HOME/src/rfPredict_main.R -m ${MODEL_PATH} -d ${DATA_CONF}

