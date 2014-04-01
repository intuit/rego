#! /bin/sh
#===================================================================================
# FILE: exportModel.sh
#
# USAGE: exportModel.sh --m=<model path> [--c=<Export config file] 
#
# DESCRIPTION: Exports a previously built RuleFit model to SQL.
#===================================================================================

USAGESTR="usage: exportModel.sh --m=<Model path> [--c=<Export config options file>]"

# Parse arguments
for i in $*
do
  case $i in
      --m=*)
      MODEL_PATH=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --c=*)
      EXPORT_CONF=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      *)
      # unknown option
      echo $USAGESTR
      exit 1
      ;;
  esac
done

# Validate command-line arguments
if [ -z "$MODEL_PATH" ]; then
    echo $USAGESTR
    exit 1
fi

# Invoke R code
if [ -z "$EXPORT_CONF" ]; then
    $REGO_HOME/src/rfExportSQL_main.R -m ${MODEL_PATH}
else
    $REGO_HOME/src/rfExportSQL_main.R -m ${MODEL_PATH} -c ${EXPORT_CONF}
fi
