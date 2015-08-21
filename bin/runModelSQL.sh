#! /bin/sh
#===================================================================================
# FILE: runModelSQL.sh
#
# USAGE: runModelSQL.sh --host --dbIn --tblIn=<Feature table> --pk=<Primary Key>
#                    --model=<Scoring SQL File> 
#                    [--sql=<SQL dialect - "SQLServer", "MySQL", "Netezza", "HiveQL">]
#                    [--dbOut] [--tblOut=<Output table>] 
#                    [--typeOut=<Output type - 1:'score', 2:'rules_only', 3:'rules+score'>]
#                    [--uname=<database username>] [--upw=<database user password>]
#
# DESCRIPTION: Computes predictions using a previously built (and exported) RuleFit 
#              model for a feature table in a database.
#===================================================================================

USAGESTR="usage: runModelSQL.sh --host=<HOST> --dbIn=<Input DB> --tblIn=<Feature Table>
          --pk=<Primary Key> --model=<Scoring SQL File> 
         [--sql=<SQL dialect - SQLServer, MySQL, Netezza, HiveQL>]
         [--dbOut=<Output DB>] [--tblOut=<Output Table>] 
         [--typeOut=<Output type - 1:'score', 2:'rules_only', 3:'rules+score'>]
         [--uname=<database username>] [--upwd=<database user password>]"
RUNMODEL_TEMP_FILE="runModel_temp.sql"
MODELSQLFNAME_SUFFIX1="2_part1"
MODELSQLFNAME_SUFFIX2="2_part2"

# Parse arguments
for i in $*
do
  case $i in
      --host=*)
      HOST=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --dbIn=*)
      INDB=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --tblIn=*)
      FEATTBL=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --pk=*)
      PK=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --model=*)
      MODELSQLFNAME=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --sql=*)
      SQLTYPE=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --dbOut=*)
      OUTDB=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --tblOut=*)
      OUTTBL=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --typeOut=*)
      OUTTYPE=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --uname=*)
      UNAME=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --upwd=*)
      UPWD=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      --verbose=*)
      VERBOSE=`echo $i | sed 's/[-a-zA-Z0-9]*=//'`
      ;;
      *)
      # unknown option
      echo $USAGESTR
      exit 1
      ;;
  esac
done

# Validate command-line arguments
if [ -z "$HOST" -o -z "$INDB" -o -z "$FEATTBL" -o -z "$PK" -o -z "$MODELSQLFNAME" ]; then
    echo $USAGESTR
    exit 1
fi

# Set defaults for unspecified arguments
if [ -z "$OUTDB" ]; then
    OUTDB=${INDB}
fi
if [ -z "$OUTTYPE" ]; then
    OUTTYPE=1
fi
if [ -z "$OUTTBL" ]; then
    if [ $OUTTYPE -eq 1 ]; then
	OUTTBL=${FEATTBL}_scores
    elif [ $OUTTYPE -eq 2 ]; then
	OUTTBL=${FEATTBL}_rules
    elif [ $OUTTYPE -eq 3 ]; then
	OUTTBL=${FEATTBL}_scores
    else 
	echo "Invalid typeOut value: "$OUTTYPE
	exit 1
    fi
fi
if [ -z "$VERBOSE" ]; then
    VERBOSE=1
fi

set_delete_query()
{
    if [ "$1" = "SQLServer" ]; then
	DELETE_QUERY="
          IF OBJECT_ID('${OUTTBL}') IS NOT NULL
            DROP TABLE ${OUTTBL}
          ;
          IF OBJECT_ID('${OUTTBL}_TEMP') IS NOT NULL
            DROP TABLE ${OUTTBL}_TEMP
        "
    elif [ "$1" = "MySQL" -o "$1" = "HiveQL" ]; then
	DELETE_QUERY="
          DROP TABLE IF EXISTS ${OUTDB}.${OUTTBL};
          DROP TABLE IF EXISTS ${OUTDB}.${OUTTBL}_TEMP;
        "
    elif [ "$1" = "Netezza" ]; then
        DELETE_QUERY1="
          DROP TABLE ${OUTTBL};
        "
        DELETE_QUERY2="
          DROP TABLE ${OUTTBL}_TEMP;
        "
    else
	echo "Invalid SQLtype value: "$1
	exit 1
    fi
    return 0
}

set_score_query()
{
    if [ "$1" = "SQLServer" ]; then
	SCORE_QUERY="
          SET NOCOUNT ON;
          SELECT 
             ${PK} = LIST.${PK}
            ,score = ${SQLCLAUSE}
          INTO ${OUTDB}..${OUTTBL}
          FROM ${INDB}..${FEATTBL} LIST
          ;
          -- Add index
          CREATE UNIQUE INDEX IX_${OUTTBL}_${PK} ON ${OUTTBL} (${PK})
        "
    elif [ "$1" = "MySQL" ]; then
	SCORE_QUERY="
          CREATE TABLE ${OUTDB}.${OUTTBL} (
             id INT
            ,score DOUBLE
          );
          INSERT INTO ${OUTDB}.${OUTTBL}
          SELECT 
             LIST.${PK}
            ,${SQLCLAUSE}
          FROM ${INDB}.${FEATTBL} LIST
          ;          
        "
    elif [ "$1" = "HiveQL" ]; then
	SCORE_QUERY="
          CREATE TABLE ${OUTDB}.${OUTTBL} (
             id INT
            ,score DOUBLE
          );
          INSERT INTO TABLE ${OUTDB}.${OUTTBL}
          SELECT 
             LIST.${PK}
            ,${SQLCLAUSE}
          FROM ${INDB}.${FEATTBL} LIST
          ;          
        "
    elif [ "$1" = "Netezza" ]; then
	SCORE_QUERY=""
    else
	echo "Invalid SQLtype value: "$1
	exit 1
    fi
    return 0
}

set_rules_only_query()
{
    if [ "$1" = "SQLServer" ]; then
	RULES_ONLY_QUERY="
          SET NOCOUNT ON;
          SELECT 
             ${PK} = LIST.${PK}
            ,${SQLCLAUSE}
          INTO ${OUTDB}..${OUTTBL}
          FROM ${INDB}..${FEATTBL} LIST
          ;
          -- Add index
          CREATE UNIQUE INDEX IX_${OUTTBL}_${PK} ON ${OUTTBL} (${PK})
        "
    elif [ "$1" = "MySQL" ]; then
	RULES_ONLY_QUERY="
          CREATE TABLE ${OUTDB}.${OUTTBL} AS
          SELECT 
             LIST.${PK}
            ,${SQLCLAUSE}
          FROM ${INDB}.${FEATTBL} LIST
          ;          
        "
    elif [ "$1" = "Netezza" ]; then
	RULES_ONLY_QUERY=""
    else
	echo "Invalid SQLtype value: "$1
	exit 1
    fi
    return 0
}

set_rules_score_query()
{
    if [ "$1" = "SQLServer" ]; then
	RULES_SCORE_QUERY="
          SET NOCOUNT ON;
          SELECT 
             ${PK} = LIST.${PK}
            ,${SQLCLAUSE}
          INTO ${OUTDB}..${OUTTBL}_TEMP
          FROM ${INDB}..${FEATTBL} LIST
          ;
          SELECT  
            ${PK}
           ,score = ${SQLSUMCLAUSE_PART1}
          INTO ${OUTDB}..${OUTTBL}
          FROM (
            SELECT
              ${PK}
             ,${SQLSUMCLAUSE_PART2}
            FROM ${OUTDB}..${OUTTBL}_TEMP)T
          ;
        "
    elif [ "$1" = "MySQL" ]; then
	RULES_SCORE_QUERY=""
    elif [ "$1" = "Netezza" ]; then
	RULES_SCORE_QUERY="
          SELECT 
             LIST.${PK} AS ${PK}
            ,${SQLCLAUSE}
          INTO ${OUTDB}..${OUTTBL}_TEMP
          FROM ${INDB}..${FEATTBL} LIST
          ;
          SELECT  
            ${PK}
           ,${SQLSUMCLAUSE_PART1} AS score
          INTO ${OUTDB}..${OUTTBL}
          FROM (
            SELECT
              ${PK}
             ,${SQLSUMCLAUSE_PART2}
            FROM ${OUTDB}..${OUTTBL}_TEMP
            LIMIT ALL)T
          ;
        "
    else
	echo "Invalid SQLtype value: "$1
	exit 1
    fi
    return 0
}

set_diag_query()
{
    if [ "$1" = "SQLServer" ]; then
	DIAG_QUERY="
          SET ANSI_WARNINGS OFF;
          SET NOCOUNT ON;
          SELECT 
             MIN    = MIN(score)
            ,AVG    = AVG(CAST(score AS BIGINT))
            ,MAX    = MAX(score)
            ,nNULLs = SUM(CASE
                            WHEN score IS NULL THEN 1
                            ELSE 0
                          END)
          FROM ${OUTDB}..${OUTTBL}
        "
    elif [ "$1" = "MySQL" ]; then
	DIAG_QUERY="
          SELECT 
             MIN(score) AS MIN
            ,AVG(score) AS AVG
            ,MAX(score) AS MAX
            ,SUM(CASE
                  WHEN score IS NULL THEN 1
                  ELSE 0
                END) AS nNULLs
          FROM ${OUTDB}.${OUTTBL}      
        "
    elif [ "$1" = "HiveQL" ]; then
	DIAG_QUERY="
          SELECT 
             MIN(score) AS MIN
            ,AVG(CAST(score AS BIGINT)) AS AVG
            ,MAX(score) AS MAX
            ,SUM(CASE
                  WHEN score IS NULL THEN 1
                  ELSE 0
                END) AS nNULLs
          FROM ${OUTDB}.${OUTTBL}      
        "
    elif [ "$1" = "Netezza" ]; then
	DIAG_QUERY=""
    else
	echo "Invalid SQLtype value: "$1
	exit 1
    fi
    return 0
}

# Load SQL clause expression
SQLCLAUSE=`cat ${MODELSQLFNAME}`
if [ ${#SQLCLAUSE} -lt 1 ]; then
    echo "Empty/missing scoring SQL..."
    exit 1
fi

# Drop output table if it already exists
set_delete_query $SQLTYPE
if [ $VERBOSE -ge 2 ]; then
    echo ${DELETE_QUERY}; echo
fi
if [ $VERBOSE -ne 100 ]; then
    if [ $SQLTYPE = "SQLServer" ]; then
	sqlcmd -E -S $HOST -d $OUTDB -Q "${DELETE_QUERY}"
    elif [ $SQLTYPE = "MySQL" ]; then
	mysql -h $HOST -u $UNAME -p$UPWD -e "${DELETE_QUERY}"
    elif [ $SQLTYPE = "HiveQL" ]; then
	hive -S -e "${DELETE_QUERY}"
    elif [ $SQLTYPE = "Netezza" ]; then
        nzsql -h $HOST -db $OUTDB -u $UNAME -pw $UPWD -c "select * from _V_RELATION_COLUMN where NAME=UPPER('${OUTTBL}')" -A -o temp.txt
        if [ `grep -c "(0 rows)" temp.txt` -eq 0 ]; then
            nzsql -h $HOST -db $OUTDB -u $UNAME -pw $UPWD -c "${DELETE_QUERY1}"
        fi
        rm temp.txt
        nzsql -h $HOST -db $OUTDB -u $UNAME -pw $UPWD -c "select * from _V_RELATION_COLUMN where NAME=UPPER('${OUTTBL}_TEMP')" -A -o temp1.txt
        if [ `grep -c "(0 rows)" temp1.txt` -eq 0 ]; then
            nzsql -h $HOST -db $OUTDB -u $UNAME -pw $UPWD -c "${DELETE_QUERY2}"
        fi
        rm temp1.txt
    else
	echo "Invalid SQLtype value: "$SQLTYPE
	exit 1
    fi
fi

# Load "score = t0+t1+..." clauses (if needed)
if [ $OUTTYPE -eq 3 ]; then
    MODELSQLFNAME2=${MODELSQLFNAME}${MODELSQLFNAME_SUFFIX1}
    SQLSUMCLAUSE_PART1=`cat ${MODELSQLFNAME2}`
    MODELSQLFNAME2=${MODELSQLFNAME}${MODELSQLFNAME_SUFFIX2}
    SQLSUMCLAUSE_PART2=`cat ${MODELSQLFNAME2}`
fi

# Instantiate query
if [ $OUTTYPE -eq 1 ]; then
    set_score_query $SQLTYPE
    QUERY=${SCORE_QUERY}
elif [ $OUTTYPE -eq 2 ]; then
    set_rules_only_query $SQLTYPE
    QUERY=${RULES_ONLY_QUERY}
else
    set_rules_score_query $SQLTYPE
    QUERY=${RULES_SCORE_QUERY}
fi
if [ $VERBOSE -ge 2 ]; then
    echo ${QUERY}; echo
fi
# ... write SQL to a temp file
echo ${QUERY} > ${RUNMODEL_TEMP_FILE}

# Execute query
if [ $VERBOSE -ne 100 ]; then
    if [ $SQLTYPE = "SQLServer" ]; then
        # sqlcmd -E -S $HOST -d $OUTDB -Q "${QUERY}"
	sqlcmd -E -S $HOST -d $OUTDB -i ${RUNMODEL_TEMP_FILE}
    elif [ $SQLTYPE = "MySQL" ]; then
	mysql -h $HOST -u $UNAME -p$UPWD < ${RUNMODEL_TEMP_FILE}
    elif [ $SQLTYPE = "HiveQL" ]; then
	hive -S -f "${RUNMODEL_TEMP_FILE}"
	exit 1
    elif [ $SQLTYPE = "Netezza" ]; then
	echo "Create output table"
	nzsql -host $HOST -db $OUTDB -u $UNAME -pw $UPWD -f ${RUNMODEL_TEMP_FILE}
    else
	echo "Invalid SQLtype value: "$SQLTYPE
	exit 1
    fi
fi

# Diagnostic queries
if [ $VERBOSE -ge 1 ] && [ $OUTTYPE -eq 1 ]; then
    set_diag_query $SQLTYPE
    echo ""
    if [ $VERBOSE -ne 100 ]; then
	if [ $SQLTYPE = "SQLServer" ]; then
	    sqlcmd -E -S $HOST -d $OUTDB -Q "${DIAG_QUERY}"
	elif [ $SQLTYPE = "MySQL" ]; then
	    mysql -h $HOST -u $UNAME -p$UPWD -e "${DIAG_QUERY}"
	elif [ $SQLTYPE = "HiveQL" ]; then
	    hive -S -e "${DIAG_QUERY}"
	    exit 1
	elif [ $SQLTYPE = "Netezza" ]; then
	    echo "Don't know how to run: "$SQLTYPE
	    exit 1
	else
	    echo "Invalid SQLtype value: "$SQLTYPE
	    exit 1
	fi
    else
	echo ${DIAG_QUERY}; echo
    fi
fi

# Print out number of rows processed
if [ $VERBOSE -ne 100 ]; then
    if [ $SQLTYPE = "SQLServer" ]; then
	nco=`sqlcmd -E -S $HOST -d $OUTDB -Q "SELECT COUNT(*) NumRows FROM ${OUTTBL}" | awk 'NR==3{print $1;}'`
    elif [ $SQLTYPE = "MySQL" ]; then
	nco=`mysql -h $HOST -u $UNAME -p$UPWD -e "SELECT COUNT(*) NumRows FROM ${OUTDB}.${OUTTBL}" | perl -ane '$. > 1 && print'`
    elif [ $SQLTYPE = "HiveQL" ]; then
	nco=`hive -S -e "SELECT COUNT(*) NumRows FROM ${OUTDB}.${OUTTBL}" | perl -ane '$. > 1 && print'`
    elif [ $SQLTYPE = "Netezza" ]; then
	nco=`nzsql -host $HOST -db $OUTDB -u $UNAME -pw $UPWD -c "SELECT COUNT(*) NumRows FROM ${OUTTBL}" | awk 'NR==3{print $1;}'`
    fi
    echo "Processed "$nco" rows..."
fi

exit 0
