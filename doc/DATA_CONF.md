# Rego Data Configuration File

| parameter  | Value|
| -------------- |:-----:|
| ## Data source     | where is the data coming from | 
| data.source.type | can be **db** (database) or **csv** (comma-separated values file) or **rdata** (RData file save()'d by R) |
| ## ...CSV spec| |
|csv.path |  specifies the location of the cdv file, when the data source type is **csv**|
| csv.fname |  csv file name|
| csv.sep | the field separator character (optional) |
| ## ...RData spec| |
| rdata.path| specifies the location of the RData file, when the data source type is **rdata**|
| rdata.fname| RData file name|
| rdata.dfname|  specifies the name of the data-frame object in the RData file. Can be omitted if there is only one object in the RData file. |
| ## ...DB spec| |
| db.dsn | |
| db.name| |
| db.tbl.name| |
| db.tbl.maxrows| |
| db.query.tmpl | a SQL query "template" file to use when fetching the data -- e.g., "SELECT * FROM _TBLNAME_ WHERE Y IN (0, 1) LIMIT _MAXROWS_ "|
| ## Column specs| |
| col.types.fname | a text file with **column name, column type** pairs (column type is **1** for continuous, and **2** for categorical variables). Can be omitted if the data source type is **rdata**, in which case columns inheriting from factor will be treated as categorical|
| col.y | name of response variable.|
| col.id | name of row-id column (optional). Often useful during prediction when <id, y, yHat> tuples are generated. |
| col.weights | name of weights column (optional). |
|row.weights.fname | name of text file with customized weights for each row (no header, one weight per line) |
| col.skip.fname| name of text file listing columns to skip (like a row-id column); one column name per line (optional)|
| col.winz.fname|  text file with column-specific [winsorizing](http://en.wikipedia.org/wiki/Winsorising) parameters (optional).|
| ## Any preprocessing| |
| na.threshold | maximum fraction of NA values to allow per column (optional).|
| min.level.count | levels with fewer than this count will be merged.|
| do.class.balancing| set to 1 to have classes to be equally weighted; 0 otherwise.|
| ## HTML model report| |
| html.fname | file where to write model summary as an HTML report.|
| html.title | |
| html.title2 | |
| html.min.var.imp | exclude from HTML report variables with importance score lower than this.|
| html.min.rule.imp | exclude from HTML report rules with importance score lower than this.|
| ## Other| |
| rand.seed| random number seed |
|log.level | one of **kLogLevelDEBUG**, **kLogLevelINFO**, **kLogLevelWARNING**, **kLogLevelERROR**, **kLogLevelFATAL**, **kLogLevelCRITICAL**. Controls the verbosity of the logging messages |
