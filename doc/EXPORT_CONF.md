# Rego Model Export Configuration File
| parameter  | Value|
| -------------- |:-----:|
| do.dedup | Do we need to check for duplicates in the rule set and merge them? Default is **1** (true).| 
| expand.lcl.mode | How do we expand low-count levels. Can be **1** to replace the *\_LowCountLevels\_* constant in the SQL scoring expression with the corresponding levels, or **2** to keep the *\_LowCountLevels\_* constant within the SQL scoring expression (which keeps it shorter and easier to read), and generate an extra SQL clause with logic to substitute low count levels *_LowCountLevels_* in a data preparation step prior to scoring |
| out.type| can be **score** (generates scoring clause -- i.e., a<sub>0</sub> + a<sub>1</sub>*b<sub>1</sub>(x) + a<sub>2</sub>*b<sub>2</sub>(x) + ...), **rulesonly** (generates rules clauses without coefficients -- i.e.,*b<sub>1</sub>(x), *b<sub>2</sub>(x), ...; useful to monitor the firing pattern of each individual rule), or **rulescoeff** (generates rules clauses with coefficients|
| sql.dialect| One of **SQLServer**, **HiveQL**, **Netezza**, or **MySQL** (default is **SQLServer**)|
| max.sqle.length | Max sql expression length, in number of characters (some sql engine have a limit on this)) |
| log.level | one of **kLogLevelDEBUG**, **kLogLevelINFO**, **kLogLevelWARNING**, **kLogLevelERROR**, **kLogLevelFATAL**, **kLogLevelCRITICAL**. Controls the verbosity of the logging messages |
| out.fname | Output file name (default is "rules_forSQL.txt")|
