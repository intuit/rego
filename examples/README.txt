Examples showing how to use a CSV or RData file as data source, using
the R diamonds dataset.

The CSV and RData files were created in R as follows:

    X <- ggplot2::diamonds
    write.csv(X, file = "diamonds.csv", na = "", row.names = FALSE)
    save(X, file = "diamonds.RData")


CSV:
    $REGO_HOME/bin/trainModel.sh --d=data_csv.conf --m=model.conf    # training
    $REGO_HOME/bin/runModel.sh  --m=/tmp/REgo/Diamonds_wd/export --d=predict_csv.conf   # prediction
    
RData:
    $REGO_HOME/bin/trainModel.sh --d=data_rdata.conf --m=model.conf   # training
    $REGO_HOME/bin/runModel.sh  --m=/tmp/REgo/Diamonds_wd/export --d=predict_rdata.conf   # prediction


*WARNING*:

If using rdata as the data source type for either training or
prediction, then it must be used for both.  This is because the order
of factor levels may be different for the two source types.
