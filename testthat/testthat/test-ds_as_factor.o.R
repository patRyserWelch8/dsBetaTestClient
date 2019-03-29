context("return the expected character factor")
test_that("the factor are the same from the R and datashield",
{
    factor.from.file <- levels(as.factor(ds.test_env$same.values[,14]))
    factor.from.server <- ds.asFactor("D$FACTOR_CHARACTER",datasources = ds.test_env$connection.opal)
    print(factor.from.server)
})




