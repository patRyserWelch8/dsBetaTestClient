context("Returns expected numerical value")
test_that("combined data set",
{
    factor.from.file <- list(levels(as.factor(ds.test_env$same.values[,15])))
    factor.from.server <- ds.asFactor.o('D$FACTOR_INTEGER','FACTOR_INT.f', datasources = ds.test_env$connection.opal)
    print(class(factor.from.server[1]))
    print(class(factor.from.file))
    expect_true(length(setdiff(factor.from.file, factor.from.server[1]))==0)
    
})




