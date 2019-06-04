source("connection_to_datasets/init_all_datasets.R")

context("<START>ds.recodeValues.o</START>")
context("Returns expected numerical value")
test_that("combined data set",
{
   connect.all.datasets()
   factor.from.file <- list(levels(as.factor(ds.test_env$local.values[,15])))
   factor.from.server <- ds.asFactor.o('D$FACTOR_INTEGER','FACTOR_INT.f', datasources = ds.test_env$connection.opal)
   expect_true(length(setdiff(factor.from.file, factor.from.server[1]))==0)
})
