source("connection_to_datasets/init_all_datasets.R")



#context("<START>ds.recodeValues.o</START>")
context("ds.recodeValues.o()::mathematical::Returns expected numerical value")
test_that("combined data set",
{
   connect.all.datasets()
   #change value locally start
   row <- sample(nrow(ds.test_env$local.values),1)
   value <- ds.test_env$local.values[row,6]

   ds.test_env$local.values[row,6] <- 0

   #change value locally end
   #change value on the server
   #a <- ds.recodeValues.o('D$FACTOR_INTEGER',c(0,1,2),c(20,27,37),newobj = 'INTEGER_recoded',datasources = connection.opal)
   recode.from.server <- ds.recodeValues.o('D$INTEGER',value,c(0),newobj = 'INTEGER_recoded',datasources = ds.test_env$connection.opal)
   factor.from.server <- ds.asFactor.o('INTEGER_recoded','INT.f', datasources = ds.test_env$connection.opal)
   factors <- factor.from.server[[1]]

   expect_true(length(factors[factors == "0"])> 0)

})


# context("<END>ds.recodeValues.o</END>")
