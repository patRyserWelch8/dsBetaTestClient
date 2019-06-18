source("connection_to_datasets/init_all_datasets.R")

.calc.distribution.locally <- function(some.local.values, column)
{
  value <- c(0,0)
  if(column <= ncol(some.local.values)) 
  {
     value[0] <- mean(some.local.values[,column])
     value[1] <- var(some.local.values[,column])
  }
  return(value)
}

.calc.distribution.server <- function(name.variable)
{
  value <- c(0,0)
  mean.from.servers <- ds.mean.o(x=name.variable,type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  var.from.servers <- ds.var.o(x=name.variable,type='combine',   datasources=ds.test_env$connection.opal)
  value[0] <- mean.from.servers[[1]][1]
  value[1] <- var.from.servers[[1]][1]
  return(value)
}


.apply.changes.locally <- function(some.local.values, a.value.to.replace, column) 
{
  row <- sample(nrow(some.local.values),1) 
  value <- some.local.values[row,column] 
  some.local.values[row,column] <- a.value.to.replace
  return(value)
}

.apply.changes.server <- function(name.variable,name.variable.recoded,values,values.to.replace)
{
  recode.from.server <- ds.recodeValues.o(name.variable,values,values.to.replace,newobj = name.variable.recoded,datasources = ds.test_env$connection.opal)

}

.complete.expected.value.test <- function(variable.name, variable.recoded, some.values, column,value.to.replace)
{
  dist.local.original <- .calc.distribution.locally(some.values,column)
  dist.server.original <- .calc.distribution.server(variable.name)
  value <- .apply.changes.locally(some.values,value.to.replace,column)   #change value locally start by randomly selecting a value locally
  .apply.changes.server(variable.name,variable.recoded, value, value.to.replace)
  dist.local.recoded <- .calc.distribution.locally(some.values,column)
  dist.server.recoded <- .calc.distribution.server(variable.name)
  
  expect_equal(dist.local.original[0],dist.server.original[0], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local.original[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local.recoded[0],dist.server.recoded[0], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local.recoded[1],dist.server.recoded[1], tolerance = ds.test_env$tolerance)
  
}

context("<START>ds.recodeValues.o</START>")
context("Returns expected numerical values")
test_that("combined data set",
{
   connect.all.datasets()
   .complete.expected.value.test('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,100)
   .complete.expected.value.test('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,0)
   .complete.expected.value.test('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7,-100)
   .complete.expected.value.test('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,0)
   .complete.expected.value.test('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,100)
   .complete.expected.value.test('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,0)
   .complete.expected.value.test('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11,-100)
   .complete.expected.value.test('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,0)
   .complete.expected.value.test('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,100)
   
   
   
   
   
})
test_that("split dataset",
{
  connect.dataset.1()
  .complete.expected.value.test('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,100)
  .complete.expected.value.test('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,0)
  .complete.expected.value.test('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7,-100)
  .complete.expected.value.test('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,0)
  .complete.expected.value.test('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,100)
  .complete.expected.value.test('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,0)
  .complete.expected.value.test('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11,-100)
  .complete.expected.value.test('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,0)
  .complete.expected.value.test('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,100)
  
  connect.dataset.2()
  .complete.expected.value.test('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.2,15,100)
  .complete.expected.value.test('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.2,6,0)
  .complete.expected.value.test('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.2,7,-100)
  .complete.expected.value.test('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.2,8,0)
  .complete.expected.value.test('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.2,9,100)
  .complete.expected.value.test('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.2,10,0)
  .complete.expected.value.test('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.2,11,-100)
  .complete.expected.value.test('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.2,12,0)
  .complete.expected.value.test('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.2,13,100)
  
  connect.dataset.3()
  .complete.expected.value.test('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.3,15,100)
  .complete.expected.value.test('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.3,6,0)
  .complete.expected.value.test('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.3,7,-100)
  .complete.expected.value.test('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.3,8,0)
  .complete.expected.value.test('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.3,9,100)
  .complete.expected.value.test('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.3,10,0)
  .complete.expected.value.test('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.3,11,-100)
  .complete.expected.value.test('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.3,12,0)
  .complete.expected.value.test('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.3,13,100)
  
  
})

context("<END>ds.recodeValues.o</END>")