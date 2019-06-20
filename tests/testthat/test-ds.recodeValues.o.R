source("connection_to_datasets/init_all_datasets.R")

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
 if (!is.null(a.value.to.replace))
  {
    row <- sample(nrow(some.local.values),1) 
    value <- some.local.values[row,column] 
    some.local.values[row,column] <- a.value.to.replace
 }
 else
 {
    value <- NULL
 }
 return(value)
}

.complete.expected.value.test <- function(variable.name, 
                                          variable.recoded, 
                                          some.values, 
                                          column,
                                          values.to.replace)
{
  dist.local.original <- .calc.distribution.locally(some.values,column)
  dist.server.original <- .calc.distribution.server(variable.name)
  values <- c()
  if (!is.null(values.to.replace))
  {
      for (i in 1:length(values.to.replace))
      {
          value <- .apply.changes.locally(some.values,values.to.replace[i],column)   #change value locally start by randomly selecting a value locally
          values[i] <- value
      }
   
      recode.from.server <- ds.recodeValues.o(variable.name,values,values.to.replace,newobj = variable.recoded,datasources = ds.test_env$connection.opal)
      dist.local.recoded <- .calc.distribution.locally(some.values,column)
      dist.server.recoded <- .calc.distribution.server(variable.name)
  
      expect_equal(dist.local.original[0],dist.server.original[0], tolerance = ds.test_env$tolerance)
      expect_equal(dist.local.original[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
      expect_equal(dist.local.recoded[0],dist.server.recoded[0], tolerance = ds.test_env$tolerance)
      expect_equal(dist.local.recoded[1],dist.server.recoded[1], tolerance = ds.test_env$tolerance)
  }
  else
  {
    expect_error(ds.recodeValues.o(variable.names,values,values.to.replace,name.variable.recoded,datasources = ds.test_env$connection.opal))
  }
}



context("ds.recodeValues.o()::mathematical::Returns expected numerical value")
test_that("combined data set",
{
   connect.all.datasets()
   values.to.replace <- c()
   .complete.expected.value.test('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
   .complete.expected.value.test('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
   .complete.expected.value.test('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
  
   values.to.replace <- c()
   .complete.expected.value.test('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
   .complete.expected.value.test('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
   .complete.expected.value.test('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
   .complete.expected.value.test('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
  
   values.to.replace <- c()
   .complete.expected.value.test('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
   .complete.expected.value.test('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
  
   values.to.replace <- c(-100)
   .complete.expected.value.test('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
   .complete.expected.value.test('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
   .complete.expected.value.test('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
   
   values.to.replace <- c(0)
   .complete.expected.value.test('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
   .complete.expected.value.test('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
   .complete.expected.value.test('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
   .complete.expected.value.test('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)

   values.to.replace <- c(100)
   .complete.expected.value.test('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
   .complete.expected.value.test('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)

   values.to.replace <- c(-100,-200)
   .complete.expected.value.test('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
   .complete.expected.value.test('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
   .complete.expected.value.test('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
   
   values.to.replace <- c(0,200)
   .complete.expected.value.test('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
   .complete.expected.value.test('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
   .complete.expected.value.test('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
   .complete.expected.value.test('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
   
   values.to.replace <- c(100,200)
   .complete.expected.value.test('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
   .complete.expected.value.test('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
   
})

test_that("split dataset",
{
  connect.dataset.1()
  
  
})





test_that("combined data set",
{
   connect.all.datasets()
   #change value locally start
   row <- sample(nrow(ds.test_env$local.values),1)
   value <- ds.test_env$local.values[row,6]
   print(ds.test_env$local.values[row,6])
   print(value)
   ds.test_env$local.values[row,6] <- 0
   print(ds.test_env$local.values[row,6])
   print(value)
   #change value locally end
   #change value on the server
   #a <- ds.recodeValues.o('D$FACTOR_INTEGER',c(0,1,2),c(20,27,37),newobj = 'INTEGER_recoded',datasources = connection.opal)
   recode.from.server <- ds.recodeValues.o('D$INTEGER',value,c(0),newobj = 'INTEGER_recoded',datasources = ds.test_env$connection.opal)
   factor.from.server <- ds.asFactor.o('INTEGER_recoded','INT.f', datasources = ds.test_env$connection.opal)
   factors <- factor.from.server[[1]]
   print(factors)

   print(length(factors[factors == "0"])> 0)
   expect_true(length(factors[factors == "0"])> 0)

})



