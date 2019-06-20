

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

.test.apply.changes <- function(variable.name, 
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

.test.apply.no.change <- function(variable.name, 
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
    max.value <- max(some.values[,column])
    
    for (i in 1:length(values.to.replace))
    {
      values[i] <- max.value + i
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

context("ds.recodeValues.o()::expt::single::no_change_applied")
test_that("no_change_applied",
          {
            connect.dataset.1() 
            values.to.replace <- c()
            .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
            .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
            .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
            
            values.to.replace <- c()
            .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
            .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
            .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
            .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
            
            values.to.replace <- c()
            .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
            .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
            
            values.to.replace <- c(-100)
            .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
            .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
            .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
            
            values.to.replace <- c(0)
            .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
            .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
            .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
            .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
            
            values.to.replace <- c(100)
            .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
            .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
            
            values.to.replace <- c(-100,-200)
            .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
            .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
            .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
            
            values.to.replace <- c(0,200)
            .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
            .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
            .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
            .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
            
            values.to.replace <- c(100,200)
            .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
            .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
            
          })



context("ds.recodeValues.o()::expt::single::changes_applied")
test_that("changes must be applied",
{
            connect.dataset.1() 
            values.to.replace <- c()
            .test.apply.changes('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
            .test.apply.changes('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
            .test.apply.changes('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
            
            values.to.replace <- c()
            .test.apply.changes('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
            .test.apply.changes('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
            .test.apply.changes('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
            .test.apply.changes('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
            
            values.to.replace <- c()
            .test.apply.changes('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
            .test.apply.changes('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
            
            values.to.replace <- c(-100)
            .test.apply.changes('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
            .test.apply.changes('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
            .test.apply.changes('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
            
            values.to.replace <- c(0)
            .test.apply.changes('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
            .test.apply.changes('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
            .test.apply.changes('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
            .test.apply.changes('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
            
            values.to.replace <- c(100)
            .test.apply.changes('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
            .test.apply.changes('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
            
            values.to.replace <- c(-100,-200)
            .test.apply.changes('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
            .test.apply.changes('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
            .test.apply.changes('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
            
            values.to.replace <- c(0,200)
            .test.apply.changes('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
            .test.apply.changes('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
            .test.apply.changes('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
            .test.apply.changes('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
            
            values.to.replace <- c(100,200)
            .test.apply.changes('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
            .test.apply.changes('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
            
})


context("ds.recodeValues.o()::expt::multiple::changes_applied")
test_that("changes must be applied",
{
    connect.all.datasets()
    values.to.replace <- c()
    .test.apply.changes('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
    .test.apply.changes('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
    .test.apply.changes('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
    values.to.replace <- c()
    .test.apply.changes('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
    .test.apply.changes('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
    .test.apply.changes('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
    .test.apply.changes('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
    values.to.replace <- c()
    .test.apply.changes('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
    .test.apply.changes('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
            
    values.to.replace <- c(-100)
            .test.apply.changes('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
            .test.apply.changes('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
            .test.apply.changes('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
            values.to.replace <- c(0)
            .test.apply.changes('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
            .test.apply.changes('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
            .test.apply.changes('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
            .test.apply.changes('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
            values.to.replace <- c(100)
            .test.apply.changes('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
            .test.apply.changes('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
            
            values.to.replace <- c(-100,-200)
            .test.apply.changes('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
            .test.apply.changes('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
            .test.apply.changes('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
            values.to.replace <- c(0,200)
            .test.apply.changes('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
            .test.apply.changes('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
            .test.apply.changes('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
            .test.apply.changes('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
            values.to.replace <- c(100,200)
            .test.apply.changes('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
            .test.apply.changes('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
})



context("ds.recodeValues.o()::expt::multiple::no_change_applied")
test_that("no_change_applied",
{
            connect.all.datasets()
            values.to.replace <- c()
            .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
            .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
            .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
            values.to.replace <- c()
            .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
            .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
            .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
            .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
            values.to.replace <- c()
            .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
            .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
            
            values.to.replace <- c(-100)
            .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
            .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
            .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
            values.to.replace <- c(0)
            .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
            .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
            .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
            .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
            values.to.replace <- c(100)
            .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
            .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
            
            values.to.replace <- c(-100,-200)
            .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
            .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
            .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
            values.to.replace <- c(0,200)
            .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
            .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
            .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
            .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
            values.to.replace <- c(100,200)
            .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
            .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
})

