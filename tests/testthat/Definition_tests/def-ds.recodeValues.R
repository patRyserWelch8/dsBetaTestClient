source("connection_to_datasets/init_all_datasets.R")

.calc.distribution.locally <- function(some.local.values, column)
{
  value <- c(0,0)
  if(column <= ncol(some.local.values)) 
  {
    value[1] <- mean(some.local.values[,column])
    value[2] <- var(some.local.values[,column])
    value[3] <- nrow(some.local.values)
  }
  return(value)
}

.calc.distribution.server <- function(name.variable)
{
  value <- c(0,0)
  mean.from.servers <- ds.mean.o(x=name.variable,type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  var.from.servers <- ds.var.o(x=name.variable,type='combine',   datasources=ds.test_env$connection.opal)
  value[1] <- mean.from.servers[[1]][1]
  value[2] <- var.from.servers[[1]][1]
  value[3] <- mean.from.servers[[1]][3]
  
  return(value)
}


.apply.changes.locally <- function(some.local.values,value, a.value.to.replace, column) 
{
  if (!is.null(a.value.to.replace))
  {
    values <- some.local.values[,column]
    values[values==value] <- a.value.to.replace
    some.local.values[,column] <- values
  }
  
}

#vrepl <- function(haystack, needle, replacement) {
#  +   haystack[haystack == needle] <- replacement
#  +   return(haystack)
#  + }

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
      row <- sample(nrow(some.values),1) 
      value <- some.values[row,column]
      .apply.changes.locally(some.values,value,values.to.replace[i],column)   #change value locally start by randomly selecting a value locally
      values[i] <- value
    }
    
    recode.from.server <- ds.recodeValues.o(variable.name,values,values.to.replace,newobj = variable.recoded,datasources = ds.test_env$connection.opal)
    dist.local.recoded <- .calc.distribution.locally(some.values,column)
    dist.server.recoded <- .calc.distribution.server(variable.recoded)
    sum.server  =  dist.server.recoded[1] * dist.server.recoded[3]
    sum.local = dist.local.original[1] * dist.server.recoded[3]
    diff = sum.local - sum.server
    
    expect_equal(dist.local.original[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.original[2],dist.server.original[2], tolerance = ds.test_env$tolerance)
    
    expect_equal(dist.local.recoded[1],dist.server.recoded[1], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.recoded[2],dist.server.recoded[2], tolerance = ds.test_env$tolerance)
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
    dist.server.recoded <- .calc.distribution.server(variable.recoded)
    
    expect_equal(dist.local.original[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.original[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.recoded[2],dist.server.recoded[2], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.recoded[2],dist.server.recoded[2], tolerance = ds.test_env$tolerance)
  }
  else
  {
    expect_error(ds.recodeValues.o(variable.names,values,values.to.replace,name.variable.recoded,datasources = ds.test_env$connection.opal))
  }
}

.test.differences.in.sets <- function(variable.name, 
                                variable.recoded, 
                                some.values, 
                                column,
                                values.to.replace)
{
  if (!is.null(values.to.replace))
  {
    values <- c()
    differences <- c() 
    #change to server 
    dist.server.original <- .calc.distribution.server(variable.name)
    sum.server.original <- dist.server.original[1] * dist.server.original[3]
    #dist.local.original <- .calc.distribution.locally(some.values,column)
    #sum.local.original <- dist.local.original[1] * dist.local.original[3]
    #print(sum.local.original)
    
    for (i in 1:length(values.to.replace))
    {
      row <- sample(nrow(some.values),1) 
      value <- some.values[row,column]
      #remove 
      #.apply.changes.locally(some.values, value, values.to.replace[i],column)   #change value locally start by randomly selecting a value locally
      values[i] <- value
      differences[i] <- abs(values[i] - values.to.replace[i])
    }
    
    differences <- sort(differences, decreasing = TRUE)
    recode.from.server <- ds.recodeValues.o(variable.name,values,values.to.replace,newobj = variable.recoded,datasources = ds.test_env$connection.opal)
    dist.server.recoded <- .calc.distribution.server(variable.recoded)
    sum.server.recoded <- dist.server.recoded[1] * dist.server.recoded[3]
    difference <- sum.server.original - sum.server.recoded
    
    #dist.local.recoded <- .calc.distribution.locally(some.values, column)
    #sum.local.recoded <- dist.local.recoded[1] * dist.local.recoded[3]
    #difference <- sum.local.original - sum.local.recoded
  
    for (i in 1:length(differences))
    {
       difference <- difference %% differences[i]
       if (!is.nan(difference))
       {
         expect_equal(difference,0)
       }
    }
    
    
   
  }
  else
  {
    expect_error(ds.recodeValues.o(variable.names,values,values.to.replace,name.variable.recoded,datasources = ds.test_env$connection.opal))
  }
}