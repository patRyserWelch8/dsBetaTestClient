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

.calc.differences <- function(values, values.to.replace)
{
 
  differences <- c()
  for (i in 1:length(values.to.replace))
  {
    differences[i] <- values[i] - values.to.replace[i]
  }
  differences <- sort(differences, decreasing = TRUE)
  return(differences)
}

.apply.changes.locally <- function(some.local.values,value, values, values.to.replace=NULL) 
{
  print(values.to.replace)
  if (!is.null(values.to.replace))
  {
    for (i in 1:length)
    {
      some.local.values[values==values[1]] <- values.to.replaces[1]
      print ("HERE")
    }
  }
  return(some.local.values)
}

.apply.changes.server <- function(variable.name, variable.recoded, values,values.to.replace)
{
  recode.from.server <- ds.recodeValues.o(variable.name,values,values.to.replace,newobj = variable.recoded,datasources = ds.test_env$connection.opal)
}

.identify.values.unique<- function(some.values, length)
{
   values <- c()
   if (length > 0 || !is.null(some.values))
   {
      factors <- list(levels(as.factor(some.values)))
      
      for (i in 1:length)
      {
        index <-sample(length(factors[[1]]),1)
        values[i]  <- as.numeric(factors[[1]][index])
      }
   }
   return(values)
}

.identify.values.invalid <- function(some.values, length)
{
  values <- c()
  if (length > 0 || !is.null(some.values))
  {
    max.value <- max(some.values[,column])
    
    for (i in 1:length)
    {
      values[i] <- max.value + i
    }
    
  }
  return(values)
}


.test.apply.changes <- function(variable.name, 
                                variable.recoded, 
                                some.values, 
                                column,
                                values.to.replace)
{
  #calculate the distribution of the local and server datasets before the changes
  dist.local.original <- .calc.distribution.locally(some.values,column)
  dist.server.original <- .calc.distribution.server(variable.name)
  #test the values to replace have been set to at least one value
  if (!is.null(values.to.replace))
  {
 
    #identifies one or more values from the local data set. changes these values  locally and on the server.
    values <- .identify.values.unique(some.values[,column], length(values.to.replace))
    some.values[,column] <- .apply.changes.locally(some.values[,column],values, values.to.replace)
    .apply.changes.server(variable.name, variable.recoded, values,values.to.replace)
    
    #calculate the distribution of the local and server datasets after the changes
    dist.local.recoded <- .calc.distribution.locally(some.values,column)
    dist.server.recoded <- .calc.distribution.server(variable.recoded)
    
    #compare the results between the local and server data distribution.
    expect_equal(dist.local.original[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.original[2],dist.server.original[2], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.recoded[1],dist.server.recoded[1], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.recoded[2],dist.server.recoded[2], tolerance = ds.test_env$tolerance)
  }
  else
  {
    #no value to replace was set
    expect_error(ds.recodeValues.o(variable.names,values,values.to.replace,name.variable.recoded,datasources = ds.test_env$connection.opal))
  }
}

.test.apply.no.change <- function(variable.name, 
                                  variable.recoded, 
                                  some.values, 
                                  column,
                                  values.to.replace)
{ 
  
  #calculate the distribution of the local and server datasets before the changes
  dist.local.original <- .calc.distribution.locally(some.values,column)
  dist.server.original <- .calc.distribution.server(variable.name)
  
  #test the values to replace have been set to at least one value
  if (!is.null(values.to.replace))
  {
   
    #identifies one or more values from the local data set. changes these values  locally and on the server.
    values <- .identify.values.invalid(some.values[,column], length(values.to.replace))
    some.values[,column] <- .apply.changes.locally(some.values[,column],values, values.to.replace)
    .apply.changes.server(variable.name, variable.recoded, values,values.to.replace)
    
    #calculate the distribution of the local and server datasets after the changes
    dist.local.recoded <- .calc.distribution.locally(some.values,column)
    dist.server.recoded <- .calc.distribution.server(variable.recoded)
  
    expect_equal(dist.local.original[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.original[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.recoded[2],dist.server.recoded[2], tolerance = ds.test_env$tolerance)
    expect_equal(dist.local.recoded[2],dist.server.recoded[2], tolerance = ds.test_env$tolerance)
  }
  else
  {
    #no value to replace was set
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
    #identifies one or more values from the local data set. Calculate the absolute difference between these values
    values <- .identify.values.unique(some.values[,column], length(values.to.replace))
    differences <- .calc.differences(values, values.to.replace)
    
    #initialise values and capture distribution of the server set
    dist.server.original <- .calc.distribution.server(variable.name)
    
    #calculate the sum of the data set on the server before recoding
    sum.server.original <- dist.server.original[1] * dist.server.original[3]
    sum.server.previous <- sum.server.original
   
    
    #keep the difference brought by changing each values
    differences.recorded  <- c()
    
    for (i in 1:length(values))
    {
      
      .apply.changes.server(variable.name, variable.recoded, values[i],values.to.replace[i])
      dist.server.recoded <- .calc.distribution.server(variable.recoded)
      sum.server.recoded <- dist.server.recoded[1] * dist.server.recoded[3]
      difference <- sum.server.previous - sum.server.recoded
      differences.recorded[i] <- difference
      expect_equal(difference %% differences[i],0)
      sum.server.previous <- sum.server.recoded
    }
    
    #apply the changes to the server 
    .apply.changes.server(variable.name, variable.recoded, values,values.to.replace)
    
    #calculate the distribution of the local and server datasets after the changes
    dist.server.recoded <- .calc.distribution.server(variable.recoded)
    
    #calculate the sum of the data set on the server before recoding. Compute the difference betwen the original sum and the recoded one.
    sum.server.recoded <- dist.server.recoded[1] * dist.server.recoded[3]
    difference.total <- sum.server.original - sum.server.recoded
    
    expect_equal(sum(differences.recorded), difference.total)
  
  }
  else
  {
    expect_error(ds.recodeValues.o(variable.names,values,values.to.replace,name.variable.recoded,datasources = ds.test_env$connection.opal))
  }
}