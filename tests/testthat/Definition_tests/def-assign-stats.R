.calc.distribution.locally <- function(some.local.values)
{
  value <- c(0,0)
  
  value[1] <- mean(some.local.values)
  value[2] <- var(some.local.values)
  value[3] <- length(some.local.values)
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
