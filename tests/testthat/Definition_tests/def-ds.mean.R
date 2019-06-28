

.test.mean.combined <- function(variable.name,some.values,column)
{
  mean.from.files <- mean(some.values[,column])
  mean.from.servers <- ds.mean.o(x=variable.name,type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = ds.test_env$tolerance)
}

.test.mean.split <- function(variable.name,some.values.1,some.values.2,some.values.3,column)
{
  if(is.null(some.values.1))
  {
    mean.from.file.1 <- 0
  }
  else
  {
    mean.from.file.1 <- mean(some.values.1[,column])
  }
  
  if(is.null(some.values.2))
  {
    mean.from.file.2 <- 0
  }
  else
  {
    mean.from.file.2 <- mean(some.values.2[,column])
  }

  if(is.null(some.values.3))
  {
    mean.from.file.3 <- 0
  }
  else
  {
    mean.from.file.3 <- mean(some.values.3[,column])
  }
  
  mean.from.servers <- ds.mean.o(x=variable.name,type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  print(mean.from.servers)
  expect_equal(mean.from.servers[[1]][1], mean.from.file.1, tolerance = ds.test_env$tolerance)
  expect_equal(mean.from.servers[[1]][2], mean.from.file.2, tolerance = ds.test_env$tolerance)
  expect_equal(mean.from.servers[[1]][3], mean.from.file.3, tolerance = ds.test_env$tolerance)
  
}