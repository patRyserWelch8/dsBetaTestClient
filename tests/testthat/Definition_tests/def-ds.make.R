source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-assign-stats.R")

.test.copy.data <-function(variable.name, variable.created, some.values)
{
  #calculate the distribution of the localserver
  dist.local <- .calc.distribution.locally(some.values)
  dist.server.original <- .calc.distribution.server(variable.name)
  
  #create a new object with the same values. calculate distribution of new object
  ds.make.o(variable.name,variable.created,datasources = ds.test_env$connection.opal)
  dist.server.new.object <- .calc.distribution.server(variable.created)
 
  #compare the results between the local and server data distribution.
  expect_equal(dist.local[1],dist.server.original[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[2],dist.server.original[2], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[1],dist.server.new.object[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[2],dist.server.new.object[2], tolerance = ds.test_env$tolerance)
  expect_equal(dist.server.original[1],dist.server.new.object[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.server.original[2],dist.server.new.object[2], tolerance = ds.test_env$tolerance)
}

.test.operation.vectors <- function(first.variable.name, second.variable.name, variable.created, arithmetic.operator, result.local)
{
  #build expressions and applies on the server
  operation <- paste("(",first.variable.name, arithmetic.operator, second.variable.name,")",sep="")
  result.server <- ds.make.o(operation,variable.created,datasources = ds.test_env$connection.opal)
  
  #distribution the results between the local and server data.
  dist.local <- .calc.distribution.locally(result.local)
  dist.server.new.object <- .calc.distribution.server(variable.created)

 
  #compare the results between the local and server data distribution.
  expect_equal(dist.local[1],dist.server.new.object[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[2],dist.server.new.object[2], tolerance = ds.test_env$tolerance)
}

.test.operation.constant <- function(first.variable.name, constant.value, variable.created, arithmetic.operator, result.local)
{
  #build expressions and applies on the server
  operation <- paste("(",first.variable.name, arithmetic.operator, constant.value,")",sep="")
  
  result.server <- ds.make.o(operation,variable.created,datasources = ds.test_env$connection.opal)
  
  #distribution the results between the local and server data.
  dist.local <- .calc.distribution.locally(result.local)
  dist.server.new.object <- .calc.distribution.server(variable.created)
  
  #compare the results between the local and server data distribution.
  expect_equal(dist.local[1],dist.server.new.object[1], tolerance = ds.test_env$tolerance)
  expect_equal(dist.local[2],dist.server.new.object[2], tolerance = ds.test_env$tolerance)
}