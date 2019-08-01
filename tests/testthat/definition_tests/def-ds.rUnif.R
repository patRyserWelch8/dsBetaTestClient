source("definition_tests/def-assign-stats.R")

#check sample of reasonable size are created. Also verifies a variable
#on the server is created with a suitable type (i.e. integer).
#The length of each sample on each server is checked.
#No seed is used
.test.basic.expectation <- function(size,variable.created)
{
  if (size <= 0)
  { 
    expect_error(ds.rUnif.o(samp.size = size, newobj = variable.created, datasources=ds.test_env$connection.opal))
  } 
  else if (size > (2^31-1))
  {
    expect_error(ds.rUnif.o(samp.size = size, newobj = variable.created, datasources=ds.test_env$connection.opal))
  }
  else
  {
    ds.rUnif.o(samp.size = size, newobj = variable.created, datasources=ds.test_env$connection.opal)
    exists <- ds.exists(variable.created,datasources=ds.test_env$connection.opal)
    type <- ds.class(variable.created,datasources=ds.test_env$connection.opal)
    sample.size <- ds.length(variable.created, type = "split", datasources=ds.test_env$connection.opal)
  
    for(index in 1:length(exists))
    {
      expect_true(exists[[index]])
    }
    
    for(index in 1:length(sample.size))
    {
      expect_true(sample.size[[index]] == size)
    }
    
    expect_true(type[[1]][1]=="integer")
  }
}

#check sample of reasonable size are created. Also verifies a variable
#on the server is created with a suitable type (i.e. integer).
#The length of each sample on each server is checked.
#Some randomly generated seeds are used.
.test.basic.expectation.with.seeds <- function(size,variable.created)
{
  seed <- as.integer(as.POSIXct(Sys.time(), "GMT"))/1000
  
  if (size <= 0)
  {
    expect_error(ds.rUnif.o(samp.size = size, newobj = variable.created, seed.as.integer = seed, datasources=ds.test_env$connection.opal))
  }
  else if (size > (2^31-1))
  {
    expect_error(ds.rUnif.o(samp.size = size, newobj = variable.created, seed.as.integer = seed, datasources=ds.test_env$connection.opal))
  }
  else
  {
    
    ds.rUnif.o(samp.size = size, newobj = variable.created, seed.as.integer = seed, datasources=ds.test_env$connection.opal)
    exists <- ds.exists(variable.created,datasources=ds.test_env$connection.opal)
    type <- ds.class(variable.created,datasources=ds.test_env$connection.opal)
    sample.size <- ds.length(variable.created, type = "split", datasources=ds.test_env$connection.opal)
    
    for(index in 1:length(exists))
    {
      expect_true(exists[[index]])
    }
    
    for(index in 1:length(sample.size))
    {
      expect_true(sample.size[[index]] == size)
    }
    
    expect_true(type[[1]][1]=="integer")
  }
  
}

#test whether a distribution with a large seed can be created (i.e. maximum size of a sample)
.test.too.large.seed <- function(no.server)
{
  seed <- .Machine$integer.max
  if (seed > seed/(no.server +1))
  {
    expect_error(ds.rUnif.o(samp.size = 100, newobj = "dist.created", seed.as.integer = seed, datasources=ds.test_env$connection.opal))
  }
}

.test.too.negative.seed <- function()
{
  seed <- -100
  
  expect_warning(ds.rUnif.o(samp.size = 100, newobj = "dist.created", seed.as.integer = seed, datasources=ds.test_env$connection.opal))
  
}



.test.range.values <- function(min.value, max.value)
{
  
}

