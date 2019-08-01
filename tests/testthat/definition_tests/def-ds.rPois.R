source("definition_tests/def-assign-stats.R")

.test.basic.expectation <- function(size,variable.created)
{
   if (size <= 0)
   {
     expect_error(ds.rPois.o(samp.size = size,newobj=variable.created, datasources=ds.test_env$connection.opal))
   }
   else if (size > (2^31-1))
   {
     expect_error(ds.rPois.o(samp.size = size,newobj=variable.created, datasources=ds.test_env$connection.opal))
   }
   else
   {
   
      ds.rPois.o(samp.size = size,newobj=variable.created, datasources=ds.test_env$connection.opal)
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

.test.basic.expectation.with.seeds <- function(size,variable.created)
{
  seed <- as.integer(as.POSIXct(Sys.time(), "GMT"))/1000
 
  if (size <= 0)
  {
    expect_error(ds.rPois.o(samp.size = size, 
                            newobj=variable.created,
                            seed.as.integer = seed,
                            return.full.seed.as.set = FALSE,
                            datasources=ds.test_env$connection.opal))
  }
  else if (size > (2^31-1))
  {
    expect_error(ds.rPois.o(samp.size = size,
                            newobj=variable.created, 
                            seed.as.integer = seed,
                            return.full.seed.as.set = FALSE,
                            datasources=ds.test_env$connection.opal))
  }
  
  else
  {
  
    ds.rPois.o(samp.size = size, 
               newobj=variable.created,
               seed.as.integer = seed,
               return.full.seed.as.set = FALSE,
               datasources=ds.test_env$connection.opal)
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

.test.too.large.seed <- function(no.server)
{
  seed <- .Machine$integer.max
  if (seed > seed/(no.server +1))
  {
    expect_error(ds.rPois.o(samp.size = size,
                            newobj=variable.created, 
                            seed.as.integer = seed,
                            return.full.seed.as.set = FALSE,
                            datasources=ds.test_env$connection.opal))
  }
}

.test.too.negative.seed <- function()
{
  seed <- -100
 
  expect_warning(ds.rPois.o(samp.size = size,
             newobj=variable.created, 
                            seed.as.integer = seed,
                            return.full.seed.as.set = FALSE,
                            datasources=ds.test_env$connection.opal))
  
}


.test.dispersions.stats.same.distribution <- function(seed.first.dist,lambda.first.dist, seed.second.dist, lambda.second.dist)
{
  size <- 20000
  #create distribution on the server
  ds.rPois.o(samp.size = size, lambda = lambda.first.dist, newobj="first.dist",seed.as.integer = seed.first.dist,datasources=ds.test_env$connection.opal)
  ds.rPois.o(samp.size = size, lambda = lambda.second.dist, newobj="second.dist",seed.as.integer = seed.second.dist,datasources=ds.test_env$connection.opal)
 
  first.dist <- .calc.distribution.server("first.dist")
  second.dist <- .calc.distribution.server("second.dist")
 
  #calculate the error between the mean and variance
  error.mean <- (first.dist[1]- second.dist[1])/size
  error.var  <- (first.dist[1] - second.dist[1])/size
  
  expect_true(error.mean == 0)
  expect_true(error.var  == 0)
}

.test.dispersions.stats.diff.distribution <- function(seed.first.dist,lambda.first.dist, seed.second.dist, lambda.second.dist)
{
  
  size <- 20000
  #create distribution on the server
  ds.rPois.o(samp.size = size, lambda = lambda.first.dist, newobj="first.dist",seed.as.integer = seed.first.dist,datasources=ds.test_env$connection.opal)
  ds.rPois.o(samp.size = size, lambda = lambda.second.dist, newobj="second.dist",seed.as.integer = seed.second.dist,datasources=ds.test_env$connection.opal)
  
  first.dist <- .calc.distribution.server("first.dist")
  second.dist <- .calc.distribution.server("second.dist")
 
  #calculate the error between the mean and variance
  error.mean <- (first.dist[1] - second.dist[1])/size
  error.var  <- (first.dist[2] - second.dist[2])/size
  
  expect_equal(lambda.first.dist - lambda.second.dist, first.dist[1] - second.dist[1], tolerance = 10^-1)
  expect_equal(lambda.first.dist - lambda.second.dist, first.dist[2] - second.dist[2], tolerance = 10^-1)

}

.test.lambda.mean.var <- function(seed, lambda.vector)
{
  size <- 20000
  ds.rPois.o(samp.size = size, lambda = lambda.vector, newobj="first.dist",seed.as.integer = seed, datasources=ds.test_env$connection.opal)
  first.dist <- .calc.distribution.server("first.dist")
  expect_equal(first.dist[1],lambda.vector[1],tolerance = 10^1)
  expect_equal(first.dist[2],lambda.vector[1],tolerance = 10^1)
}
