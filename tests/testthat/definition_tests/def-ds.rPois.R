
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

.test.lambdas <- function(size, variable.created, lambdas)
{
  seed <- as.integer(as.POSIXct(Sys.time(), "GMT"))/1000

  ds.rPois.o(samp.size = size, lambda = lambdas,  seed.as.integer = seed,newobj=variable.created, datasources=ds.test_env$connection.opal)
 
  sample.size <- ds.length(variable.created, type = "split", datasources=ds.test_env$connection.opal)
    
  for(index in 1:length(sample.size))
  {
    expect_true(sample.size[[index]] == lambdas[index])
  }
}

.test.dispersions.stats <- function(seed.first.dist,lambda.first.dist, seed.second.dist, lambda.second.dist)
{
  size <- 20000
  #create distribution on the server
  ds.rPois.o(samp.size = size, lambda = lambda.first.dist, newobj="first.dist",seed.as.integer = seed.first.dist,datasources=ds.test_env$connection.opal)
  ds.rPois.o(samp.size = size, lambda = lambda.second.dist, newobj="second.dist",seed.as.integer = seed.second.dist,datasources=ds.test_env$connection.opal)
 
  #calculated the mean values
  mean.first.dist      <- ds.mean.o(x="first.dist",type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  mean.second.dist     <- ds.mean.o(x="second.dist",type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  var.first.dist       <- ds.var.o(x="first.dist",type='combine', check=TRUE, datasources=ds.test_env$connection.opal)
  var.second.dist      <- ds.var.o(x="second.dist",type='combine', check=TRUE, datasources=ds.test_env$connection.opal)
  
  #calculate the error between the mean and variance
  error.mean <- (mean.first.dist[[1]][1]-mean.second.dist[[1]][1]) / size
  error.var <-(var.first.dist[[1]][1]-var.second.dist[[1]][1]) /size
  
  # the two Poisson distribution should be the same.
  if((lambda.first.dist == lambda.second.dist) &&
     (seed.first.dist   == seed.second.dist))
  {
    expect_equal(error.mean, 0)
    expect_equal(error.var, 0)
  }
  else
  {
    # the two Poisson distributions have a "similar" measure of centrality and 
    # dispersion
    if ((lambda.first.dist == lambda.second.dist) &&
      (seed.first.dist!= seed.second.dist))
    {
      expect_equal(error.mean, 0, tolerance = 10^1)
      expect_equal(error.var, 0, tolerance = 10^1)
    }
    else
    {
      # the two Poisson distributions have a dissimilar measure of centrality and 
      # dispersion. 
      expect_false(error.mean == 0)
      expect_false(error.var == 0)
    }
  }
  
}

