source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.rPois.R")

context("ds.rPois.o()::math::multiple")
test_that("null hypothesis",
{
  connect.all.datasets()
  seed <- as.integer(as.POSIXct(Sys.time(), "GMT"))/1000
  .test.dispersions.stats(seed, 
                             c(4000),
                             seed,
                             c(4000))
  
  
  .test.dispersions.stats(seed/1000, 
                             c(4000),
                             seed,
                             c(4000))
  
  .test.dispersions.stats(seed/1000, 
                             c(4700),
                             seed,
                             c(4000))
})