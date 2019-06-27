source("connection_to_datasets/init_all_datasets.R")
source("definition_tests/def-ds.recodeValues.R")

context("ds.recodeValues.o::math::single")
test_that("difference",
{
  connect.dataset.1() 
  values.to.replace <- c(0)
  .test.differences.in.sets('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
})