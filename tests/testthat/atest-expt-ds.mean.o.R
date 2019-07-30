source("connection_to_datasets/init_all_datasets.R")
source("Definition_tests/def-ds.mean.R")

connect.all.datasets()

context("ds.mean.o::expct::multiple")
test_that("combined data set",
{
  
    connect.all.datasets()
    .test.mean.combined('D$INTEGER',ds.test_env$local.values,6)
    .test.mean.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values,7)
    .test.mean.combined('D$POSITIVE_INTEGER',ds.test_env$local.values,8)
    .test.mean.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values,9)
    .test.mean.combined('D$NUMERIC',ds.test_env$local.values,10)
    .test.mean.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values,11)
    .test.mean.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values,12)
    .test.mean.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values,13)

})

test_that("split dataset",
{
  
   connect.dataset.1()
   .test.mean.split ('D$INTEGER',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 6)
   .test.mean.split ('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 7)
   .test.mean.split ('D$POSITIVE_INTEGER',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 8)
   .test.mean.split ('D$NEGATIVE_INTEGER',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 9)
   .test.mean.split ('D$NUMERIC',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 10)
   .test.mean.split ('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 11)
   .test.mean.split ('D$POSITIVE_NUMERIC',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 12)
   .test.mean.split ('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 13)
})


context("ds.mean.o::expct::single")
test_that("combined data set",
{
   connect.dataset.1()
  .test.mean.combined('D$INTEGER',ds.test_env$local.values.1,6)
  .test.mean.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1,7)
  .test.mean.combined('D$POSITIVE_INTEGER',ds.test_env$local.values.1,8)
  .test.mean.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values.1,9)
  .test.mean.combined('D$NUMERIC',ds.test_env$local.values.1,10)
  .test.mean.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1,11)
  .test.mean.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values.1,12)
  .test.mean.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1,13)   
})

test_that("split data set",
{
    connect.dataset.1()
    mean.from.files <- mean(ds.test_env$local.values.1[,6])
    mean.from.servers <- ds.mean.o(x='D$INTEGER',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
            
    mean.from.files <- mean(ds.test_env$local.values.1[,8])
    mean.from.servers <- ds.mean.o(x='D$POSITIVE_INTEGER',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
            
    mean.from.files <- mean(ds.test_env$local.values.1[,9])
    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_INTEGER',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
            
    mean.from.files <- mean(ds.test_env$local.values.1[,7])
    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_INTEGER',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
            
    mean.from.files <- mean(ds.test_env$local.values.1[,10])
    mean.from.servers <- ds.mean.o(x='D$NUMERIC',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
            
    mean.from.files <- mean(ds.test_env$local.values.1[,12])
    mean.from.servers <- ds.mean.o(x='D$POSITIVE_NUMERIC',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
            
    mean.from.files <- mean(ds.test_env$local.values.1[,13])
    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_NUMERIC',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
            
    mean.from.files <- mean(ds.test_env$local.values.1[,11])
    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_NUMERIC',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
            
})

context("ds.mean.o::expct::assign::single")
test_that("single dataset",
{
   connect.dataset.1()
  .test.mean.split ('D$INTEGER',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 6)
  .test.mean.split ('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 7)
  .test.mean.split ('D$POSITIVE_INTEGER',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 8)
  .test.mean.split ('D$NEGATIVE_INTEGER',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 9)
  .test.mean.split ('D$NUMERIC',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 10)
  .test.mean.split ('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 11)
  .test.mean.split ('D$POSITIVE_NUMERIC',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 12)
  .test.mean.split ('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3, 13)
   
})

#context("Returns expected non-numerical values")
#test_that("character",
#{
#  #print("character")
#  mean.from.servers <- ds.mean.o(x='D$CHARACTER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
#  expect_equal(is.na(mean.from.servers),TRUE)
#})

#test_that("boolean",
#{
#  #print("boolean")
#  mean.from.files <- mean(ds.test_env$local.values[,3])
#  mean.from.servers <- ds.mean.o(x='D$BOOLEAN',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
#  print("mean.from.files")
#  expect_equal(mean.from.servers,mean.from.files,tolerance = 10^-15)
#})

# context("Mathematical properties")
test_that("residual deviation tends to 0",
{
    mean.from.servers <- ds.mean.o(x='D$INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$local.values[,6] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)

    mean.from.servers <- ds.mean.o(x='D$POSITIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$local.values[,8] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)

    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$local.values[,9] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)

    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$local.values[,7] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)
    #----
    mean.from.servers <- ds.mean.o(x='D$NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$local.values[,10] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)

    mean.from.servers <- ds.mean.o(x='D$POSITIVE_NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$local.values[,12] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)

    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$local.values[,13] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)

    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$local.values[,11] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)
})


