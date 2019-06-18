source("connection_to_datasets/init_all_datasets.R")
connect.all.datasets()

#context("<START>ds.var.o</START>")
#context("Returns expected numerical value")
context("ds.var.o()::mathematical::Returns expected numerical value")
test_that("combined data set",
{
  #  print("positive and negative integers value")
    var.from.files <- var(ds.test_env$local.values[,6])
    var.from.servers <- ds.var.o(x='D$INTEGER',type='combine',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.files, tolerance = 10^-15)

   # print("positive integers")
    var.from.files <- var(ds.test_env$local.values[,8])
    var.from.servers <- ds.var.o(x='D$POSITIVE_INTEGER',type='combine',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.files, tolerance = 10^-15)

  #  print("negative integers")
    var.from.files <- var(ds.test_env$local.values[,9])
    var.from.servers <- ds.var.o(x='D$NEGATIVE_INTEGER',type='combine',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.files, tolerance = 10^-15)

   # print("non negative integers")
    var.from.files <- var(ds.test_env$local.values[,7])
    var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_INTEGER',type='combine',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.files, tolerance = 10^-15)
    #----------------------
    #print("positive and negative numeric value")
    var.from.files <- var(ds.test_env$local.values[,10])
    var.from.servers <- ds.var.o(x='D$NUMERIC',type='combine',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.files, tolerance = 10^-15)

    #print("positive numeric")
    var.from.files <- var(ds.test_env$local.values[,12])
    var.from.servers <- ds.var.o(x='D$POSITIVE_NUMERIC',type='combine',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.files, tolerance = 10^-15)

  #  print("negative numeric")
    var.from.files <- var(ds.test_env$local.values[,13])
    var.from.servers <- ds.var.o(x='D$NEGATIVE_NUMERIC',type='combine',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.files, tolerance = 10^-15)

   # print("non negative numeric")
    var.from.files <- var(ds.test_env$local.values[,11])
    var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_NUMERIC',type='combine',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.files, tolerance = 10^-15)
})

test_that("split dataset",
{
    #print("positive and negative integers value")
    var.from.file.1 <- var(ds.test_env$local.values.1[,6])
    var.from.file.2 <- var(ds.test_env$local.values.2[,6])
    var.from.file.3 <- var(ds.test_env$local.values.3[,6])
    var.from.servers <- ds.var.o(x='D$INTEGER',type='split',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.file.1, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][2], var.from.file.2, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][3], var.from.file.3, tolerance = 10^-15)

    #print("positive integers value")
    var.from.file.1 <- var(ds.test_env$local.values.1[,8])
    var.from.file.2 <- var(ds.test_env$local.values.2[,8])
    var.from.file.3 <- var(ds.test_env$local.values.3[,8])
    var.from.servers <- ds.var.o(x='D$POSITIVE_INTEGER',type='split',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.file.1, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][2], var.from.file.2, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][3], var.from.file.3, tolerance = 10^-15)

    #print("negative integers")
    var.from.file.1 <- var(ds.test_env$local.values.1[,9])
    var.from.file.2 <- var(ds.test_env$local.values.2[,9])
    var.from.file.3 <- var(ds.test_env$local.values.3[,9])
    var.from.servers <- ds.var.o(x='D$NEGATIVE_INTEGER',type='split',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.file.1, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][2], var.from.file.2, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][3], var.from.file.3, tolerance = 10^-15)

    #print("non negative integers")
    var.from.file.1 <- var(ds.test_env$local.values.1[,7])
    var.from.file.2 <- var(ds.test_env$local.values.2[,7])
    var.from.file.3 <- var(ds.test_env$local.values.3[,7])
    var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_INTEGER',type='split',   datasources=ds.test_env$connection.opal)

    expect_equal(var.from.servers[[1]][1], var.from.file.1, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][2], var.from.file.2, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][3], var.from.file.3, tolerance = 10^-15)

    #--------------
    #print("positive and negative numeric value")
    var.from.file.1 <- var(ds.test_env$local.values.1[,10])
    var.from.file.2 <- var(ds.test_env$local.values.2[,10])
    var.from.file.3 <- var(ds.test_env$local.values.3[,10])
    var.from.servers <- ds.var.o(x='D$NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.file.1, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][2], var.from.file.2, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][3], var.from.file.3, tolerance = 10^-15)

    #print("positive numeric value")
    var.from.file.1 <- var(ds.test_env$local.values.1[,12])
    var.from.file.2 <- var(ds.test_env$local.values.2[,12])
    var.from.file.3 <- var(ds.test_env$local.values.3[,12])
    var.from.servers <- ds.var.o(x='D$POSITIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.file.1, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][2], var.from.file.2, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][3], var.from.file.3, tolerance = 10^-15)

    #print("negative numeric")
    var.from.file.1 <- var(ds.test_env$local.values.1[,13])
    var.from.file.2 <- var(ds.test_env$local.values.2[,13])
    var.from.file.3 <- var(ds.test_env$local.values.3[,13])
    var.from.servers <- ds.var.o(x='D$NEGATIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_equal(var.from.servers[[1]][1], var.from.file.1, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][2], var.from.file.2, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][3], var.from.file.3, tolerance = 10^-15)

    #print("non negative numeric")
    var.from.file.1 <- var(ds.test_env$local.values.1[,11])
    var.from.file.2 <- var(ds.test_env$local.values.2[,11])
    var.from.file.3 <- var(ds.test_env$local.values.3[,11])
    var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)

    expect_equal(var.from.servers[[1]][1], var.from.file.1, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][2], var.from.file.2, tolerance = 10^-15)
    expect_equal(var.from.servers[[1]][3], var.from.file.3, tolerance = 10^-15)
})

#context("Returns expected non-numerical values")
#test_that("character",
#{
#  #print("character")
#  var.from.servers <- ds.var.o(x='D$CHARACTER',type='combine',   datasources=ds.test_env$connection.opal)
#  expect_equal(is.na(var.from.servers),TRUE)
#})

#test_that("boolean",
#{
#  #print("boolean")
#  var.from.files <- var(ds.test_env$local.values[,3])
#  var.from.servers <- ds.var.o(x='D$BOOLEAN',type='combine',   datasources=ds.test_env$connection.opal)
#  print("var.from.files")
#  expect_equal(var.from.servers,var.from.files,tolerance = 10^-15)
#})

#context("Mathematical properties")
test_that("variance >=0",
{
    var.from.servers <- ds.var.o(x='D$INTEGER',type='combine', datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers$Global.Variance[1] >= 0)

    var.from.servers <- ds.var.o(x='D$POSITIVE_INTEGER',type='combine',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers$Global.Variance[1] >= 0)

    var.from.servers <- ds.var.o(x='D$NEGATIVE_INTEGER',type='combine',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers$Global.Variance[1] >= 0)

    var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_INTEGER',type='combine',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers$Global.Variance[1] >= 0)

    #----
    var.from.servers <- ds.var.o(x='D$NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    var.from.servers <- ds.var.o(x='D$POSITIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    var.from.servers <- ds.var.o(x='D$NEGATIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    var.from.servers <- ds.var.o(x='D$INTEGER',type='split', datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    var.from.servers <- ds.var.o(x='D$POSITIVE_INTEGER',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    var.from.servers <- ds.var.o(x='D$NEGATIVE_INTEGER',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_INTEGER',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    #----
    var.from.servers <- ds.var.o(x='D$NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    var.from.servers <- ds.var.o(x='D$POSITIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    var.from.servers <- ds.var.o(x='D$NEGATIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)

    var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
    expect_true(var.from.servers[[1]][1] >= 0)
    expect_true(var.from.servers[[1]][2] >= 0)
    expect_true(var.from.servers[[1]][3] >= 0)
})

test_that("The variance is  the standard deviation to the power of 2",
{
  var.from.servers <- ds.var.o(x='D$INTEGER',type='combine', datasources=ds.test_env$connection.opal)
  std.from.files <- sd(ds.test_env$local.values[,6])
  expect_equal(sqrt(var.from.servers$Global.Variance[1]),std.from.files, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$POSITIVE_INTEGER',type='combine',   datasources=ds.test_env$connection.opal)
  std.from.files <- sd(ds.test_env$local.values[,8])
  expect_equal(sqrt(var.from.servers$Global.Variance[1]),std.from.files, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$NEGATIVE_INTEGER',type='combine',   datasources=ds.test_env$connection.opal)
  std.from.files <- sd(ds.test_env$local.values[,9])
  expect_equal(sqrt(var.from.servers$Global.Variance[1]),std.from.files, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_INTEGER',type='combine',   datasources=ds.test_env$connection.opal)
  std.from.files <- sd(ds.test_env$local.values[,7])
  expect_equal(sqrt(var.from.servers$Global.Variance[1]),std.from.files, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$NUMERIC',type='combine', datasources=ds.test_env$connection.opal)
  std.from.files <- sd(ds.test_env$local.values[,10])
  expect_equal(sqrt(var.from.servers$Global.Variance[1]),std.from.files, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$POSITIVE_NUMERIC',type='combine',   datasources=ds.test_env$connection.opal)
  std.from.files <- sd(ds.test_env$local.values[,12])
  expect_equal(sqrt(var.from.servers$Global.Variance[1]),std.from.files, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$NEGATIVE_NUMERIC',type='combine',   datasources=ds.test_env$connection.opal)
  std.from.files <- sd(ds.test_env$local.values[,13])
  expect_equal(sqrt(var.from.servers$Global.Variance[1]),std.from.files, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_NUMERIC',type='combine',   datasources=ds.test_env$connection.opal)
  std.from.files <- sd(ds.test_env$local.values[,11])
  expect_equal(sqrt(var.from.servers$Global.Variance[1]),std.from.files, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
  std.from.files.1 <- sd(ds.test_env$local.values.1[,10])
  std.from.files.2 <- sd(ds.test_env$local.values.2[,10])
  std.from.files.3 <- sd(ds.test_env$local.values.3[,10])
  expect_equal(sqrt(var.from.servers[[1]][1]), std.from.files.1, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][2]), std.from.files.2, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][3]), std.from.files.3, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$POSITIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
  std.from.files.1 <- sd(ds.test_env$local.values.1[,12])
  std.from.files.2 <- sd(ds.test_env$local.values.2[,12])
  std.from.files.3 <- sd(ds.test_env$local.values.3[,12])
  expect_equal(sqrt(var.from.servers[[1]][1]), std.from.files.1, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][2]), std.from.files.2, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][3]), std.from.files.3, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$NEGATIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
  std.from.files.1 <- sd(ds.test_env$local.values.1[,13])
  std.from.files.2 <- sd(ds.test_env$local.values.2[,13])
  std.from.files.3 <- sd(ds.test_env$local.values.3[,13])
  expect_equal(sqrt(var.from.servers[[1]][1]), std.from.files.1, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][2]), std.from.files.2, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][3]), std.from.files.3, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_NUMERIC',type='split',   datasources=ds.test_env$connection.opal)
  std.from.files.1 <- sd(ds.test_env$local.values.1[,11])
  std.from.files.2 <- sd(ds.test_env$local.values.2[,11])
  std.from.files.3 <- sd(ds.test_env$local.values.3[,11])
  expect_equal(sqrt(var.from.servers[[1]][1]), std.from.files.1, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][2]), std.from.files.2, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][3]), std.from.files.3, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$INTEGER',type='split',   datasources=ds.test_env$connection.opal)
  std.from.files.1 <- sd(ds.test_env$local.values.1[,6])
  std.from.files.2 <- sd(ds.test_env$local.values.2[,6])
  std.from.files.3 <- sd(ds.test_env$local.values.3[,6])
  expect_equal(sqrt(var.from.servers[[1]][1]), std.from.files.1, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][2]), std.from.files.2, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][3]), std.from.files.3, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$POSITIVE_INTEGER',type='split',   datasources=ds.test_env$connection.opal)
  std.from.files.1 <- sd(ds.test_env$local.values.1[,8])
  std.from.files.2 <- sd(ds.test_env$local.values.2[,8])
  std.from.files.3 <- sd(ds.test_env$local.values.3[,8])
  expect_equal(sqrt(var.from.servers[[1]][1]), std.from.files.1, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][2]), std.from.files.2, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][3]), std.from.files.3, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$NEGATIVE_INTEGER',type='split',   datasources=ds.test_env$connection.opal)
  std.from.files.1 <- sd(ds.test_env$local.values.1[,9])
  std.from.files.2 <- sd(ds.test_env$local.values.2[,9])
  std.from.files.3 <- sd(ds.test_env$local.values.3[,9])
  expect_equal(sqrt(var.from.servers[[1]][1]), std.from.files.1, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][2]), std.from.files.2, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][3]), std.from.files.3, tolerance = 10^-8)

  var.from.servers <- ds.var.o(x='D$NON_NEGATIVE_INTEGER',type='split',   datasources=ds.test_env$connection.opal)
  std.from.files.1 <- sd(ds.test_env$local.values.1[,7])
  std.from.files.2 <- sd(ds.test_env$local.values.2[,7])
  std.from.files.3 <- sd(ds.test_env$local.values.3[,7])
  expect_equal(sqrt(var.from.servers[[1]][1]), std.from.files.1, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][2]), std.from.files.2, tolerance = 10^-8)
  expect_equal(sqrt(var.from.servers[[1]][3]), std.from.files.3, tolerance = 10^-8)
  #-------



})



#context("incorrect arguments")
#test_that("A suitable value is returned with some incorrect arugements",
#{
#    print(class(ds.test_env$connection.opal))
#    print(ds.var.o())
#})
# context("<END>ds.var.o</END>")
