context("returns an expected numerical value")
test_that("The mean function returns some expected numerical values",
{
    print("positive and negative integers value")
    mean.from.files <- mean(ds.test_env$same.values[,6])
    mean.from.servers <- ds.mean.o(x='D$INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
   
    print("positive integers")
    mean.from.files <- mean(ds.test_env$same.values[,8])
    mean.from.servers <- ds.mean.o(x='D$POSITIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
    
    print("negative integers")
    mean.from.files <- mean(ds.test_env$same.values[,9])
    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
    
    print("non negative integers")
    mean.from.files <- mean(ds.test_env$same.values[,7])
    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
    #----------------------
    print("positive and negative numeric value")
    mean.from.files <- mean(ds.test_env$same.values[,10])
    mean.from.servers <- ds.mean.o(x='D$NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
    
    print("positive numeric")
    mean.from.files <- mean(ds.test_env$same.values[,12])
    mean.from.servers <- ds.mean.o(x='D$POSITIVE_NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
    
    print("negative numeric")
    mean.from.files <- mean(ds.test_env$same.values[,13])
    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
    
    print("non negative numeric")
    mean.from.files <- mean(ds.test_env$same.values[,11])
    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(mean.from.servers[[1]][1], mean.from.files, tolerance = 10^-15)
})

test_that("The mean function returns some expected numerical values",
{
    print("positive and negative integers value")
    mean.from.file.1 <- mean(ds.test_env$same.values.1[,6])
    mean.from.file.2 <- mean(ds.test_env$same.values.2[,6])
    mean.from.file.3 <- mean(ds.test_env$same.values.3[,6])
    mean.from.servers <- ds.mean.o(x='D$INTEGER',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)      
    expect_equal(mean.from.servers[[1]][1], mean.from.file.1, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][2], mean.from.file.2, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][3], mean.from.file.3, tolerance = 10^-15)
    
    print("positive integers value")
    mean.from.file.1 <- mean(ds.test_env$same.values.1[,8])
    mean.from.file.2 <- mean(ds.test_env$same.values.2[,8])
    mean.from.file.3 <- mean(ds.test_env$same.values.3[,8])
    mean.from.servers <- ds.mean.o(x='D$POSITIVE_INTEGER',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)      
    expect_equal(mean.from.servers[[1]][1], mean.from.file.1, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][2], mean.from.file.2, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][3], mean.from.file.3, tolerance = 10^-15)
    
    print("negative integers")
    mean.from.file.1 <- mean(ds.test_env$same.values.1[,9])
    mean.from.file.2 <- mean(ds.test_env$same.values.2[,9])
    mean.from.file.3 <- mean(ds.test_env$same.values.3[,9])
    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_INTEGER',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)      
    expect_equal(mean.from.servers[[1]][1], mean.from.file.1, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][2], mean.from.file.2, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][3], mean.from.file.3, tolerance = 10^-15)
    
    print("non negative integers")
    mean.from.file.1 <- mean(ds.test_env$same.values.1[,7])
    mean.from.file.2 <- mean(ds.test_env$same.values.2[,7])
    mean.from.file.3 <- mean(ds.test_env$same.values.3[,7])
    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_INTEGER',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)      
    
    expect_equal(mean.from.servers[[1]][1], mean.from.file.1, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][2], mean.from.file.2, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][3], mean.from.file.3, tolerance = 10^-15)
    
    #--------------
    print("positive and negative numeric value")
    mean.from.file.1 <- mean(ds.test_env$same.values.1[,10])
    mean.from.file.2 <- mean(ds.test_env$same.values.2[,10])
    mean.from.file.3 <- mean(ds.test_env$same.values.3[,10])
    mean.from.servers <- ds.mean.o(x='D$NUMERIC',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)      
    expect_equal(mean.from.servers[[1]][1], mean.from.file.1, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][2], mean.from.file.2, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][3], mean.from.file.3, tolerance = 10^-15)
    
    print("positive numeric value")
    mean.from.file.1 <- mean(ds.test_env$same.values.1[,12])
    mean.from.file.2 <- mean(ds.test_env$same.values.2[,12])
    mean.from.file.3 <- mean(ds.test_env$same.values.3[,12])
    mean.from.servers <- ds.mean.o(x='D$POSITIVE_NUMERIC',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)      
    expect_equal(mean.from.servers[[1]][1], mean.from.file.1, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][2], mean.from.file.2, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][3], mean.from.file.3, tolerance = 10^-15)
    
    print("negative numeric")
    mean.from.file.1 <- mean(ds.test_env$same.values.1[,13])
    mean.from.file.2 <- mean(ds.test_env$same.values.2[,13])
    mean.from.file.3 <- mean(ds.test_env$same.values.3[,13])
    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_NUMERIC',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)      
    expect_equal(mean.from.servers[[1]][1], mean.from.file.1, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][2], mean.from.file.2, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][3], mean.from.file.3, tolerance = 10^-15)
    
    print("non negative numeric")
    mean.from.file.1 <- mean(ds.test_env$same.values.1[,11])
    mean.from.file.2 <- mean(ds.test_env$same.values.2[,11])
    mean.from.file.3 <- mean(ds.test_env$same.values.3[,11])
    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_NUMERIC',type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)      
    
    expect_equal(mean.from.servers[[1]][1], mean.from.file.1, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][2], mean.from.file.2, tolerance = 10^-15)
    expect_equal(mean.from.servers[[1]][3], mean.from.file.3, tolerance = 10^-15)
})

context("An mean returns an expected value for non-numerical values")
test_that("The mean function returns an NA for character arguments",
{
  print("character")
  mean.from.servers <- ds.mean.o(x='D$CHARACTER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  expect_equal(is.na(mean.from.servers),TRUE)
  
  print("boolean")
  mean.from.files <- mean(ds.test_env$same.values[,3])
  mean.from.servers <- ds.mean.o(x='D$BOOLEAN',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  expect_equal(mean.from.servers,mean.from.files,tolerance = 10^-15)
})

#context("mathematical properties")
#test_that("residual deviation tends to 0",
#{
#    mean.from.servers <- ds.mean.o(x='D$INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
#    expect_equal(sum(ds.test_env$same.values[,6] - mean.from.servers[[1]][1]), 0, tolerance = 10^-10)
#    
#    mean.from.servers <- ds.mean.o(x='D$POSITIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
#    expect_equal(sum(ds.test_env$same.values[,8] - mean.from.servers[[1]][1]), 0, tolerance = 10^-10)
#   
#    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
#    expect_equal(sum(ds.test_env$same.values[,9] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)
    
#    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
#    expect_equal(sum(ds.test_env$same.values[,7] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)
    #----
#    mean.from.servers <- ds.mean.o(x='D$NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
#    expect_equal(sum(ds.test_env$same.values[,10] - mean.from.servers[[1]][1]), 0, tolerance = 10^-10)
    
#    mean.from.servers <- ds.mean.o(x='D$POSITIVE_NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
#    expect_equal(sum(ds.test_env$same.values[,12] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)
#    
#    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
#    expect_equal(sum(ds.test_env$same.values[,13] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)
#    
#    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_NUMERIC',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
#    expect_equal(sum(ds.test_env$same.values[,11] - mean.from.servers[[1]][1]), 0, tolerance = 10^-8)
#})



#context("incorrect arguments")
#test_that("A suitable value is returned with some incorrect arugements",
#{
#    print(class(ds.test_env$connection.opal))
#    print(ds.mean.o())  
#})

