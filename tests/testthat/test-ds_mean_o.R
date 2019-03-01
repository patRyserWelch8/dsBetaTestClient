context("An integer mean returns an expected numerical value")
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
})

context("mathematical properties")
test_that("residual deviation tends to 0",
{
    mean.from.servers <- ds.mean.o(x='D$INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$same.values[,6] - mean.from.servers[[1]][1]), 0, tolerance = 10^-10)
    
    mean.from.servers <- ds.mean.o(x='D$POSITIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$same.values[,8] - mean.from.servers[[1]][1]), 0, tolerance = 10^-10)
   
    mean.from.servers <- ds.mean.o(x='D$NEGATIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$same.values[,9] - mean.from.servers[[1]][1]), 0, tolerance = 10^-10)
    
    mean.from.servers <- ds.mean.o(x='D$NON_NEGATIVE_INTEGER',type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
    expect_equal(sum(ds.test_env$same.values[,7] - mean.from.servers[[1]][1]), 0, tolerance = 10^-10)
})




