

.test.mean.combined <- function(variable.name,some.values)
{
  mean.local <- mean(some.values)
  mean.server <- ds.mean.o(x=variable.name,type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  expect_equal(mean.server[[1]][1], mean.local, tolerance = ds.test_env$tolerance)
}

.test.mean.split <- function(variable.name,some.values.1,some.values.2,some.values.3)
{
  mean.local.1 <- mean(some.values.1)
  mean.local.2 <- mean(some.values.2)
  mean.local.3 <- mean(some.values.3)
  
  mean.server <- ds.mean.o(x=variable.name,type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  expect_equal(mean.server[[1]][1], mean.local.1, tolerance = ds.test_env$tolerance)
  expect_equal(mean.server[[1]][2], mean.local.2, tolerance = ds.test_env$tolerance)
  expect_equal(mean.server[[1]][3], mean.local.3, tolerance = ds.test_env$tolerance)
}

.test.residual.combined <- function(variable.name, some.values)
{
  mean.server <- ds.mean.o(variable.name,type='combine', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  residue <- sum(some.values - mean.server[[1]][1])
  expect_equal(residue, 0, tolerance = ds.test_env$tolerance)
}  


.test.residual.split <- function(variable.name, some.values.1,some.values.2,some.values.3)
{
  mean.server <- ds.mean.o(variable.name,type='split', check=TRUE,save.mean.Nvalid=FALSE, datasources=ds.test_env$connection.opal)
  residue.1 <- sum(some.values.1 - mean.server[[1]][1])
  residue.2 <- sum(some.values.2 - mean.server[[1]][2])
  residue.3 <- sum(some.values.3 - mean.server[[1]][3])
  
  expect_equal(residue.1, 0, tolerance = ds.test_env$tolerance)
  expect_equal(residue.2, 0, tolerance = ds.test_env$tolerance)
  expect_equal(residue.3, 0, tolerance = ds.test_env$tolerance)
}
