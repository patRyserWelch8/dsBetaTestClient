source("definition_tests/def-assign-stats.R")
.test.data.frame.creation <- function(list.variables,data.frame.name)
{
  
  data.frame.server <- ds.dataFrame.o(x=list.variables, newobj = data.frame.name, datasources = ds.test_env$connection.opal)
 
  type <- ds.class(data.frame.name)
  exists <- ds.exists(data.frame.name, datasources = ds.test_env$connection.opal)
  
  for(i in 1:length(exists))
  {
    expect_true(exists[[i]])
  }
  expect_true(type[[1]][1]=="data.frame")
}

.test.data.frame.from.objects <- function(variable.name,variable.created,data.frame.name)
{
  list.variables <- c()
  list.variables[1] <- variable.name
  
  ds.make.o(variable.name,variable.created,datasources = ds.test_env$connection.opal)
 
  data.frame.server <- ds.dataFrame.o(x=list.variables, newobj = data.frame.name, datasources = ds.test_env$connection.opal)
  
  type <- ds.class(data.frame.name)
  exists <- ds.exists(data.frame.name, datasources = ds.test_env$connection.opal)
  
  for(i in 1:length(exists))
  {
    expect_true(exists[[i]])
  }
  expect_true(type[[1]][1]=="data.frame")
}
#
