ds.test_env <- new.env()
print('reading values from files')
ds.test_env$same.values.1 <- read.csv("data_files/data_set_1.csv", header = TRUE)
ds.test_env$same.values.2 <- read.csv("data_files/data_set_2.csv", header = TRUE)
ds.test_env$same.values.3 <- read.csv("data_files/data_set_3.csv", header = TRUE)
ds.test_env$same.values   <- rbind(ds.test_env$same.values.1,ds.test_env$same.values.2,ds.test_env$same.values.3)


ds.test_env$server <- c("study1", "study2", "study3")
ds.test_env$url <- c("http://192.168.56.100:8080","http://192.168.56.100:8080","http://192.168.56.100:8080")
ds.test_env$user <- c("administrator","administrator","administrator")
ds.test_env$password <- c("datashield_test&","datashield_test&","datashield_test&")
#ds.test_env$table <- c("DASIM.DASIM1", "DASIM.DASIM2", "DASIM.DASIM3")
ds.test_env$table <- c("TESTING.SAMEVALUES1", "TESTING.SAMEVALUES2", "TESTING.SAMEVALUES3")
ds.test_env$login.data <- datashield.build.login.data.frame.o(ds.test_env$server,
                                                              ds.test_env$url,
                                                              ds.test_env$table,
                                                              ds.test_env$user,
                                                              ds.test_env$password)

ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                              'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                              'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC')

