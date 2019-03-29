source("login_details.r")

if (ds.test_env$secure_login_details)
{
    print('reading values from files')
    ds.test_env$same.values.3 <- read.csv("data_files/data_set_3.csv", header = TRUE)


    ds.test_env$server <- c("study3")
    ds.test_env$url <- c(ds.test_env$ip_address_1,ds.test_env$ip_address_2,ds.test_env$ip_address_3)
    ds.test_env$user <- c(ds.test_env$user_1,ds.test_env$user_2,ds.test_env$user_3)
    ds.test_env$password <- c(ds.test_env$password_1,ds.test_env$password_2,ds.test_env$password_3)
    ds.test_env$table <- c("TESTING.DATASET3")
    ds.test_env$login.data <- datashield.build.login.data.frame.o(ds.test_env$server,
                                                              ds.test_env$url,
                                                              ds.test_env$table,
                                                              ds.test_env$user,
                                                              ds.test_env$password)

    ds.test_env$stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                                  'FACTOR_INTEGER')
}