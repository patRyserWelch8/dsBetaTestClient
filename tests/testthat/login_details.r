ds.test_env <- new.env()
ds.test_env$server_ip_address = "localhost"
ds.test_env$ip_address_1 <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="")
ds.test_env$ip_address_2 <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="")
ds.test_env$ip_address_3 <- paste("http://", ds.test_env$server_ip_address, ":8080", sep="")

ds.test_env$user_1 <- "administrator"
ds.test_env$user_2 <- "administrator"
ds.test_env$user_3 <- "administrator"

ds.test_env$password_1 <- "datashield_test&"
ds.test_env$password_2 <- "datashield_test&"
ds.test_env$password_3 <- "datashield_test&"

ds.test_env$secure_login_details = TRUE
