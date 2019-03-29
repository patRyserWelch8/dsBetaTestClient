
init.login.data<-function()
{
  server <- c("study1", "study2", "study3")
  url <- c("http://192.168.56.100:8080","http://192.168.56.100:8080","http://192.168.56.100:8080")
  user <- c("administrator","administrator","administrator")
  password <- c("datashield_test&","datashield_test&","datashield_test&")
  table <- c("DASIM.DASIM1", "DASIM.DASIM2", "DASIM.DASIM3")
  return(datashield.build.login.data.frame.o(server,url,table,user,password))
}

stats.var <- list('GENDER', 'LAB_TSC')
login.data = init.login.data()
opal.connection <- datashield.login(logins=init.login.data(), assign=TRUE, stats.var)
ds.dim(x='D', type='combine')
ds.summary(x='D$GENDER')
