library(opal)
library(dsBaseClient)
library(RCurl)

load.packages <- function()
{
  print('Loading packages....')
  print('dsBase')
  package.loaded = require('dsBase')
  if (!package.loaded)
  {
    install.packages('dsBase',repos='https://cran.obiba.org')
    library('dsBase')
  }
  
  print('dsModelling')
  package.loaded = require('dsModelling')
  if (!package.loaded)
  {
    install.packages('dsModelling',repos='https://cran.obiba.org')
    library('dsModelling')
  }
  
  print('dsGraphics')
  package.loaded = require('dsGraphics')
  if (!package.loaded)
  {
    install.packages('dsGraphics',repos='http://cran.obiba.org')
    library('dsGraphics')
  }
  
  print('dsStats')
  package.loaded = require('dsStats')
  if (!package.loaded)
  {
    install.packages('dsStats',repos='http://cran.obiba.org')
    library('dsStats')
  }
}

load.packages()

server <- c("study1", "study2", "study3")
url <- c("https://192.168.56.100:8443","https://192.168.56.100:8443","https://192.168.56.100:8443")
user <- c("administrator","administrator","administrator")
password <- c("datashield_test&","datashield_test&","datashield_test&")
table <- c("TESTING.DATASET1", "TESTING.DATASET2", "TESTING.DATASET3")
login.data <- datashield.build.login.data.frame.o(server,url,table,user,password)


address ="http://192.168.56.100:8080"
print(address)
url.exists(address, timeout=5)

print ("connect to server")
stats.var <- list('ID','CHARACTER', 'LOGICAL','NA_VALUES','INTEGER','NULL_VALUES',
                  'NON_NEGATIVE_INTEGER','POSITIVE_INTEGER','NEGATIVE_INTEGER',
                  'NUMERIC', 'NON_NEGATIVE_NUMERIC','POSITIVE_NUMERIC','NEGATIVE_NUMERIC','FACTOR_CHARACTER', 
                  'FACTOR_INTEGER')

#stats.var <- list('FACTOR_CHARACTER')

connection.opal <- datashield.login(logins=login.data, assign=TRUE,variables=stats.var)

ds.dim('D')
ds.colnames('D')
ds.class('D$FACTOR_INTEGER')

ds.asNumeric("D$FACTOR_INTEGER","FACTOR_INT")
stat.factor <- ds.asFactor.o('FACTOR_INT','FACTOR_INT.f', datasources = connection.opal)
#print(stat.factor)


