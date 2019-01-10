#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2019 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
#
# Datashield test suite set up
#

library(opal)
library(dsBaseClient)
library(RCurl)

#define login data
server <- c("study1", "study2", "study3")
url <- c("http://192.168.56.100:8080","http://192.168.56.100:8080","http://192.168.56.100:8080")
user <- c("administrator","administrator","administrator")
password <- c("datashield_test&","datashield_test&","datashield_test&")
table <- c("DASIM.DASIM1", "DASIM.DASIM2", "DASIM.DASIM3")
login.data <- datashield.build.login.data.frame.o(server,url,table,user,password)

#load up the packages required. As P.B scripts suggested
load.packages <- function()
{
  print('Loading packages....')
  print('dsBase')
  package.loaded = require('dsBase')
  if (!package.loaded)
  {
    install.packages('dsBase',repos='http://cran.obiba.org')
    library('dsBase')
  }
  
  print('dsModelling')
  package.loaded = require('dsModelling')
  if (!package.loaded)
  {
    install.packages('dsModelling',repos='http://cran.obiba.org')
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

#load up packages and verify they have been uploaded 
context ("packages are loaded")
load.packages()
test_that("dsBase, dsModelling, dsGraphics, dsStats.",
{
            expect_true(require('dsBase'))
            expect_true(require('dsGraphics'))
            expect_true(require('dsStats'))
            expect_true(require('dsModelling'))
})

#verifies a server (i.e. VM is running)
context("A server is available")
test_that("The virtual machine is loaded.",
{          
            expect_true(url.exists("192.168.56.100:8080", timeout=5))
})

#Verifies a connection to opal has been made 
stats.var <- list('GENDER', 'LAB_TSC')
connection.opal <- datashield.login(logins=login.data, assign=TRUE,variables=stats.var)


context("A connection to opal server has been made")
test_that("The number of servers is the same has setup",
{
  expect_true(length(connection.opal) == length(server))
}          
)


#print(connection.opal)

#ds.dim(x='D', type='combine')
#ds.summary(x='D$GENDER')


#options(verbose=FALSE)

#options(opal.username='administrator',
#        opal.password='password')

#options(opal.url='http://localhost:8080')
#options(opal.url='http://demo.obiba.org:8080')

#server <- c(getOption("opal.server1"), getOption("opal.server2"), getOption("opal.server3"))
#url <- c(getOption("opal.url"), getOption("opal.url"), getOption("opal.url"))
#user <- c(getOption("opal.username"), getOption("opal.username"), getOption("opal.username"))
#password <- c(getOption("opal.password"), getOption("opal.password"), getOption("opal.password"))
#table <- c(getOption("opal.table1"), getOption("opal.table2"), getOption("opal.table3"))

#if (!is.null(getOption("opal.server1"))) {
#    logindata <- data.frame(server,url,user,password,table)

# adminopals <- datashield.login(logins=logindata,assign=TRUE,variables=getOption("datashield.variables", NULL))
# opaladmin::dsadmin.install_package(opal=adminopals,pkg="dsBetaTest",githubusername="datashield",ref="master")
# datashield.logout(adminopals)

#    opals <- datashield.login(logins=logindata,assign=TRUE,variables=getOption("datashield.variables", NULL))
#}
