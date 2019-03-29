#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
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


source("init_all_dataset.r") 
#connect to a server
context("VM problems")
test_that("The virtual machine is loaded. ",
{          
    expect_that(url.exists("192.168.56.100:8080", timeout=5), is_true())
    print("A server is available")
})

#define test_environment variables - connection to data shield and read from local files



#load the packages required for datashield to work
source("load_libraries.R")
test_that(" The packages dsBase, dsModelling, dsGraphics, dsStats are installed and loaded.",
{
  expect_true(require('dsBase'))
  expect_true(require('dsGraphics'))
  expect_true(require('dsStats'))
  expect_true(require('dsModelling'))
})


print ("connect to server")
ls()
ds.test_env$connection.opal <- datashield.login(logins=ds.test_env$login.data, assign=TRUE,variables=ds.test_env$stats.var)
print(class(ds.test_env$connection.opal))

test_that("The number of servers the same has setup",
{
  expect_true(length(ds.test_env$connection.opal) == length(ds.test_env$server))
})


dimensions <- ds.dim(x='D',type='combine',datasources = ds.test_env$connection.opal)
#print("dimensions")
#print(dimensions[[1]][1])
#print(nrow(ds.test_env$same.values))
#print(dimensions[[1]][1] == nrow(ds.test_env$same.values))

context("The number of rows of the test data are the same on the server and locally")
test_that("The of rows are the same",
{
  expect_true(dimensions[[1]][1] == nrow(ds.test_env$same.values))
})

