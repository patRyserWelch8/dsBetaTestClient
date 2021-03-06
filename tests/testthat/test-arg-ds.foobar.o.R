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
# Set up
#

# context("dsBetaTestClient::ds.foobar.o::arg")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.foobar.o::arg::aggregate")
test_that("NULL opal", {
    calltext <- call("fooBarDS.o")
    expect_error(opal::datashield.aggregate(opal=NULL, expr=calltext), "no applicable method for 'datashield.aggregate' applied to an object of class \"NULL\"", fixed=TRUE)
})

context("ds.foobar.o::arg::aggregate")
test_that("NULL expr", {
    calltext <- call("fooBarDS.o")
    expect_error(opal::datashield.aggregate(opal=ds.test_env$connection.opal, expr=NULL), "Invalid expression type: 'NULL'. Expected a call or character vector.", fixed=TRUE)
})

context("ds.foobar.o::arg::aggregate")
test_that("non existent aggregate foobarDS", {
    calltext <- call("fooBarDS.o")
    expect_error(opal::datashield.aggregate(opal=ds.test_env$connection.opal, expr=calltext), "Command 'fooBarDS.o()' failed on 'sim1': No such DataSHIELD 'AGGREGATE' method with name: fooBarDS.o", fixed=TRUE)
})

context("ds.foobar.o::arg::assign")
test_that("NULL opal", {
    calltext <- call("fooBarDS.o")
    expect_error(opal::datashield.assign(opal=NULL, symbol="new_obj", value=calltext), "no applicable method for 'datashield.assign' applied to an object of class \"NULL\"", fixed=TRUE)
})

#context("ds.foobar.o::arg::assign")
#test_that("NULL symbol", {
#    calltext <- call("fooBarDS.o")
#    res <- opal::datashield.assign(opal=ds.test_env$connection.opal, symbol=NULL, value=calltext)
#    expect_true(is.raw(res))
#    expect_length(res, 0)
#})

context("ds.foobar.o::arg::assign")
test_that("NULL value", {
    calltext <- call("fooBarDS.o")
    expect_error(opal::datashield.assign(opal=ds.test_env$connection.opal, symbol="new_obj", value=NULL), "Invalid value type: 'NULL'. Use quote() to protect from early evaluation.", fixed=TRUE)
})

context("ds.foobar.o::arg::assign")
test_that("non existent assign foobarDS", {
    calltext <- call("fooBarDS.o")
    res <- opal::datashield.assign(opal=ds.test_env$connection.opal, symbol="new_obj", value=calltext)
    expect_true(is.raw(res))
    expect_length(res, 0)
})

#
# Tear down
#

# context("dsBetaTestClient::ds.foobar.o::arg done")
