#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2018 University of Newcastle upon Tyne. All rights reserved.
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


source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.survival(list("survtime", "time.id", "female"))

#
# Tests
#

context("ds.rm.o::disc")

test_that("simple test", {
    res1 <- ds.rm.o("thisisareallylongname_testing_datashield")

    expect_length(res1, 3)
    expect_length(res1$survival1, 1)
    expect_equal(res1$survival1$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    expect_length(res1$survival2, 1)
    expect_equal(res1$survival2$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    expect_length(res1$survival3, 1)
    expect_equal(res1$survival3$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    

    res2 <- ds.rm.o(c("erty33u88","erty33u88","erty33u88","erty33u88","erty33u88"))
    expect_length(res2, 3)
    expect_length(res2$survival1, 1)
    expect_equal(res2$survival1$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    expect_length(res2$survival2, 1)
    expect_equal(res2$survival2$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    expect_length(res2$survival3, 1)
    expect_equal(res2$survival3$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
})

#
# Done
#


