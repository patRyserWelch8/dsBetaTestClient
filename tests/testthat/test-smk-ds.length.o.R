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

# context("dsBetaTestClient::ds.length.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.length.o::smk")
test_that("basic length, both", {
    res.length <- ds.length.o('D$LAB_TSC', type='both')

    expect_length(res.length, 4)
    expect_equal(res.length$`length of D$LAB_TSC in sim1`, 2163)
    expect_equal(res.length$`length of D$LAB_TSC in sim2`, 3088)
    expect_equal(res.length$`length of D$LAB_TSC in sim3`, 4128)
    expect_equal(res.length$`total length of D$LAB_TSC in all studies combined`, 9379)
})

test_that("basic length, split", {
    res.length <- ds.length.o('D$LAB_TSC', type='split')

    expect_length(res.length, 3)
    expect_equal(res.length$`length of D$LAB_TSC in sim1`, 2163)
    expect_equal(res.length$`length of D$LAB_TSC in sim2`, 3088)
    expect_equal(res.length$`length of D$LAB_TSC in sim3`, 4128)
})

test_that("basic length, combine", {
    res.length <- ds.length.o('D$LAB_TSC', type='combine')

    expect_length(res.length, 1)
    expect_equal(res.length$`total length of D$LAB_TSC in all studies combined`, 9379)
})

test_that("basic length, both", {
    res.length <- ds.length.o('D$LAB_TSC', type='both', check=TRUE)

    expect_length(res.length, 4)
    expect_equal(res.length$`length of D$LAB_TSC in sim1`, 2163)
    expect_equal(res.length$`length of D$LAB_TSC in sim2`, 3088)
    expect_equal(res.length$`length of D$LAB_TSC in sim3`, 4128)
    expect_equal(res.length$`total length of D$LAB_TSC in all studies combined`, 9379)
})

test_that("basic length, split", {
    res.length <- ds.length.o('D$LAB_TSC', type='split', check=TRUE)

    expect_length(res.length, 3)
    expect_equal(res.length$`length of D$LAB_TSC in sim1`, 2163)
    expect_equal(res.length$`length of D$LAB_TSC in sim2`, 3088)
    expect_equal(res.length$`length of D$LAB_TSC in sim3`, 4128)
})

test_that("basic length, combine", {
    res.length <- ds.length.o('D$LAB_TSC', type='combine', check=TRUE)

    expect_length(res.length, 1)
    expect_equal(res.length$`total length of D$LAB_TSC in all studies combined`, 9379)
})

#
# Done
#

# context("dsBetaTestClient::ds.length.o::smk done")
