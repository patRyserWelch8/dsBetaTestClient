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

# context("dsBetaTestClient::ds.testObjExists.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

context("ds.testObjExists.o::smk")
test_that("simple testObjExists", {
    res <- ds.testObjExists.o('D')

    expect_length(res, 1)
    expect_equal(res$return.message, "A valid copy of data object <D> exists in all specified data sources")
})

#test_that("data.frame testObjExists", {
#    myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
#    ds.dataFrame.o(x=myvectors)
#
#    res <- ds.testObjExists.o("ds_new")
#
#    expect_length(res, 1)
#    expect_equal(res$return.message, "A valid copy of data object <ds_new> exists in all specified data sources")
#})


#
# Done
#

# context("dsBetaTestClient::ds.testObjExists.o::smk done")
