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

context("dsBetaTestClient::ds.dataFrame.o:smoke")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

context("dsBetaTestClient::ds.dataFrame.o():smoke create a dataframe")
test_that("dataframe_exists", {
    myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame.o(x=myvectors)
    res <- ds.ls()

    expect_equal(ds.ls()$sim1[2], "dframe")
    expect_equal(ds.ls()$sim2[2], "dframe")
    expect_equal(ds.ls()$sim3[2], "dframe")
})

context("dsBetaTestClient::ds.dataFrame.o() errors:smoke")
test_that("dataframe_errors", {
    expect_error(ds.dataframe(), "argument is of length zero", fixed=TRUE)
})

#
# Done
#

context("dsBetaTestClient::ds.dataFrame.o:smoke done")
