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

# context("dsBetaTestClient::ds.dataFrame.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

context("ds.dataFrame.o::smk::create a dataframe")
test_that("dataframe_exists", {
    vectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame.o(x=vectors)
    res <- ds.ls(datasources=ds.test_env$connection.opal)

    expect_equal(res$sim1[2], "df_new")
    expect_equal(res$sim2[2], "df_new")
    expect_equal(res$sim3[2], "df_new")
})

context("ds.dataFrame.o::smk::create a dataframe, with DataSHIELD.checks")
test_that("dataframe_exists, with DataSHIELD.checks", {
    vectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame.o(x=vectors, DataSHIELD.checks=TRUE)
    res <- ds.ls(datasources=ds.test_env$connection.opal)

    expect_equal(res$sim1[2], "df_new")
    expect_equal(res$sim2[2], "df_new")
    expect_equal(res$sim3[2], "df_new")
})

#
# Done
#

## context("dsBetaTestClient::ds.dataFrame.o::smk done")
