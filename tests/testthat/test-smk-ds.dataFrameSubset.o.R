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

# context("dsBetaTestClient::ds.dataFrameSubset.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

# context("dsBetaTestClient::ds.dataFrameSubset.o::smk create a subset dataframe")
context("ds.dataFrameSubset.o::smk::create a subset dataframe")
test_that("dataFrameSubset_exists", {
    myvectors <- c('D$LAB_TSC', 'D$LAB_HDL')
    ds.dataFrame.o(x=myvectors, newobj="unsubset_df")

    res <- ds.dataFrameSubset.o(df.name="unsubset_df", V1.name="D$LAB_TSC", V2.name="D$LAB_HDL", Boolean.operator="!=", newobj="subset_df")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <subset_df> has been created in all specified data sources")
    expect_equal(res$validity.check, "<subset_df> appears valid in all sources")
})

#
# Done
#

# context("dsBetaTestClient::ds.dataFrameSubset.o::smk done")
