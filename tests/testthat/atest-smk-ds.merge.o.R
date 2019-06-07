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

# context("dsBetaTestClient::ds.merge.o:smoke")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL", "DIS_CVA", "DIS_AMI"))

#
# Tests
#

context("ds.merge.o()::smoke")

test_that("simple test", {
    spec_vectors_1 <- c('D$LAB_TSC', 'D$LAB_HDL')
    spec_vectors_2 <- c('D$LAB_TRIG', 'D$DIS_AMI')
    ds.dataFrame.o(x=spec_vectors_1, newobj="test_1_df")
    ds.dataFrame.o(x=spec_vectors_2, newobj="test_2_df")

    res <- ds.merge.o(x.name="test_1_df", y.name="test_2_df", by.x.names="LAB_TSC", by.y.names="LAB_TRIG", newobj="merge_newobj")

    print("====")
    print(res)
    print("====") 

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <merge_newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<merge_newobj> appears valid in all sources")

    new.res <- ds.class("merge_newobj", datasources=ds.test_env$connection.opal)

    expect_length(new.res, 3)
    expect_equal(new.res$survival1, "numeric")
    expect_equal(new.res$survival2, "numeric")
    expect_equal(new.res$survival3, "numeric")
})

#
# Done
#

# context("dsBetaTestClient::ds.merge.o 1:smoke done")
