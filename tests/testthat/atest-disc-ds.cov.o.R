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

context("ds.cov.o::disc::type=combine")
test_that("var values [both]", {
    myvectors <- c("D$survtime", "D$time.id")
    ds.dataFrame.o(x=myvectors, newobj="unsubset_df")

    ds.dataFrameSubset.o(df.name="unsubset_df", V1.name="D$survtime", V2.name="D$time.id", Boolean.operator=">", newobj="subset_df")

    length_res <- ds.length.o("subset_df")

    expect_length(length_res, 4)
    expect_equal(length_res$`length of subset_df in survival1`, 2)
    expect_equal(length_res$`length of subset_df in survival2`, 2)
    expect_equal(length_res$`length of subset_df in survival3`, 2)
    expect_equal(length_res$`total length of subset_df in all studies combined`, 6)

    cov.res <- ds.cov.o(x="subset_df$survtime", y="subset_df$time.id", type="combine")

    print("====")
    print(cov.res)
    print("====")

    expect_length(cov.res, 5)
})

#
# Done
#
