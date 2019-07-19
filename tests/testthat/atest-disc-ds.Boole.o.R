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

connect.smk.dataset.sim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("ds.Boole.o::disc::type=both")
test_that("boole values [both]", {
    myvectors <- c("D$LAB_TSC", "D$LAB_TRIG")
    ds.dataFrame.o(x=myvectors, newobj="unsubset_df")

    ds.dataFrameSubset.o(df.name="unsubset_df", V1.name="D$LAB_TSC", V2.name="D$LAB_TRIG", Boolean.operator=">", newobj="subset_df")

    length_res <- ds.length.o("subset_df")
    
    expect_length(length_res, 4)
    expect_equal(length_res$`length of subset_df in sim1`, 2)
    expect_equal(length_res$`length of subset_df in sim2`, 2)
    expect_equal(length_res$`length of subset_df in sim3`, 2)
    expect_equal(length_res$`total length of subset_df in all studies combined`, 6)

    expect_error(boole_res <- ds.Boole.o("unsubset_df$LAB_TSC", "unsubset_df$LAB_TRIG", "==", newobj="newbool"), "argument is of length zero")

    print(ds.message.o("newbool"))

    expect_length(boole_res, 2)
    expect_equal(boole_res$is.object.created, "A data object <D$LAB_TSC_Boole> has been created in all specified data sources")
    expect_equal(boole_res$validity.check, "<D$LAB_TSC_Boole> appears valid in all sources")
})

#
# Done
#
