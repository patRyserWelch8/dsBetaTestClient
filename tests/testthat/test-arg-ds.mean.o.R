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

# context("dsBetaTestClient::ds.mean.o:args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.mean.o::arg::test errors")
test_that("mean_erros", {
    ds.asCharacter.o(x='D$LAB_TSC', newobj="not_a_numeric")

    expect_error(ds.mean.o(), "Please provide the name of the input vector!", fixed=TRUE)
    expect_error(ds.mean.o(x='D$LAB_TSC', type='datashield'), 'Function argument "type" has to be either "both", "combine" or "split"', fixed=TRUE)
    expect_error(ds.mean.o(x='not_a_numeric', checks=TRUE), "The input object must be an integer or a numeric vector.", fixed=TRUE)
})

#context("ds.mean.o::arg::discordant errors")
#test_that("mean_discordant", {
#    res <- ds.mean.o("D$A")
#
#    print("====")
#    print(res)
#    print("====")
#
#    expect_length(res, 1)
#    expect_equal(res, "")
#})

#
# Done
#

# context("dsBetaTestClient::ds.mean.o:arg done")
