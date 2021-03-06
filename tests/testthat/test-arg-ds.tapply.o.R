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

# context("dsBetaTestClient::ds.tapply.o:args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "GENDER"))

#
# Tests
#

context("ds.tapply.o::arg::test errors")
test_that("tapply_erros", {
    res1 <- ds.tapply.o()

    expect_length(res1, 1)
    expect_equal(res1, "Error: Please provide the name of the variable to be summarized, as a character string")

    res2 <- ds.tapply.o('D$LAB_TSC')

    expect_length(res2, 3)
    expect_equal(res2$Error.message, "Error: Please provide the name of the single factor or")
    expect_equal(res2$Err.cont2, "the list of factors to index the variable to be summarized.")
    expect_equal(res2$Err.cont3, "In either case the argument must be specified in inverted commas")

    res3 <- ds.tapply.o('D$LAB_TSC', 'D$GENDER')

    expect_length(res3, 1)
    expect_equal(res3, "Error: Please provide a valid summarizing function, as a character string")
})

#
# Done
#

# context("dsBetaTestClient::ds.tapply.o:arg done")
