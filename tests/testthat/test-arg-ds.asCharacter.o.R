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

# context("dsBetaTestClient::ds.asCharacter.o:args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_discordant_datasets.R")

connect.discordant.dataset.simple(list("A", "B", "C"))

#
# Tests
#

context("ds.asCharacter.o::arg::test errors")
test_that("asCharacter_erros", {
    expect_error(ds.asCharacter.o(), "Please provide the name of the input vector!", fixed=TRUE)
})

# context("ds.asCharacter.o::arg::discordant errors")
# test_that("asCharacter_discordant", {
#     res <- ds.asCharacter.o("D$A")
#
#     print("====")
#     print(res)
#     print("====")
#
#     expect_length(res, )
#     expect_equal(res, "")
# })

#
# Done
#

# context("dsBetaTestClient::ds.asCharacter.o:arg done")
