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

# context("dsBetaTestClient::ds.matrixDiag::args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.matrixDiag::arg::test errors")
test_that("matrixDiag_erros", {
    res <- ds.matrixDiag()

    expect_length(res, 1)
    expect_equal(res, "Error: x1 must have a value which is a character string, a numeric vector or a scalar", fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.matrixDiag::arg done")
