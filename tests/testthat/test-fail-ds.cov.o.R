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

<<<<<<< HEAD:tests/testthat/test-fail-ds.cov.o.R
=======
# context("dsBetaTestClient::ds.rilm.b::smk")

>>>>>>> upstream/master:tests/testthat/atest-smk-ds.rilm.b.R
source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.survival(list("survtime", "time.id", "female"))

#
# Tests
#

<<<<<<< HEAD:tests/testthat/test-fail-ds.cov.o.R
context("ds.cov.o()::forceFail")

test_that("simple test", {
    res <- ds.cov.o(x="D$survtime", y="D$time.id")


    expect_length(res, 3)
    expect_length(res, 100)

    expect_equal(class(res[[1]]$`Number of missing values in each variable`), "matrix")
    expect_equal(class(res[[1]]$`Number of missing values in each variable`), "horse")
=======
context("ds.rilm.b::smk")
test_that("simple dim", {
    rilm.res <- ds.rilm.b(c('D$LAB_TSC'), y="D$LAB_HDL")
>>>>>>> upstream/master:tests/testthat/atest-smk-ds.rilm.b.R

})

#
# Done
#
<<<<<<< HEAD:tests/testthat/test-fail-ds.cov.o.R
=======

# context("dsBetaTestClient::ds.rilm.b:smk done")
>>>>>>> upstream/master:tests/testthat/atest-smk-ds.rilm.b.R
