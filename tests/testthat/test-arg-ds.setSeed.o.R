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

# context("dsBetaTestClient::ds.setSeed.o::args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.setSeed.o::arg::test errors")
test_that("setSeed_erros", {
    res <- ds.setSeed.o(seed.as.integer="Test")

    expect_equal(res, "ERROR terminated: seed.as.integer must be set as an integer [numeric] or as being NULL")
})

#
# Done
#

# context("dsBetaTestClient::ds.setSeed.o::arg done")
