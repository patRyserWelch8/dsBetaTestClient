#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2019 University of Newcastle upon Tyne. All rights reserved.
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

# context("dsBetaTestClient::ds.look.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL"))

#
# Tests
#

context("ds.look.o::smk")
test_that("simple look", {
    res <- ds.look.o("lengthDS.o('D$LAB_TSC')")

    expect_length(res, 1)
    expect_length(res$output, 3)
    expect_equal(res$output$sim1, 1)
    expect_equal(res$output$sim2, 1)
    expect_equal(res$output$sim3, 1)
})

#
# Done
#

# context("dsBetaTestClient::ds.look.o::smk done")
