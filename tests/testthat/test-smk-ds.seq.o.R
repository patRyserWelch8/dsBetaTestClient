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

# context("dsBetaTestClient::ds.seq.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.seq.o::smk")
test_that("simplest ds.seq.o", {
    seq.res <- ds.seq.o("1", "1", "10", "D$LAB_TSC", "obj")

    expect_true(length(seq.res) == 2)
    expect_equal(seq.res[[1]], "A data object <obj> has been created in all specified data sources")
    expect_equal(seq.res[[2]], "<obj> appears valid in all sources")
})

#
# Tear down
#

# context("dsBetaTestClient::ds.seq.o::smk done")
