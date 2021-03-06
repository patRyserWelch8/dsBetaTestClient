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

# context("dsBetaTestClient::ds.Boole.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("ds.Boole.o::smk")
test_that("simple boole", {
    res <- ds.Boole.o("D$LAB_TSC", "D$LAB_TRIG", "==")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <D$LAB_TSC_Boole> has been created in all specified data sources")
    expect_equal(res$validity.check, "<D$LAB_TSC_Boole> appears valid in all sources")
})

#
# Done
#

# context("dsBetaTestClient::ds.Boole.o::smk done")
