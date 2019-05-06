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

context("dsBetaTestClient::ds.length.o:smoke")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("dsBetaTestClient::ds.length.o():smoke")
test_that("basic length", {
    res.length <- ds.length.o('D$LAB_TSC')

    expect_equal(res.length$`length of D$LAB_TSC in sim1`, 2163)
    expect_equal(res.length$`length of D$LAB_TSC in sim2`, 3088)
    expect_equal(res.length$`length of D$LAB_TSC in sim3`, 4128)
    expect_equal(res.length$`total length of D$LAB_TSC in all studies combined`, 9379)
})

#
# Done
#

context("dsBetaTestClient::ds.length.o:smoke done")