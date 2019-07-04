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

# context("dsBetaTestClient::ds.message.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.message.o::smk")
test_that("request message", {
    message.res <- ds.message.o('Test')

    expect_length(message.res, 3)
    expect_equal(message.res$sim1, "Error: the object <message.object.name> does not exist in this datasource")
    expect_equal(message.res$sim2, "Error: the object <message.object.name> does not exist in this datasource")
    expect_equal(message.res$sim3, "Error: the object <message.object.name> does not exist in this datasource")
})

context("ds.message.o::smk")
test_that("request message for NULL", {
    expect_error(ds.message.o(NULL), "Please provide the name of the studyside list object that holds the message\n in character format ie: 'object.name' in inverted commas", fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.message.o::smk done")
