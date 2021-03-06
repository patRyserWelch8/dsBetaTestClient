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

# context("dsBetaTestClient::ds.message.o::arg")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.message.o::arg")
test_that("request message for NULL", {
    expect_error(ds.message.o(NULL), "Please provide the name of the studyside list object that holds the message\n in character format ie: 'object.name' in inverted commas", fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.message.o::arg done")
