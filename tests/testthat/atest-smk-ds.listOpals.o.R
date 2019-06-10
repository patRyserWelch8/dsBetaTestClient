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

# context("dsBetaTestClient::ds.listOpals.o:smoke")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

context("ds.listOpals.o()::smoke::check results")
test_that("check results", {
    message <- "*  This function lists all Opal objects in the R analysis environment\n\n\n*  There is only one set of opals available,\n that is: 'ds.test_env$connection.opal'\n\n\n\n*  This set of Opals has been copied to create 'default.opals',\n which all DataSHIELD functions will now use by default.\n If you want to change the default Opal object,\n please run the function ds.setDefaultOpals() again. \n\n"

    expect_message(res <- ds.listOpals.o(), message)
    expect_null(res)
})

#
# Done
#

# context("dsBetaTestClient::ds.listOpals.o:smoke done")
