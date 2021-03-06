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

# context("dsBetaTestClient::ds.rm.o::arg")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.survival(list("survtime", "time.id", "female"))

#
# Tests
#

context("ds.rm.o::arg::test errors")

test_that("ds.rm.o erros", {
    expect_error(ds.rm.o(), "Please provide the name of the object to be deleted (eg 'object.name') as the x.name argument", fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.rm.o::arg done")
