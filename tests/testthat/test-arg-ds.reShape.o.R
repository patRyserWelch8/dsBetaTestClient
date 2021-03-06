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

# context("dsBetaTestClient::ds.reShape.o:args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.reShape.o::arg::test errors")
test_that("reShape_erros", {
    expect_error(ds.reShape.o(), "Please provide the name of the list that holds the input vectors!", fixed=TRUE)
    expect_error(ds.reShape.o(data.name="test", sep=TRUE), "'sep' must be a character string", fixed=TRUE)
    expect_error(ds.reShape.o(data.name="test", sep=""), "'sep' must be a character string", fixed=TRUE)
    expect_error(ds.reShape.o(data.name="test", sep="ts"), "'sep' must be a character string", fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.reShape.o:arg done")
