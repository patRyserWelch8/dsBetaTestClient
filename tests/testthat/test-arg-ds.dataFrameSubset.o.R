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

# context("dsBetaTestClient::ds.dataFrameSubset.o:args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.dataFrameSubset.o::arg::test errors")
test_that("dataFrameSubset_erros", {
    expect_error(ds.dataFrameSubset.o(), "Please provide the name of the data.frame to be subsetted as a character string: eg 'xxx'", fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.dataFrameSubset.o:arg done")
