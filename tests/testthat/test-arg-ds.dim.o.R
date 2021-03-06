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

# context("dsBetaTestClient::ds.dim.o:args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.dim.o::arg::test errors")
test_that("dim_erros", {
    expect_error(ds.dim.o(), "Please provide a the name of a data.frame or matrix!", fixed=TRUE)
    expect_error(ds.dim.o(x="F", checks = TRUE), "The input object must be a table structure!", fixed=TRUE)
    expect_error(ds.dim.o(x="D", type = "other"), 'Function argument "type" has to be either "both", "combine" or "split"', fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.dim.o:arg done")
