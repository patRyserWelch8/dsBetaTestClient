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

# context("dsBetaTestClient::ds.recodeValues.o:args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.recodeValues.o::arg::test errors")
test_that("recodeValues_erros", {
    expect_error(ds.recodeValues.o(), "Please provide the name of the variable to be recoded: eg 'xxx'", fixed=TRUE)
    expect_error(ds.recodeValues.o('D$LAB_TSC'), "Please provide a vector specifying the values to be replaced eg c(1,7,NA)", fixed=TRUE)
    expect_error(ds.recodeValues.o('D$LAB_TSC', values2replace.vector=c(0,1)), "Please provide a vector specifying the new values to be set eg c(3,NA,4)", fixed=TRUE)
    expect_error(ds.recodeValues.o('D$LAB_TSC', values2replace.vector=c(0,1), new.values.vector=c(-10)), "Please ensure that values2replace.vector and new.values.vector have same length and are in the same order", fixed=TRUE)
    expect_error(ds.recodeValues.o('D$LAB_TSC', values2replace.vector=c(1,1), new.values.vector=c(-10, 10)), "No value may appear more than once in the values2replace.vector", fixed=TRUE)
    expect_error(ds.recodeValues.o('D$LAB_TSC', values2replace.vector=c(NA), new.values.vector=c(10)), "The <values2replace.vector> consists solely of one element which is NA. Please see details\nin the help information for ds.recodeValues.o to find an easy work around that circumvents\nthe coding restriction that prohibits this particular way of specifying this recoding request", fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.recodeValues.o:arg done")
