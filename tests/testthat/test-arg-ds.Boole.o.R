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

# context("dsBetaTestClient::ds.Boole.o:args test")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("ds.Boole.o::arg::test errors")
test_that("Boole_erros", {
    expect_error(ds.Boole.o(), "Please provide the name of the column or scalar that holds V1!", fixed=TRUE)
    expect_error(ds.Boole.o(V1="D$LAB_TSC"), "Please provide the name of a column or scalar holding V2 or declare a scalar in character format: eg '3'", fixed=TRUE)
    expect_error(ds.Boole.o(V1="D$LAB_TSC", V2="LAB_TRIG"), "Please provide a Boolean operator in character format: eg '==' or '>=' or '<' or '!='", fixed=TRUE)
    expect_error(ds.Boole.o(V1="D$LAB_TSC", V2="LAB_TRIG", Boolean.operator="==", na.assign=2), "Error: na.assign must be a character string taking value 'NA', '0' or '1'- if <na.action> not specified default is 'NA'", fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.Boole.o:arg done")
