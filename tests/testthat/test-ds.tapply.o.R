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

context("dsBetaTestClient::ds.tapply.o")

options(opal.server1="sim1", opal.server2="sim2", opal.server3="sim3")
options(opal.table1="CNSIM.CNSIM1", opal.table2="CNSIM.CNSIM2", opal.table3="CNSIM.CNSIM3")
options(datashield.variables=list("LAB_TSC","GENDER"))
source("setup.R")

#
# Tests
#

context("dsBetaTestClient::ds.tapply.o()")
test_that("simplest 'ds.tapply.o'", {
    list <- ds.tapply.o('D$LAB_TSC', INDEX.names='GENDER', FUN.name='sum', datasources=opals)

    expect_true(length(list) == 0)
})

#
# Tear down
#

source("teardown.R")

context("dsBetaTestClient::ds.tapply.o done")
