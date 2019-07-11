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

# context("dsBetaTestClient::ds.tapply.assign.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "GENDER"))

#
# Tests
#

context("ds.tapply.assign.o::smk")
test_that("simplest 'ds.tapply.assign.o'", {
    list <- ds.tapply.assign.o(X.name='D$LAB_TSC', INDEX.names=c('D$GENDER'), FUN.name='sum', newobj='temp.obj')

    expect_true(length(list) == 0)
})

#
# Tear down
#

# context("dsBetaTestClient::ds.tapply.assign.o::smk done")

