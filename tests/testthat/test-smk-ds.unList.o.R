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

# context("dsBetaTestClient::ds.unList.o:smoke")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("GENDER"))

#
# Tests
#

# context("dsBetaTestClient::ds.unList.o():smoke simple test")
context("ds.unList.o()::smoke::simple test")

test_that("simple test", {
    ds.unList.o(x.name="D$GENDER")

    res <- ds.unList(D$GENDER.list)

    print("====")
    print(res)
    print("====")

    expect_length(res, 3)
})

#
# Done
#

# context("dsBetaTestClient::ds.unList.o 1:smoke done")
