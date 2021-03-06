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

# context("dsBetaTestClient::ds.rm.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.survival(list("survtime", "time.id", "female"))

#
# Tests
#

context("ds.rm.o::smk")

test_that("simple test", {
    res1 <- ds.rm.o("nonexistant_object")

    expect_length(res1, 3)
    expect_length(res1$survival1, 1)
    expect_equal(res1$survival1$return.message, "Object to be deleted, i.e. <nonexistant_object> , does not exist so does not need deleting")
    expect_length(res1$survival2, 1)
    expect_equal(res1$survival2$return.message, "Object to be deleted, i.e. <nonexistant_object> , does not exist so does not need deleting")
    expect_length(res1$survival3, 1)
    expect_equal(res1$survival3$return.message, "Object to be deleted, i.e. <nonexistant_object> , does not exist so does not need deleting")
    
    ds.rbind.o(c("D$survtime", "D$time.id", "D$female"), newobj="existing_object")

    res2 <- ds.rm.o("existing_object")

    expect_length(res2, 3)
    expect_length(res2$survival1, 1)
    expect_equal(res2$survival1$return.message, "Object <existing_object> successfully deleted")
    expect_length(res2$survival2, 1)
    expect_equal(res2$survival2$return.message, "Object <existing_object> successfully deleted")
    expect_length(res2$survival3, 1)
    expect_equal(res2$survival3$return.message, "Object <existing_object> successfully deleted")
})

#
# Done
#

# context("dsBetaTestClient::ds.rm.o::smk done")
