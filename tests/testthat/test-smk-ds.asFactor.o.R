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

# context("dsBetaTestClient::ds.asFactor.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.survival(list("survtime", "time.id", "female", "age.60"))

#
# Tests
#

context("ds.asFactor.o::smk")

ds.asNumeric.o("D$time.id","TID")

context("ds.asFactor.o::smk::force.factor.levels")

test_that("with no force.factor.levels", {
    ds.asFactor.o("TID", "TID.f1")

    res1 <- ds.class("TID.f1", datasources=ds.test_env$connection.opal)
    res2 <- ds.table1D("TID.f1", datasources=ds.test_env$connection.opal)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

test_that("with forced.factor.levels of 1:6", {
    ds.asFactor.o("TID", "TID.f2", forced.factor.levels=1:6)

    res1 <- ds.class("TID.f2", datasources=ds.test_env$connection.opal)
    res2 <- ds.table1D("TID.f2", datasources=ds.test_env$connection.opal)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

test_that("with force.factor.levels of 0:10", {
    ds.asFactor.o("TID", "TID.f3", forced.factor.levels=0:10)

    res1 <- ds.class("TID.f3", datasources=ds.test_env$connection.opal)
    res2 <- ds.table1D("TID.f3", datasources=ds.test_env$connection.opal)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

test_that("with force.factor.levels of 2:3", {
    ds.asFactor.o("TID", "TID.f4", forced.factor.levels=2:3)

    res1 <- ds.class("TID.f4", datasources=ds.test_env$connection.opal)
    res2 <- ds.table1D("TID.f4", datasources=ds.test_env$connection.opal)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

test_that("with force.factor.levels of c(1,2,3,4,'a','h',5)", {
    ds.asFactor.o("TID", "TID.f5", forced.factor.levels=c(1,2,3,4,'a','h',5))

    res1 <- ds.class("TID.f5", datasources=ds.test_env$connection.opal)
    res2 <- ds.table1D("TID.f5", datasources=ds.test_env$connection.opal)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

# context("dsBetaTestClient::ds.asFactor.o(fixed.dummy.vars)::smk")
context("ds.asFactor.o::smk::fixed.dummy.vars")

test_that("with fixed.dummy.vars of TRUE", {
    ds.asFactor.o("TID", "TID.mat1", fixed.dummy.vars=TRUE)

    res <- ds.class("TID.mat1", datasources=ds.test_env$connection.opal)

    expect_equal("matrix", res$`survival1`)
    expect_equal("matrix", res$`survival2`)
    expect_equal("matrix", res$`survival3`)
})

test_that("with fixed.dummy.vars of TRUE and baseline.level of 6", {
    ds.asFactor.o("TID", "TID.mat6", fixed.dummy.vars=TRUE,baseline.level=6)

    res <- ds.class("TID.mat6", datasources=ds.test_env$connection.opal)

    expect_equal("matrix", res$`survival1`)
    expect_equal("matrix", res$`survival2`)
    expect_equal("matrix", res$`survival3`)
})

#
# Done
#

# context("dsBetaTestClient::ds.asFactor.o 1::smk done")
