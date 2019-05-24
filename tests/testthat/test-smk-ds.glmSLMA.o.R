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

context("dsBetaTestClient::ds.glmSLMA.o:smoke")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("dsBetaTestClient::ds.glmSLMA.o():smoke")
test_that("simple glmSLMA", {
    glmSLMA.res <- ds.glmSLMA.o('D$LAB_TSC~D$LAB_TRIG', family="gaussian")

    expect_equal(class(glmSLMA.res$study3$family), "family")
    expect_equal(class(glmSLMA.res$study3$coefficients), "matrix")
    expect_equal(class(glmSLMA.res$input.beta.matrix.for.SLMA), "matrix")
    expect_equal(class(glmSLMA.res$input.se.matrix.for.SLMA), "matrix")
    expect_equal(class(glmSLMA.res$SLMA.pooled.estimates), "matrix")
})

#
# Done
#

context("dsBetaTestClient::ds.glmSLMA.o:smoke done")