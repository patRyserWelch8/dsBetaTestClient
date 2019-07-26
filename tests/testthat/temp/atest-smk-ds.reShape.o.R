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

# context("dsBetaTestClient::ds.reShape.o::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.survival(list("id", "study.id", "time.id", "cens", "age.60", "female"))

#
# Tests
#

context("ds.reShape.o::smk")
test_that("simplest ds.reShape.o", {
    vectors <- c("D$id", "D$study.id", "D$time.id", "D$cens", "D$age.60", "D$female")
    ds.dataFrame.o(x=vectors, newobj = "sur_df")

    print("====")
    print(ds.colnames("sur_df", datasources=ds.test_env$connection.opal))
    print("====")

    res <- ds.reShape.o(data.name="sur_df", v.names="outcome", timevar.name="D$time.id", idvar.name="D$age.60", direction='wide', newobj="reshape_obj")

    print("====")
    print(res)
    print("====")

    expect_length(res, 3)
})

#
# Done
#

# context("dsBetaTestClient::ds.reShape.o::smk done")
