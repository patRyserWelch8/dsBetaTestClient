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

# context("dsBetaTestClient::ds.listClientsideFunctions.o:smoke")

# source("connection_to_datasets/init_all_datasets.R")
# source("connection_to_datasets/init_smk_datasets.R")

# connect.smk.dataset.sim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

context("ds.listClientsideFunctions.o()::smoke::check results")
test_that("check results", {
    print("======")
    ds.listClientsideFunctions.o(all.functions=FALSE, dsBetaTestClient=TRUE)
    print("======")

    output <-      paste(
'\n### dsBetaTestClient functions',
' [1] "checkClass"',
' [2] "colPercent"',
' [3] "ds.asCharacter.o"',
' [4] "ds.asDataMatrix.o"',
' [5] "ds.asFactor.o"',
' [6] "ds.asInteger.o"',
' [7] "ds.asList.o"',
' [8] "ds.asLogical.o"',
' [9] "ds.asMatrix.o"',
'[10] "ds.asNumeric.o"',
'[11] "ds.Boole.o"',
'[12] "ds.cbind.o"',
'[13] "ds.cor.o"',
'[14] "ds.cov.o"',
'[15] "ds.dataFrame.o"',
'[16] "ds.dataFrameSort.o"',
'[17] "ds.dataFrameSubset.o"',
'[18] "ds.dim.o"',
'[19] "ds.glm.o"',
'[20] "ds.glmSLMA.o"',
'[21] "ds.length.o"',
'[22] "ds.lexis.o"',
'[23] "ds.listClientsideFunctions.o"',
'[24] "ds.listDisclosureSettings.o"',
'[25] "ds.listOpals.o"',
'[26] "ds.listServersideFunctions.o"',
'[27] "ds.look.o"',
'[28] "ds.make.o"',
'[29] "ds.matrix"',
'[30] "ds.matrixDet"',
'[31] "ds.matrixDet.report"',
'[32] "ds.matrixDiag"',
'[33] "ds.matrixDimnames"',
'[34] "ds.matrixInvert"',
'[35] "ds.matrixMult"',
'[36] "ds.matrixTranspose"',
'[37] "ds.mean.o"',
'[38] "ds.meanSdGp"',
'[39] "ds.merge.o"',
'[40] "ds.message.o"',
'[41] "ds.rbind.o"',
'[42] "ds.rBinom.o"',
'[43] "ds.recodeValues.o"',
'[44] "ds.reShape.o"',
'[45] "ds.rilm.b"',
'[46] "ds.rm.o"',
'[47] "ds.rNorm.o"',
'[48] "ds.rPois.o"',
'[49] "ds.rUnif.o"',
'[50] "ds.scatterPlot.o"',
'[51] "ds.seq.o"',
'[52] "ds.setDefaultOpals.o"',
'[53] "ds.setSeed.o"',
'[54] "ds.table2D.o"',
'[55] "ds.tapply.assign.o"',
'[56] "ds.tapply.o"',
'[57] "ds.testObjExists.o"',
'[58] "ds.tTestF"',
'[59] "ds.unList.o"',
'[60] "ds.var.o"',
'[61] "extract"',
'[62] "findLoginObjects"',
'[63] "getOpals"',
'[64] "getPooledMean"',
'[65] "getPooledVar"',
'[66] "glmChecks"',
'[67] "init.object.list.global.environment"',
'[68] "init.object.list.testing.environment"',
'[69] "init.opal.list"',
'[70] "isAssigned"',
'[71] "isDefined"',
'[72] "library.dynam.unload"',
'[73] "logical2int"',
'[74] "rowPercent"',
'[75] "subsetHelper"',
'[76] "system.file"',
'[77] "tTestHelper1"',
'[78] "tTestHelper2"', sep="\n")

    expect_output(ds.listClientsideFunctions.o(all.functions=FALSE, dsBetaTestClient=TRUE), output, fixed=TRUE)
})

#
# Done
#

# context("dsBetaTestClient::ds.listClientsideFunctions.o:smoke done")
