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

print("teardown.r - clearing all the r objects")

if(!is.null(connection.opal))
{
   datashield.logout((connection.opal))
}

rm("connection.opal", envir=ds.test_env)
rm("login.data", envir=ds.test_env)
rm("password", envir=ds.test_env)
rm("server", envir=ds.test_env)
rm("stats.var", envir=ds.test_env)
rm("table", envir=ds.test_env)
rm("url", envir=ds.test_env)
rm("user", envir=ds.test_env)





#if (!is.null(opals)) {
#    datashield.logout(opals)
#    opals <- NULL
#}
