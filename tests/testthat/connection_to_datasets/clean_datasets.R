
clean.environment <- function()
{
  rm("connection.opal", envir=ds.test_env)
  rm("login.data", envir=ds.test_env)
  rm("password", envir=ds.test_env)
  rm("server", envir=ds.test_env)
  rm("stats.var", envir=ds.test_env)
  rm("table", envir=ds.test_env)
  rm("url", envir=ds.test_env)
  rm("user", envir=ds.test_env)
  rm("local.values.1",  envir=ds.test_env)
  rm("local.values.2",  envir=ds.test_env)
  rm("local.values.3",  envir=ds.test_env)
  rm("local.values",  envir=ds.test_env)
  gc()
}