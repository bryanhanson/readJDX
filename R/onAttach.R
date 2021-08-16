#
#
.onAttach <- function(libname, pkgname) {
  msg <- "\nreadJDX is tested with > 175 files before release.\nHowever, there are still challenging files out in the wild.\nIf you have trouble importing a file, please file an issue at\ngithub.com/bryanhanson/readJDX/issues\n"
  packageStartupMessage(msg)
}
