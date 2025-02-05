#find path to this file
cmdArgs = commandArgs(trailingOnly = FALSE)
needle = "--file="
match = grep(needle, cmdArgs)
res<-normalizePath(sub(needle, "", cmdArgs[match]))

libPATH<-gsub("bin\\\\x64\\\\Rscript.exe","library",paste(Sys.which("Rscript.exe")))
.libPaths(c(libPATH,.libPaths()))

# check if devtools is installed
if (!"devtools" %in% rownames(installed.packages())){
  install.packages("devtools")
}

# load condenserDuty path
devtools::install_deps(dirname(res), upgrade = "never")
#devtools::load_all(dirname(res), recompile = FALSE)