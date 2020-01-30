rm(list=ls(all=TRUE))
#install.packages("nlme")
library(devtools)
# warnings becomes errors with install_github by default, and may need to change this with:
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
install_github("NERC-CEH/RCflux", auth_token = "cf75f3ae2091f58e6dd664ce9031bee3aa98f0f8")
library(RCflux)
getwd()

# examples:
convert_GC_output("S:/DISE_Instrumentation/GC/Agilent/processing/filelist_aGCxls.txt")
convert_GC_output("S:/DISE_People/Nick_Cowan/CINAG_RFlux/filelist_aGCxls.txt")
convert_GC_output("d:/0Peter/misc/stats/GCflux/filelist_aGCxls.txt")
convert_GC_output("./filelist_aGCxls.txt")
convert_GC_output("C:/Users/plevy/OneDrive - NERC/Stephanie_RCflux/filelist_aGCxls.txt", dataSource = "SRUC_GC")

calcFlux("./filelist.txt")
calcFlux("S:/DISE_Instrumentation/GC/Agilent/processing/filelist.txt")
calcFlux("S:/DISE_People/Nick_Cowan/CINAG_RFlux/filelist.txt")

calcFlux("C:/Users/plevy/OneDrive - NERC/Stephanie_RCflux/filelist.txt")
calcFlux("S:/DISE_Instrumentation/GC/Agilent/processing/filelist.txt")