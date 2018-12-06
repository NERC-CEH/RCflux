rm(list=ls(all=TRUE))
library("devtools")
#install.packages("HMR")
#devtools::install_github("klutometis/roxygen")
library(roxygen2)
#library(HMR)
getwd()
#create("RCflux") # if it doesn't already exist
# Add functions in files to R/ directory 

setwd("../")

check_man()
document()
clean_vignettes()
build_vignettes()

# build the manual
#Sys.getenv(c("R_TEXI2DVICMD", "R_PAPERSIZE", "RD2PDF_INPUTENC"))
#Sys.setenv(RD2PDF_INPUTENC = "inputenx ")
pack <- "RCflux"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))
#C:/PROGRA~1/R/R-32~1.4RE/bin/x64/R R CMD Rd2pdf --no-clean N:/0Peter/prop/UKinverseFlux/GHG_TAP/DelD/anthEmis/ukghg

#check()
build()
#build(manual = TRUE, vignettes = FALSE)
build(binary = TRUE)

setwd("..")
detach("package:RCflux", unload=TRUE)
install("RCflux")
library(RCflux)
vignette("use_RCflux")
?RCflux
?calcFlux
?read_GC_output
?read_GC_input
?convert_GC_output
?excelToCsv

convert_GC_output("S:/DISE_Instrumentation/GC/Agilent/processing/filelist_aGCxls.txt")
convert_GC_output("d:/0Peter/misc/stats/GCflux/filelist_aGCxls.txt")
convert_GC_output("./filelist_aGCxls.txt")
calcFlux("./filelist.txt")
calcFlux("S:/DISE_Instrumentation/GC/Agilent/processing/filelist.txt")
