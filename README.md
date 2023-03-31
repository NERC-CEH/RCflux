# RCflux
R package for calculating chamber fluxes

The package can be installed directly from a bundled .tar.gz file.
Alternatively, and to keep versions up to date more easily, the package can be installed directly from GitHub, using `install_github` from devtools.

```r
# The install_github function is in the package devtools;
# you'll need to install this if you don't already have it.
#install.packages("devtools")
library(devtools)

# warnings becomes errors with install_github by default, and may need to change this with:
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")

# need remotes package for function install_github()
install_github("NERC-CEH/RCflux")
library(RCflux)
# Check version installed
packageVersion("RCflux")

```

And if it installs successfully:

```r
# check the help:
?RCflux
?calcFlux
?convert_GC_output

# files on the S: drive should work as a test
getwd()
setwd("S:/DISE_Instrumentation/GC/Agilent/processing")
convert_GC_output("./filelist_aGCxls.txt")
calcFlux("./filelist.txt")

```


# References
Levy, P.E., Gray, A., Leeson, S.R., Gaiawyn, J., Kelly, M.P.C., Cooper, M.D.A., Dinsmore, K.J., Jones, S.K., Sheppard, L.J., 2011. Quantification of uncertainty in trace gas fluxes measured by the static chamber method. European Journal of Soil Science 62, 811–821. https://doi.org/10.1111/j.1365-2389.2011.01403.x

