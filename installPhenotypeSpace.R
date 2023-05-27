install.packages("C:/Users/Shelby Palmer/Desktop/CHICKADEES/spatstat.core_2.4-4.tar", repos=NULL, type="source")
library(spatstat.core)
remotes::install_github("maRce10/PhenotypeSpace")
library(PhenotypeSpace)

# R version 4.3.0 (2023-04-21) -- "Already Tomorrow"
# Copyright (C) 2023 The R Foundation for Statistical Computing
# Platform: aarch64-apple-darwin20 (64-bit)
# 
# R is free software and comes with ABSOLUTELY NO WARRANTY.
# You are welcome to redistribute it under certain conditions.
# Type 'license()' or 'licence()' for distribution details.
# 
# Natural language support but running in an English locale
# 
# R is a collaborative project with many contributors.
# Type 'contributors()' for more information and
# 'citation()' on how to cite R or R packages in publications.
# 
# Type 'demo()' for some demos, 'help()' for on-line help, or
# 'help.start()' for an HTML browser interface to help.
# Type 'q()' to quit R.
# 
# Warning: namespace ‘rstan’ is not available and has been replaced
# by .GlobalEnv when processing object ‘.__C__Rcpp_stan_fit4model245666fce58_9a261a94d7f6336176962582e89337ce’
# Warning: namespace ‘lubridate’ is not available and has been replaced
# by .GlobalEnv when processing object ‘.__C__Rcpp_stan_fit4model245666fce58_9a261a94d7f6336176962582e89337ce’
# Warning: namespace ‘brms’ is not available and has been replaced
# by .GlobalEnv when processing object ‘beta_binomial2_lpmf’
# [Workspace loaded from ~/.RData]
# 
# > source("/Users/mcentee_lab_2/Downloads/spatstat.core/R/density.ppp.R")
# > remotes::install_github("maRce10/PhenotypeSpace")
# Downloading GitHub repo maRce10/PhenotypeSpace@HEAD
# These packages have more recent versions available.
# It is recommended to update all of them.
# Which would you like to update?
#   
#   1: All                                
# 2: CRAN packages only                 
# 3: None                               
# 4: rlang       (1.1.0 -> 1.1.1) [CRAN]
# 5: viridisLite (0.4.1 -> 0.4.2) [CRAN]
# 
# Enter one or more numbers, or an empty line to skip updates: 3
# Installing 22 packages: spatstat.utils, RcppArmadillo, pixmap, CircStats, adehabitatMA, ade4, sp, polyclip, deldir, spatstat.data, permute, terra, adehabitatLT, gridExtra, rgeos, proxy, spatstat.geom, vegan, raster, adehabitatHR, viridis, pbapply
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/spatstat.utils_3.0-3.tgz'
# Content type 'application/x-gzip' length 383529 bytes (374 KB)
# ==================================================
#   downloaded 374 KB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/RcppArmadillo_0.12.2.0.0.tgz'
# Content type 'application/x-gzip' length 1654778 bytes (1.6 MB)
# ==================================================
#   downloaded 1.6 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/pixmap_0.4-12.tgz'
# Content type 'application/x-gzip' length 207414 bytes (202 KB)
# ==================================================
#   downloaded 202 KB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/CircStats_0.2-6.tgz'
# Content type 'application/x-gzip' length 175620 bytes (171 KB)
# ==================================================
#   downloaded 171 KB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/adehabitatMA_0.3.16.tgz'
# Content type 'application/x-gzip' length 839679 bytes (819 KB)
# ==================================================
#   downloaded 819 KB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/ade4_1.7-22.tgz'
# Content type 'application/x-gzip' length 6065137 bytes (5.8 MB)
# ==================================================
#   downloaded 5.8 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/sp_1.6-0.tgz'
# Content type 'application/x-gzip' length 1877537 bytes (1.8 MB)
# ==================================================
#   downloaded 1.8 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/polyclip_1.10-4.tgz'
# Content type 'application/x-gzip' length 456174 bytes (445 KB)
# ==================================================
#   downloaded 445 KB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/deldir_1.0-6.tgz'
# Content type 'application/x-gzip' length 308578 bytes (301 KB)
# ==================================================
#   downloaded 301 KB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/spatstat.data_3.0-1.tgz'
# Content type 'application/x-gzip' length 4103251 bytes (3.9 MB)
# ==================================================
#   downloaded 3.9 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/permute_0.9-7.tgz'
# Content type 'application/x-gzip' length 217812 bytes (212 KB)
# ==================================================
#   downloaded 212 KB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/terra_1.7-29.tgz'
# Content type 'application/x-gzip' length 100672746 bytes (96.0 MB)
# ==================================================
#   downloaded 96.0 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/adehabitatLT_0.3.27.tgz'
# Content type 'application/x-gzip' length 2327005 bytes (2.2 MB)
# ==================================================
#   downloaded 2.2 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/gridExtra_2.3.tgz'
# Content type 'application/x-gzip' length 1105778 bytes (1.1 MB)
# ==================================================
#   downloaded 1.1 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/rgeos_0.6-2.tgz'
# Content type 'application/x-gzip' length 1616804 bytes (1.5 MB)
# ==================================================
#   downloaded 1.5 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/proxy_0.4-27.tgz'
# Content type 'application/x-gzip' length 196057 bytes (191 KB)
# ==================================================
#   downloaded 191 KB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/spatstat.geom_3.2-1.tgz'
# Content type 'application/x-gzip' length 4113700 bytes (3.9 MB)
# ==================================================
#   downloaded 3.9 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/vegan_2.6-4.tgz'
# Content type 'application/x-gzip' length 3165118 bytes (3.0 MB)
# ==================================================
#   downloaded 3.0 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/raster_3.6-20.tgz'
# Content type 'application/x-gzip' length 4936052 bytes (4.7 MB)
# ==================================================
#   downloaded 4.7 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/adehabitatHR_0.4.21.tgz'
# Content type 'application/x-gzip' length 1410027 bytes (1.3 MB)
# ==================================================
#   downloaded 1.3 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/viridis_0.6.3.tgz'
# Content type 'application/x-gzip' length 3018268 bytes (2.9 MB)
# ==================================================
#   downloaded 2.9 MB
# 
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/pbapply_1.7-0.tgz'
# Content type 'application/x-gzip' length 98936 bytes (96 KB)
# ==================================================
#   downloaded 96 KB
# 
# 
# The downloaded binary packages are in
# /var/folders/4r/rnzjnh054l34lqbwpx4t_8k00000gn/T//RtmpOOEyqp/downloaded_packages
# ── R CMD build ─────────────────────────────────────────────────────
# ✔  checking for file ‘/private/var/folders/4r/rnzjnh054l34lqbwpx4t_8k00000gn/T/RtmpOOEyqp/remotes338241561401/maRce10-PhenotypeSpace-e505ef5/DESCRIPTION’ ...
# ─  preparing ‘PhenotypeSpace’:
#   ✔  checking DESCRIPTION meta-information ...
# ─  checking for LF line-endings in source and make files and shell scripts
# ─  checking for empty or unneeded directories
# ─  looking to see if a ‘data/datalist’ file should be added
# ─  building ‘PhenotypeSpace_0.1.0.tar.gz’
# 
# ERROR: dependency ‘spatstat.core’ is not available for package ‘PhenotypeSpace’
# * removing ‘/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library/PhenotypeSpace’
# Warning message:
#   In i.p(...) :
#   installation of package ‘/var/folders/4r/rnzjnh054l34lqbwpx4t_8k00000gn/T//RtmpOOEyqp/file338219b85f21/PhenotypeSpace_0.1.0.tar.gz’ had non-zero exit status
# > install.packages('/Users/mcentee_lab_2/Downloads/spatstat.core_2.4-4.tar/', repos=NULL, type='source')
# Warning: invalid package ‘/Users/mcentee_lab_2/Downloads/spatstat.core_2.4-4.tar/’
# Error: ERROR: no packages specified
# Warning in install.packages :
#   installation of package ‘/Users/mcentee_lab_2/Downloads/spatstat.core_2.4-4.tar/’ had non-zero exit status
# > install.packages('/Users/mcentee_lab_2/Downloads/spatstat.core_2.4-4.tar', repos=NULL, type='source')
# ERROR: dependencies ‘spatstat.random’, ‘spatstat.sparse’, ‘abind’, ‘tensor’, ‘goftest’ are not available for package ‘spatstat.core’
# * removing ‘/Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library/spatstat.core’
# Warning in install.packages :
#   installation of package ‘/Users/mcentee_lab_2/Downloads/spatstat.core_2.4-4.tar’ had non-zero exit status
# > install.packages(c(‘spatstat.random’, ‘spatstat.sparse’, ‘abind’, ‘tensor’, ‘goftest’))
# Error: unexpected input in "install.packages(c(‘"
# > install.packages(‘spatstat.random’, ‘spatstat.sparse’, ‘abind’, ‘tensor’, ‘goftest’)
# Error: unexpected input in "install.packages(‘"
# > install.packages(‘spatstat.random’)
# Error: unexpected input in "install.packages(‘"
# > install.packages("spatstat.random")
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/spatstat.random_3.1-5.tgz'
# Content type 'application/x-gzip' length 1226334 bytes (1.2 MB)
# ==================================================
#   downloaded 1.2 MB
# 
# 
# The downloaded binary packages are in
# /var/folders/4r/rnzjnh054l34lqbwpx4t_8k00000gn/T//RtmpOOEyqp/downloaded_packages
# > install.packages("spatstat.sparse", "abind", "tensor", "goftest")
# Warning in install.packages :
#   'lib = "abind"' is not writable
# Would you like to use a personal library instead? (yes/No/cancel) cancel
# Error in install.packages : unable to install packages
# > install.packages("abind")
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/abind_1.4-5.tgz'
# Content type 'application/x-gzip' length 62100 bytes (60 KB)
# ==================================================
#   downloaded 60 KB
# 
# 
# The downloaded binary packages are in
# /var/folders/4r/rnzjnh054l34lqbwpx4t_8k00000gn/T//RtmpOOEyqp/downloaded_packages
# > install.packages("tensor")
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/tensor_1.5.tgz'
# Content type 'application/x-gzip' length 13799 bytes (13 KB)
# ==================================================
#   downloaded 13 KB
# 
# 
# The downloaded binary packages are in
# /var/folders/4r/rnzjnh054l34lqbwpx4t_8k00000gn/T//RtmpOOEyqp/downloaded_packages
# > install.packages("goftest")
# trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/goftest_1.2-3.tgz'
# Content type 'application/x-gzip' length 64509 bytes (62 KB)
# ==================================================
#   downloaded 62 KB
# 
# 
# The downloaded binary packages are in
# /var/folders/4r/rnzjnh054l34lqbwpx4t_8k00000gn/T//RtmpOOEyqp/downloaded_packages
# > require(abind, goftest)
# Loading required package: abind
# Failed with error:  ‘object 'goftest' not found’
# > library(goftest)
# > require(c(tensor, spatstat.sparse))
# Error in if (!loaded) { : the condition has length > 1
#   > require(tensor)
#   Loading required package: tensor
#   > require(spatstat.sparse)
#   Loading required package: spatstat.sparse
#   Warning message:
#     In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
#                  there is no package called ‘spatstat.sparse’
#                > install.packages("spatstat.sparse")
#                trying URL 'https://cran.rstudio.com/bin/macosx/big-sur-arm64/contrib/4.3/spatstat.sparse_3.0-1.tgz'
#                Content type 'application/x-gzip' length 222294 bytes (217 KB)
#                ==================================================
#                  downloaded 217 KB
#                
#                
#                The downloaded binary packages are in
#                /var/folders/4r/rnzjnh054l34lqbwpx4t_8k00000gn/T//RtmpOOEyqp/downloaded_packages
#                > require(spatstat.sparse)
#                Loading required package: spatstat.sparse
#                Loading required package: Matrix
#                Loading required package: abind
#                > require(spatstat.random)
#                Loading required package: spatstat.random
#                Loading required package: spatstat.data
#                Loading required package: spatstat.geom
#                spatstat.geom 3.2-1
#                spatstat.random 3.1-5
#                > require(spatstat.core)
#                Loading required package: spatstat.core
#                Warning message:
#                  In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
#                               there is no package called ‘spatstat.core’
#                             > install.packages("spatstat.core")
#                             Warning in install.packages :
#                               package ‘spatstat.core’ is not available for this version of R
#                             
#                             A version of this package for your version of R might be available elsewhere,
#                             see the ideas at
#                             https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
#                             > install.packages('/Users/mcentee_lab_2/Downloads/spatstat.core_2.4-4.tar', repos=NULL, type='source')
#                             * installing *source* package ‘spatstat.core’ ...
#                             ** package ‘spatstat.core’ successfully unpacked and MD5 sums checked
#                             ** using staged installation
#                             ** libs
#                             using C compiler: ‘Apple clang version 12.0.5 (clang-1205.0.22.11)’
#                             using SDK: ‘MacOSX11.3.sdk’
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c Ediggatsti.c -o Ediggatsti.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c Ediggra.c -o Ediggra.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c Efiksel.c -o Efiksel.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c Egeyer.c -o Egeyer.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c Estrauss.c -o Estrauss.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c Kborder.c -o Kborder.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c Knone.c -o Knone.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c Krect.c -o Krect.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c areapair.c -o areapair.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c call3d.c -o call3d.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c corrections.c -o corrections.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c denspt.c -o denspt.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c densptcross.c -o densptcross.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c digber.c -o digber.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c f3.c -o f3.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c g3.c -o g3.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c idw.c -o idw.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c init.c -o init.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c k3.c -o k3.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c localpcf.c -o localpcf.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c loccum.c -o loccum.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c pcf3.c -o pcf3.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c raster.c -o raster.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c scan.c -o scan.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c segdens.c -o segdens.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c sphefrac.c -o sphefrac.o
#                             clang -arch arm64 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG   -I/opt/R/arm64/include    -fPIC  -falign-functions=64 -Wall -g -O2  -c sphevol.c -o sphevol.o
#                             clang -arch arm64 -dynamiclib -Wl,-headerpad_max_install_names -undefined dynamic_lookup -single_module -multiply_defined suppress -L/Library/Frameworks/R.framework/Resources/lib -L/opt/R/arm64/lib -o spatstat.core.so Ediggatsti.o Ediggra.o Efiksel.o Egeyer.o Estrauss.o Kborder.o Knone.o Krect.o areapair.o call3d.o corrections.o denspt.o densptcross.o digber.o f3.o g3.o idw.o init.o k3.o localpcf.o loccum.o pcf3.o raster.o scan.o segdens.o sphefrac.o sphevol.o -F/Library/Frameworks/R.framework/.. -framework R -Wl,-framework -Wl,CoreFoundation
#                             installing to /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/library/00LOCK-spatstat.core/00new/spatstat.core/libs
#                             ** R
#                             ** inst
#                             ** byte-compile and prepare package for lazy loading
#                             ** help
#                             *** installing help indices
#                             ** building package indices
#                             ** testing if installed package can be loaded from temporary location
#                             ** checking absolute paths in shared objects and dynamic libraries
#                             ** testing if installed package can be loaded from final location
#                             ** testing if installed package keeps a record of temporary installation path
#                             * DONE (spatstat.core)
#                             > library(spatstat.core)
#                             Loading required package: nlme
#                             Loading required package: rpart
#                             spatstat.core 2.4-4
#                             
#                             Attaching package: ‘spatstat.core’
#                             
#                             The following objects are masked _by_ ‘.GlobalEnv’:
#                               
#                               bandwidth.is.infinite, density.ppp, density.ppplist, density.splitppp,
#                             densitycrossEngine, densitypointsEngine, resolve.2D.kernel
#                             
#                             > remotes::install_github("maRce10/PhenotypeSpace")
#                             Downloading GitHub repo maRce10/PhenotypeSpace@HEAD
#                             These packages have more recent versions available.
#                             It is recommended to update all of them.
#                             Which would you like to update?
#                               
#                               1: All                                
#                             2: CRAN packages only                 
#                             3: None                               
#                             4: rlang       (1.1.0 -> 1.1.1) [CRAN]
#                             5: viridisLite (0.4.1 -> 0.4.2) [CRAN]
#                             6: deldir      (1.0-6 -> 1.0-9) [CRAN]
#                             
#                             Enter one or more numbers, or an empty line to skip updates: 3
#                             ── R CMD build ────────────────────────────────────────────────────────────────────────────────
#                             ✔  checking for file ‘/private/var/folders/4r/rnzjnh054l34lqbwpx4t_8k00000gn/T/RtmpOOEyqp/remotes338236e79f84/maRce10-PhenotypeSpace-e505ef5/DESCRIPTION’ ...
#                             ─  preparing ‘PhenotypeSpace’:
#                               ✔  checking DESCRIPTION meta-information ...
#                             ─  checking for LF line-endings in source and make files and shell scripts
#                             ─  checking for empty or unneeded directories
#                             ─  looking to see if a ‘data/datalist’ file should be added
#                             ─  building ‘PhenotypeSpace_0.1.0.tar.gz’
#                             
#                             * installing *source* package ‘PhenotypeSpace’ ...
#                             ** using staged installation
#                             ** R
#                             ** data
#                             ** inst
#                             ** byte-compile and prepare package for lazy loading
#                             Warning: replacing previous import ‘proxy::as.dist’ by ‘stats::as.dist’ when loading ‘PhenotypeSpace’
#                             Warning: replacing previous import ‘proxy::dist’ by ‘stats::dist’ when loading ‘PhenotypeSpace’
#                             ** help
#                             *** installing help indices
#                             *** copying figures
#                             ** building package indices
#                             ** testing if installed package can be loaded from temporary location
#                             Warning: replacing previous import ‘proxy::as.dist’ by ‘stats::as.dist’ when loading ‘PhenotypeSpace’
#                             Warning: replacing previous import ‘proxy::dist’ by ‘stats::dist’ when loading ‘PhenotypeSpace’
#                             ** testing if installed package can be loaded from final location
#                             Warning: replacing previous import ‘proxy::as.dist’ by ‘stats::as.dist’ when loading ‘PhenotypeSpace’
#                             Warning: replacing previous import ‘proxy::dist’ by ‘stats::dist’ when loading ‘PhenotypeSpace’
#                             ** testing if installed package keeps a record of temporary installation path
#                             * DONE (PhenotypeSpace)
#                             > library(PhenotypeSpace)
#                             Warning messages:
#                               1: replacing previous import ‘proxy::as.dist’ by ‘stats::as.dist’ when loading ‘PhenotypeSpace’ 
#                             2: replacing previous import ‘proxy::dist’ by ‘stats::dist’ when loading ‘PhenotypeSpace’