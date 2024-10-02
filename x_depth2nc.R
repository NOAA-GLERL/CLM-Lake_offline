#!/usr/bin/Rscripts
qlibrary(raster)
qlibrary(rgdal)
qlibrary(ncdf4)
source('utils.R')
#
## =====================================================================================
## ==================     USER  CONTROLS   =============================================
## =====================================================================================
#casename <- commandArgs(trail=T)[1]
var_str <- 'LAKEDEPTH2D'
msk <- T
casename <- 'glo'

fn <- sprintf('grid/%s_depth.nc', casename) # netCDF for all of CONUS
dir_out <- sprintf('extracted_lakes_netCDF/%s', casename)     # where to save individual lakes netCDF
# =====================================================================================
# =====================================================================================
# =====================================================================================

options(warn=-1)
lks <- read_lks()
options(warn=0)

process_file <- function(fn){
		var2D <- rastT(fn, var_str)
        # ================== LAKE LOOP =======================
        for (i in 1:length(lks)){
            lk <- lks[i,]
            cat(sprintf('%s...',lk$NAME))
            lk_var <- intersect(var2D, lk)    
            if (msk) lk_var <- mask(lk_var, lk)
            fout <- sprintf('%s/%s_%s.nc', dir_out, tolower(lk$NAME), var_str)
            writeRaster(t(lk_var), file=fout, varname=var_str, zname='time', overwrite=T)

        }
        cat(sprintf('\n'))
    }

process_file(fn)

