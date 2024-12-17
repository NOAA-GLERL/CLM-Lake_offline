#!/sw/pkgs/arc/stacks/gcc/10.3.0/R/4.2.0/bin/Rscript
qlibrary(raster)
qlibrary(rgdal)
qlibrary(ncdf4)
source('/nfs/turbo/seas-hydro/um-jtti-ufs/scripts/utils.R')
#
## =====================================================================================
## ==================     USER  CONTROLS   =============================================
## =====================================================================================
casename <- commandArgs(trail=T)[1]
varname <- commandArgs(trail=T)[2]
#t0 <- '20181101'
#tf <- '20190102'
t0 <- '20181101'
tf <- '20200101'
#casename <- 'tl'
#varname <- 'temp'

if (varname=='temp'){
	var_str <- 'LAKE_T3D'
	dig <- 1
	shift <- 273.15
}

if (varname=='ice'){
	var_str <- 'LAKE_ICE3D'
	dig <- 2
	shift <- 0
}

if (varname=='evap'){
	var_str <- 'LAKE_EVAP2D'
	dig <- 2
	shift <- 0
}
#print('USING LATEST RUNS')
#dir_in <- sprintf('/nfs/turbo/seas-drewgron/wrf_hydro/run/OUTPUT2/%s', casename)

print('USING 2022 RUNS')
dir_in <- sprintf('/nfs/turbo/seas-hydro/um-jtti-ufs/wrfout/%s', casename)

#dir_out <- sprintf('lw_out/%s', var_str)
#dir_out <- sprintf('lw_out/%s', var_str)
#if (!dir.exists(dir_out)) dir.create(dir_out)

# =====================================================================================
# =====================================================================================
# =====================================================================================

options(warn=-1)
lks <- read_lks()
options(warn=0)

fout <- sprintf('lw_out/%s_%s.csv', basename(dir_in), gsub('LAKE_','',var_str))
write_file <- function() write.table(file=fout, round(var_out,digits=dig), quote=F, sep=', ')


print('finding files')
#flist <- sort(list.files(path=dir_in, full.names=T, pattern='(2018|2019).*.(01|08|15|22|29)0000.LDASOUT_DOMAIN1',recursive=T)) # weekly starting Nov '18
#flist <- sort(list.files(path=dir_in, full.names=T, pattern='(2018(11|12)|2019).*.0000.LDASOUT_DOMAIN1$',recursive=T)) # daily starting in Nov '18
flist <- sort(list.files(path=dir_in, full.names=T, pattern='(2018(11|12)|2019).*.LDASOUT_DOMAIN1$',recursive=T)) # hourly starting in Nov '18


# subset dates
dts <- as.POSIXct(gsub('.LDASOUT_DOMAIN1','',basename(flist)), format='%Y%m%d%H', tz='z')
#dts <- as.POSIXct(gsub('.LDASOUT_DOMAIN1','',basename(flist)), format='%Y%m%d%H', tz='z')

t0 <- as.POSIXct(t0, format='%Y%m%d', tz='z')
tf <- as.POSIXct(tf, format='%Y%m%d', tz='z')
#dts <- seq.POSIXt(t0, tf, by='hour')
#flist <- paste(dir_in, format(dts, '%Y%m%d%H00.LDASOUT_DOMAIN1'), sep='/')

sel_dts <- dts >= t0  & dts <= tf
dts <- dts[sel_dts]
flist <- flist[sel_dts]
dtstr <- format(dts, '%Y-%m-%d %H:%M')

#var_out <- matrix(NA, length(flist), length(lks))
#colnames(var_out) <- lks$NAME
#rownames(var_out) <- dtstr
print(range(dts))

if (file.exists(fout))var_out <- as.matrix(read.csv(fout, colClasses=c(NA,rep('numeric',23))))
if (!file.exists(fout)){ 
	var_out <- matrix(NA, length(flist), length(lks))
	colnames(var_out) <- lks$NAME
	rownames(var_out) <- dtstr
}


for (i in 1:length(flist)){
	fn <- flist[i]
	print(fn)
	var2D <- rastT(fn, var_str) - shift
	var_out[dtstr[i],] <-  extract(var2D,lks, fun=mean, na.rm=T)
	if (i %% 7 == 0) write_file()
}


write_file()
