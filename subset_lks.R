#!/bin/Rscript
library(terra)
library(sf)
#library(rgeos)

# this script 
# 1. reads in a lakes polygon shapefile which contains ~48,000 lakes for CONUS (in lat/lon coords)
# 2. subsets some lakes of interest based on a user specified tab seperated file
# 3. transforms the subset lake polygons to the same CRS as HRRR
# 4. writes the subset/transformed lake polygons to a shapefile for use in extraction


plt <- F

conus_shp_fn <- './conus_lakes.shp'
lake_tab_fn <- './lakes_to_extract.txt'
#output_dir <- '/nfs/turbo/seas-drewgron/hrrr/shapefiles/'
#plot_dir <- '/nfs/turbo/seas-drewgron/hrrr/figs/'

grid_file <- './CLM-Lake-offline/grid/wrf_grid.tif'


# read in ~48K lakes (SpatialPolygons) in CONUS
all_lks <- st_read(conus_shp_fn) 

# read in lake table (name, lat, lon) and create from it a spatialpts data frame with each point in lake center
lks <- read.table(lake_tab_fn, head=T)
#lk_pts <- SpatialPointsDataFrame(lks[,c('lon','lat')], data=lks, proj4string=crs(proj4string(all_lks))) 

#stop()
#lk_pts <- st_as_sf(lks[,c('lon','lat')], data=lks, proj4string=crs(proj4string(all_lks))) 
lk_pts <- st_as_sf(lks, coords=c('lon','lat'), crs=st_crs(all_lks))


# now use the lk center points to subset the polygons

#my_lks <- intersect(all_lks, lk_pts) # THIS WOULD BE SO MUCH EASIER, but it doesn't keep the metadata (e.g. lake names coming from lk_pts)
# instead do it in few steps below...

# 1. return a logical array [ length(all_lks) x length(lk_pts) ] representing the intersections of points and lake_polygons
win <- st_intersects(lk_pts, all_lks, sparse=F)

# 2. convert that huge logical array to an array with indices of the "TRUES"
sel <- which(win, arr.ind=T)   

# 3. subset all_lks with the sel variable and assign lake names from lk_pts
my_lks <- all_lks[sel[,2],]
my_lks$NAME <- lk_pts[sel[,1],]$name


if (plt){
	# plot all lakes in conus
	png(sprintf('%s/all_lakes.png',plot_dir),w=1400, h=800)
	plot(all_lks, ax=T)
	map('usa', add=T)

	# plot subset of lakes (for visual confirmation that we got the right lakes and names)
	png(sprintf('%s/subset_lakes.png',plot_dir),w=1400, h=800)
	plot(my_lks, ax=T)
	text(my_lks, lab=my_lks$NAME, pos=1)
	map('usa', add=T)

	graphics.off()
}


# read in a geotif which is properly referenced with HRRR's CRS
grid_lcc <- rast(grid_file)  
my_lks_lcc <- st_transform(my_lks, st_crs(grid_lcc))  # transrom my lks to HRRR's CRS


# write out to shapefile
st_write(obj=my_lks_lcc, dsn='CLM-Lake-offline/grid/my_lks.shp', append=F)


