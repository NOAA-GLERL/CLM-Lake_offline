#!/bin/Rscript
library(stringr)






# rename and re-order lakes
renamorder <- function(dat){
    dat <- dat[,order(names(dat), decreasing=T)]
    names(dat) <- str_to_title(gsub('_',' ',names(dat)))
    names(dat)[names(dat)=='Mcconaugh'] <- 'McConaughy'
    names(dat) <- gsub(' Lake','', names(dat)
    )
    return(dat)
}


outdir <- '/home/kessler/work/jtti/65e0d491f698c7b0fdfee2b7/figures/'
rng <- list(min=-40, max=50)

meta <- read.table('/home/kessler/work/jtti/analysis/txt/lks_meta.txt', head=T, row.names=1)
row.names(meta) <- gsub('_',' ',row.names(meta))
lks <- meta$name



min_frac <- .3
ctlcol <- 'brown'
glocol <- 'orange'

filter_rng <- function(data, low=rng$min, high=rng$max){
    sellow <- which(data < low, arr.ind=T)
    selhig <- which(data > high, arr.ind=T)
    data[sellow] <- NA
    data[selhig] <- NA
    #if(length(sellow) > 0) cat(sprintf('threw out %i low vals\n', length(sellow)))
    #if(length(selhig) > 0) cat(sprintf('threw out %i high vals\n', length(selhig)))
    return(data)
}

