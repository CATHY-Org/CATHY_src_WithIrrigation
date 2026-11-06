# make files for Accursio 2009
# first cycle not well done; but second cycle too short given that it was done too late

# seminato il 3/12/2008, con quattro tagli, eseguiti il 18/2/09, 16/3/09, 6/4/09 e 24/4/09
# irrigation by aspersion and drip

# one "prosa" is 0.9 m wide and in between there are 0.45 m
# do one prose in between two interrows
# set the resolution and calculates number of cols needed
# rows 1 m is okay considering the length of tunnel is tens of m

# La fertirrigazione è stata condotta con soluzione nutritiva all'1% mediante 20
# aspersori per settore con portata di 300 L/h (metodo per aspersione) o mediante
# sette manichette per prosa con portata di 1 L/h e passo di 0,1m (metodo a goccia).

# Gli adacquamenti sono stati 24 nel primo ciclo

# Tabella 1. Acqua ed elementi somministrati con la fertirrigazione in due cicli di coltura di rucola da
# taglio (il primo con quattro tagli e il secondo con un taglio).
# Ciclo Tagli Acqua [mm] NH4-N NO3-N H2PO4-P K Ca Mg SO4-S Cl Na [kg/ha]
#     1     4      210,5  32,5 126,8 66,3 268,2 437,5 145,9 180,1 141,6 121,7

# sonda causa ritardi misura da inizi di aprile in poi
# periodo 1/4 - 6/5, in corrispondenza della crescita per il terzo e quarto taglio del primo ciclo di coltura.

# use 1.5 to round
# matlab plot script use same gridsize

setwd("C:/CATHY/cathy1_0/runs/Venezia_2009_ic_ET_soil")
rm(list=ls())

myheader <- as.data.frame(matrix(NA,nrow=6,ncol=1))
myheader[,1] <- c("north:     0",
                  "south:           0.00000000",
                  "east:      0",
                  "west:            0.00000000",
                  "rows:           10",
                  "cols:           13")

dx <- 0.1 # m
dy <- 0.1 # m
myrow <- 10 # go to 1 m
myprose <- 1.0
width_sys <- 0.3 + myprose # m, one prose and two interrows (half)
col_sx <- 1 : (0.3/dx)
col_crop <- max(col_sx) + (1 : (myprose/dx))

mycol <- width_sys / dx

mysoildepth <- 20 # DTM, go big so that WTD can be large too and test "ic" conditions
a <- as.data.frame(matrix(mysoildepth + seq(0.79, 0.8, length.out=length(col_sx)), nrow=1))
a <- a[rep(seq_len(nrow(a)), each = 10), ]
mydtm <- a

a <- as.data.frame(matrix(mysoildepth + seq(0.89, 0.9, length.out=length(col_crop)), nrow=1))
a <- a[rep(seq_len(nrow(a)), each = 10), ]
mydtm <- cbind(mydtm,a)

mydtm[1,1] <- mysoildepth + 0.78 # to find 1 outlet

write.table(mydtm, file="prepro/dtm_13.val", dec=".", sep=" ", col.names = FALSE, quote=FALSE, row.names = FALSE)




#
# myroot <- as.data.frame(matrix(1, nrow=10,ncol=35))

a <- as.data.frame(matrix(rep(1,length(col_sx)), nrow=1))
a <- a[rep(seq_len(nrow(a)), each = 10), ]
myroot <- a

a <- as.data.frame(matrix(rep(2,length(col_crop)), nrow=1))
a <- a[rep(seq_len(nrow(a)), each = 10), ]
myroot <- cbind(myroot,a)

write.table(myheader, file="input/root_map", dec=".", sep=" ", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table(myroot, file="input/root_map", dec=".", sep=" ", append=TRUE, col.names = FALSE, quote=FALSE, row.names = FALSE)








################################################################################
nameout_index_GW_DLC <- paste0("DLC_index_vert_prof_map.asc", sep="")
nameout_index_SW_DLC <- paste0("DLC_index_surf_node_map.asc", sep="")

# myheader_GIS <- read.table(namelanduse,
#                            head = FALSE,
#                            nrows = 6)
myheader_GIS <- as.data.frame(matrix(NA,nrow=6,ncol=1))
myheader_GIS[,1] <- c("ncols     13",
                  "nrows           10",
                  "xllcorner      0",
                  "yllcorner            0",
                  "cellsize           0.1",
                  "NODATA_value           0.0")

# create raster to know cell value to extract vertical profile in parm
myIndex_GW <- as.data.frame(matrix(1:(myrow*mycol), nrow=myrow, ncol=mycol, byrow=TRUE))

# save data GIS
write.table(myheader_GIS, file=nameout_index_GW_DLC, dec=".", sep=" ", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table(myIndex_GW, file=nameout_index_GW_DLC, dec=".", sep=" ", append=TRUE, col.names = FALSE, quote=FALSE, row.names = FALSE)

################################################################################
# create raster to know cell value to extract vertical profile in parm
tmp <- matrix(1:(myrow*mycol), nrow=myrow, ncol=mycol, byrow=FALSE)
tmp <- tmp[myrow:1, , drop = FALSE]
myIndex_SW <- as.data.frame(tmp)

# save data GIS
write.table(myheader_GIS, file=nameout_index_SW_DLC, dec=".", sep=" ", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table(myIndex_SW, file=nameout_index_SW_DLC, dec=".", sep=" ", append=TRUE, col.names = FALSE, quote=FALSE, row.names = FALSE)