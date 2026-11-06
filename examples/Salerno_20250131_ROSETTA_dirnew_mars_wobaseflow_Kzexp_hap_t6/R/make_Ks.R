
Sys.setlocale("LC_TIME", "English")

rm(list=ls());
#.rs.restartR()

library(dplyr)
library(ggplot2)
library(reshape2)
library(raster)
# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3

namefolder<- "C:/UNIPD/CATHY/cathy2_0/REWATERING/Salerno_20250131_ROSETTA_dirnew_mars_wobaseflow_Kzexp_hap_t6/prepro-dem"
setwd(namefolder)

nameKs1 <- "dtm_kSs1_sf_1_90_40"
nameKs2 <- "dtm_kSs1_sf_2_90_40"

namezone <- "zone_map"
nameinflow <- "dtm_A_inflow" # to change KS only above threshold

r_Ks1 <- read.table(nameKs1, header=FALSE, sep="", dec = ".", skip=6)
r_Ks2 <- read.table(nameKs2, header=FALSE, sep="", dec = ".", skip=6)

r_Ks1_0 <- r_Ks1
r_Ks2_0 <- r_Ks2

r_zone <- read.table(namezone, header=FALSE, sep=" ", dec = ".", skip=6)

r_inflow <- read.table(nameinflow, header=FALSE, sep="", dec = ".", skip=6)


###
# where drainage area below threshold (replace channels found with pre-pro)
j <- which(r_inflow < 6.5e6 & r_inflow > 0 & r_Ks1_0 > 0, arr.ind=T)
# a <- NULL
# for (i in 1:nrow(j)) {
#   a[i] <- r_inflow[j[i,1],j[i,2]]
# }
for (i in 1:nrow(j)) {
  r_Ks1[j[i,1],j[i,2]] <- 30
}
j <- which(r_inflow < 6.5e6 & r_inflow > 0 & r_Ks2_0 > 0, arr.ind=T)
for (i in 1:nrow(j)) {
  r_Ks2[j[i,1],j[i,2]] <- 30
}
j <- which(r_inflow >= 6.5e6 & r_Ks1_0 > 0, arr.ind=T)
for (i in 1:nrow(j)) {
  r_Ks1[j[i,1],j[i,2]] <- 40
}
j <- which(r_inflow >= 6.5e6 & r_Ks2_0 > 0, arr.ind=T)
for (i in 1:nrow(j)) {
  r_Ks2[j[i,1],j[i,2]] <- 40
}

# where greenhouses are
j <- which(r_zone == 2 & r_Ks1_0>0, arr.ind=T)

for (i in 1:nrow(j)) {
  r_Ks1[j[i,1],j[i,2]] <- 40
}

# where greenhouses are
j <- which(r_zone == 2 & r_Ks2_0>0, arr.ind=T)

for (i in 1:nrow(j)) {
  r_Ks2[j[i,1],j[i,2]] <- 40
}

# where agriculture is
j <- which(r_zone == 1 & r_Ks1_0>0, arr.ind=T)

for (i in 1:nrow(j)) {
  r_Ks1[j[i,1],j[i,2]] <- 1
}

# where agriculture is
j <- which(r_zone == 1 & r_Ks2_0>0, arr.ind=T)

for (i in 1:nrow(j)) {
  r_Ks2[j[i,1],j[i,2]] <- 1
}

# # where channel from prepro is
# j <- which(r_Ks1_0 == 40, arr.ind=T)
#
# for (i in 1:nrow(j)) {
#   r_Ks1[j[i,1],j[i,2]] <- 40
# }
#
# # where channel from prepro is
# j <- which(r_Ks2_0 == 40, arr.ind=T)
#
# for (i in 1:nrow(j)) {
#   r_Ks2[j[i,1],j[i,2]] <- 40
# }



mycol <- 74
myrow <- 72
tmp <- r_Ks1
names(tmp) <- 1:mycol
tmp <- melt(tmp) # , id.vars=c("subject", "sex")
names(tmp)[which(names(tmp)=="variable")] <- "V2"
tmp$V3 <- rep(1:myrow, by=mycol)
tmp$value[tmp$value == 0] <- NA
ggplot(tmp,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = value))

mycol <- 74
myrow <- 72
tmp <- r_Ks2
names(tmp) <- 1:mycol
tmp <- melt(tmp) # , id.vars=c("subject", "sex")
names(tmp)[which(names(tmp)=="variable")] <- "V2"
tmp$V3 <- rep(1:myrow, by=mycol)
tmp$value[tmp$value == 0] <- NA
ggplot(tmp,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = value))


nameoutKs <- "dtm_kSs1_sf_1_tmp.asc"
write.table("ncols        74", file=nameoutKs, append = FALSE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("nrows        72", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("xllcorner    999082.799200000009", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("yllcorner    4503510.294599999674", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("cellsize     80.000000000000", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("NODATA_value  0", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)

write.table(r_Ks1, file=nameoutKs, append = TRUE, dec=".", sep="   ", col.names = FALSE, quote=FALSE, row.names = FALSE)




nameoutKs <- "dtm_kSs1_sf_2_tmp.asc"
write.table("ncols        74", file=nameoutKs, append = FALSE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("nrows        72", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("xllcorner    999082.799200000009", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("yllcorner    4503510.294599999674", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("cellsize     80.000000000000", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("NODATA_value  0", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)

write.table(r_Ks2, file=nameoutKs, append = TRUE, dec=".", sep="   ", col.names = FALSE, quote=FALSE, row.names = FALSE)






nameoutKs <- "dtm_kSs1_sf_1"
write.table("north:     0", file=nameoutKs, append = FALSE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("south:           0.00000000", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("east:      0", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("west:            0.00000000", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("rows:     72", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("cols:     74", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)

write.table(r_Ks1, file=nameoutKs, append = TRUE, dec=".", sep="   ", col.names = FALSE, quote=FALSE, row.names = FALSE)




nameoutKs <- "dtm_kSs1_sf_2"
write.table("north:     0", file=nameoutKs, append = FALSE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("south:           0.00000000", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("east:      0", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("west:            0.00000000", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("rows:     72", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)
write.table("cols:     74", file=nameoutKs, append = TRUE, dec=".", sep="", col.names = FALSE, quote=FALSE, row.names = FALSE)

write.table(r_Ks2, file=nameoutKs, append = TRUE, dec=".", sep="   ", col.names = FALSE, quote=FALSE, row.names = FALSE)


