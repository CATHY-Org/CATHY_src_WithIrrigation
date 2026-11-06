# this is a test whether neumann reproduce amtbc... se if this approach can be followed in catchment scale CATHY

# .rs.restartR()
rm(list=ls())
namefolder<- "Salerno_20250131_ROSETTA_dirnew_mars_wobaseflow_Kzexp_hap_t6"
setwd(paste0("C:/UNIPD/CATHY/cathy2_0/REWATERING/", namefolder, sep=""))

# neumann is m3/s not m/s like atmbc. thus need area of the flux
# change NQMAX with number nodes where you apply Neumann BCs in CATHY.H and recompile

# example
# 0.00000  TIME
# 0 1      ZERO NQ (Important, the first value must be 0!)
# 6309
# 1e-5
# 1200  TIME
# 0  1      ZERO NQ (Important, the first value must be 0!)
# 6309
# 0

library(dplyr)
library(ggplot2)
library(reshape2)
library(raster)
# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3

#
myrow <- 72
mycol <- 74
dx <- 80
dy <- 80


dt_weather <- 60*60 # going to 1 hour

# is myatmbc in make_atmbc*
myAtmbc <- read.csv("R/Table_NEUMANN.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

#
namelanduse <- paste0("other-dem/veg-type_map", sep="") # postprocessed to match DTM active cells
nameinflow <- paste0("prepro-dem/dtm_A_inflow", sep="") # to deliver rain to closest draining channel with larger area
namechannel <- paste0("prepro-dem/dtm_hcID", sep="") # to deliver rain to closest draining channel with larger area
namexyz <- paste0("output/xyz", sep="")

r_landuse <- read.table(namelanduse, header=FALSE, sep=" ", dec = ".", skip=6)
if (is.na(r_landuse[1,1])) {r_landuse <- r_landuse[,-1]}
if (nrow(r_landuse) != myrow | (ncol(r_landuse) != mycol)) {stop("row or col do not match")}

r_inflow <- read.table(nameinflow, header=FALSE, sep="", dec = ".", skip=6)
if (is.na(r_inflow[1,1])) {r_inflow <- r_inflow[,-1]}
if (nrow(r_inflow) != myrow | (ncol(r_inflow) != mycol)) {stop("row or col do not match")}

r_channel <- read.table(namechannel, header=FALSE, sep="", dec = ".", skip=6)
if (is.na(r_channel[1,1])) {r_channel <- r_channel[,-1]}
if (nrow(r_channel) != myrow | (ncol(r_channel) != mycol)) {stop("row or col do not match")}

xyz_header <- read.table(namexyz,
                         head = FALSE,
                         nrows = 1)
if (!exists("xyz_header")) { stop("create file xyz running CATHY with input/parm/IPRT1=3") }
NNOD <- xyz_header[1,1]
N <- xyz_header[1,2]
# subirr at 30 cm depth
Nlayer <- 3 # 1 for atmbc, 2... for subsurface. plastic says that Nlayer=2 has Kz=0

xyz <- read.table(namexyz, header=FALSE, sep="", dec = ".", skip=1) # do not specify delimiter to treat multiple space as delimiter
xyz_p <- xyz[(NNOD*(Nlayer-1)+1) : (NNOD*Nlayer), ]
if (nrow(xyz_p) != NNOD) {stop("nrow do not match")}
xyz_p$V2 <- (xyz_p$V2 + 0) /dx
xyz_p$V3 <- (xyz_p$V3 + 0) /dy

# area nodes, var ARENOD
r_arenod <- read.csv("fort.111", header=FALSE, sep="", dec = ".", skip=0) # do not specify delimiter to treat multiple space as delimiter
if (ncol(r_arenod) != NNOD) {stop("nrow do not match")}










# assume irrigation greenhouse given in 3 hours to avoid shocks
myAtmbc$Irrgreen_m3s <- myAtmbc$Irrgreen_m3s / 3
#



# myAtmbc$ET0 <- runif(nrow(myAtmbc)) ##############################################################
# calendar Kc
# do the cuts decrease ET, or then soil transpiration predominate?
# jtime <- c("14.10.2010 00:00", "21.10.2010 00:00",#after seeding
#            "21.10.2010 00:15", "28.10.2010 00:00",#growing
#            "28.10.2010 00:15", "14.12.2010 23:45"# grown
# )

myAtmbc$DOY <- as.numeric(strftime(
  myAtmbc$Date_pos,
  format = "%j", tz="UTC")) # doy


jSolar <- which(myAtmbc$DOY >=151 & myAtmbc$DOY <= 211) # june july solarization
jIrrg <- which(myAtmbc$Irrgreen_m3s>0)
# # assume irrigation greenhouse given in 3 hours to avoid shocks
# jIrrg <- sort(c(jIrrg,jIrrg+1,jIrrg+2))
#
jtime <- myAtmbc$Date[jIrrg]
# jirr <- c(0, 0,
#           0, 1,
#           1, 1
# )

# myk <- rep(0, nrow(myAtmbc))
# for (i in seq(1, length(jtime), 2)) {
#   j1 <- which(myAtmbc$Date == jtime[i])
#   j2 <- which(myAtmbc$Date == jtime[i+1])
#   myk[j1:j2] <- seq(jirr[i], jirr[i+1], length.out=j2-j1+1)
# }
# myAtmbc$ET0 <- myAtmbc$ET0 * myk
# rm(myk)
# assuming same kc everywhere
myk <- rep(1, length(jIrrg))
print("accounting for solarization")
j <- which(jIrrg %in% jSolar)
if (length(j)>0) {
  myk[j] <- 0
}
myAtmbc$Irrgreen_m3s[jIrrg] <- myAtmbc$Irrgreen_m3s[jIrrg] * myk

jirr <- myAtmbc$Irrgreen_m3s[jIrrg]

sum(jirr) # water in with irrigation

print(paste0("sum of ET water without solarization is: ", sum(myAtmbc$ET0green_m3s), " m3s", sep=""))
print(paste0("sum of irrigation water is: ", sum(myAtmbc$Irrgreen_m3s), " m3s", sep=""))

# boundary conditions do not need to have a flux every hour but only when need to start and end.
# so skip first 0.
# then find fluxes =! 0 and put a 0 when they end

# assume irrigation greenhouse given in 3 hours to avoid shocks - use jkeep instead of jdel
jdel <- NULL
zerobefore <- 1
pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1)
for (ii in 2:(nrow(myAtmbc)-1)) {
  setTxtProgressBar(pb,ii)
  if (myAtmbc$Irrgreen_m3s[ii] == 0) {
    if (zerobefore == 1) {
      jdel <- c(jdel , ii)
    } else {
      zerobefore <- 1
    }
  } else {
    zerobefore <- 0
  }
}
close(pb)
# myAtmbc <- myAtmbc[-jdel,]
# rm(jdel)
jkeep <- setdiff(2:nrow(myAtmbc) , jdel)
jkeep[seq(2,length(jkeep),by=2)] <- jkeep[seq(2,length(jkeep),by=2)] + 2
jkeep <- c(1,jkeep)
myAtmbc <- myAtmbc[jkeep,]
rm(jdel,jkeep)




# must be the same as for atmbc
addndays <- 0 * 86400 # in seconds
nrepwarmup <- 1 # how many times to repeat the warm up?
stepndays <- addndays * nrepwarmup / dt_weather

# for subsurface use only selected nodes
r_landuse_copy <- r_landuse # as.data.frame(matrix(0, nrow=nrow(r_landuse), ncol=ncol(r_landuse)))
r_landuse <- r_landuse[myrow:1,] # subsurface grid is coherent with xyz, not need for flipping

tmp <- r_landuse

cols <- c("1" = "red", "2" = "yellow", "3" = "orange", "4" = "darkgreen", "5" = "pink", "6" = "brown") # 0 as na.value
names(tmp) <- 1:mycol
tmp <- melt(tmp) # , id.vars=c("subject", "sex")
names(tmp)[which(names(tmp)=="variable")] <- "V2"
tmp$V3 <- rep(1:myrow, by=mycol)

ggplot(tmp,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = as.factor(value))) +
  theme_bw() +
  scale_fill_manual(values = cols, na.value=0) +
  xlab("Easting") +
  ylab("Northing") +
  labs(fill = "Land cover")
# ggsave("Land_cover.jpg", width = 12, height = 8, units = 'in', dpi=300)

ggplot(xyz_p,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = V4))
# ggsave(paste0("scheme_elevation.jpg", sep=""), width = 12, height = 8, units = 'in', dpi=300)

# it is consistent with xyz_p
tmp <- as.data.frame(t(r_arenod))
names(tmp) <- "V4"
tmp$V2 <- xyz_p$V2
tmp$V3 <- xyz_p$V3
ggplot(tmp,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = V4))
# ggsave(paste0("scheme_arenod.jpg", sep=""), width = 12, height = 8, units = 'in', dpi=300)
sum(tmp$V4) # area one layer

# ncol is v2
xyz_p_a <- xyz_p
xyz_p_a$a <- NA
for (i in 1:nrow(xyz_p)) {
  nowcol <- xyz_p$V2[i]
  nowrow <- xyz_p$V3[i]

  if (nowrow == 0 & nowcol == 0) {
    a <- c(
      r_landuse[nowrow+1,nowcol+1]
    )
  }
  if (nowrow == 0 & nowcol > 0 & nowcol < mycol ) {
    a <- c(
      r_landuse[nowrow+1,nowcol],
      r_landuse[nowrow+1,nowcol+1]
    )
  }
  if (nowrow == 0 & nowcol == mycol) {
    a <- c(
      r_landuse[nowrow+1,nowcol]
    )
  }

  if (nowrow == myrow & nowcol == 0) {
    a <- c(
      r_landuse[nowrow,nowcol+1]
    )
  }
  if (nowrow == myrow & nowcol > 0 & nowcol < mycol ) {
    a <- c(
      r_landuse[nowrow,nowcol],
      r_landuse[nowrow,nowcol+1]
    )
  }
  if (nowrow == myrow & nowcol == mycol) {
    a <- c(
      r_landuse[nowrow,nowcol]
    )
  }

  if (nowcol == 0 & nowrow > 0 & nowrow < myrow ) {
    a <- c(
      r_landuse[nowrow,nowcol+1],
      r_landuse[nowrow+1,nowcol+1]
    )
  }
  if (nowcol == mycol & nowrow > 0 & nowrow < myrow ) {
    a <- c(
      r_landuse[nowrow,nowcol],
      r_landuse[nowrow+1,nowcol]
    )
  }

  if (nowrow > 0 & nowrow < myrow & nowcol > 0 & nowcol < mycol) {
    a <- c(
      r_landuse[nowrow,nowcol],
      r_landuse[nowrow+1,nowcol],
      r_landuse[nowrow,nowcol+1],
      r_landuse[nowrow+1,nowcol+1]
    )
  }
  a <- sum(a==1, na.rm=T) / 4
  xyz_p_a$a[i] <- a
}
ggplot(xyz_p_a,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = a))

jj <- which(xyz_p_a$a > 0)
myNodeID_atmbc <- xyz_p_a$V1[jj]
myNodeID_atmbc <- as.data.frame(matrix(myNodeID_atmbc, nrow=1))

print("neumann condition require m/s to be multiplied by area pertaining to the node")
r_arenod_copy <- r_arenod
r_arenod <- r_arenod[1,jj]
xyz_p_a$a_area <- NA
# xyz_p_a$a_area[jj] <- as.numeric(xyz_p_a$a[jj] * r_arenod)
xyz_p_a$a_area[jj] <- as.numeric(xyz_p_a$a[jj] * 6400)
ggplot(xyz_p_a,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = a_area))
sum(xyz_p_a$a_area[jj])
sum(r_landuse==1) *dx*dy



xyz_p_copy <- xyz_p
xyz_p_plot <- xyz_p # for plotting
xyz_p <- xyz_p[jj,]
# print(paste0("NQMAX is: ", nrow(xyz_p), sep=""))
xyz_p_a_copy <- xyz_p_a
xyz_p_a_plot <- xyz_p_a # for plotting
xyz_p_a <- xyz_p_a[jj,]
# print(paste0("NQMAX is: ", nrow(xyz_p), sep=""))

iprint <- c(2, 4) # ET0-Irr and Irr
namelabel <- c("ET0_Irr","Irr")
# iprint <- 1 + (1:length(namelabel))
contp <- 1

myNEU <- data.frame(t(xyz_p_a$a_area))

# myAtmbc$Irr_m3s <- myAtmbc$Irr_m3s * 10 # increase magnitude to see if it works

# signif(myAtmbc$Irrgreen_m3s[2],digits=3)

sumwater_m2 <- rep(0, nrow(myAtmbc)) # CATHY wants m/2 and then uses area to node for having m3/s
# data series
# myAtmbc$Irrgreen_m3s[2]
# as.numeric(formatC(myAtmbc$Irrgreen_m3s[2], format = "e", digits = 2))
myAtmbc$Irrgreen_m3s <- as.numeric(formatC(myAtmbc$Irrgreen_m3s, format = "e", digits = 2))
pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1)
for (ii in 1:nrow(myAtmbc)) {
  setTxtProgressBar(pb,ii)

  myNode_atmbc <- myAtmbc$Irrgreen_m3s[ii] * myNEU * 1.0

  sumwater_m2[ii] <- sum(myNode_atmbc, na.rm=TRUE)

  if (ii == 1) {
    write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos), # write tieme 0
                paste0("input/nansfneubc", sep=""),
                append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  } else {
    write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays*nrepwarmup,
                paste0("input/nansfneubc", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }

  write.table(paste0("0 ", length(myNode_atmbc), sep=""),
              paste0("input/nansfneubc", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

  write.table(myNodeID_atmbc,
              paste0("input/nansfneubc", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

  write.table(myNode_atmbc,
              paste0("input/nansfneubc", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
}
close(pb)
r_landuse <- r_landuse_copy
rm(r_landuse_copy)

# sumwater_m2 <- sumwater_m2 * 1e3 * dt_weather # from m/s to mm/dt
# sum_myAtmbc <- myAtmbc$Irrgreen_m3s * sum(r_arenod[jj])
sumwater_plot <- sumwater_m2 * dt_weather # from m/s to m/dt
sum_myAtmbc <- myAtmbc$Irrgreen_m3s * sum(xyz_p_a$a_area) * dt_weather # from m/s to m/dt
plot(sum_myAtmbc , sumwater_plot)
abline(a=0, b=1)
# sum(sum_myAtmbc)
# CATHY CUMULATIVE TOTAL NET FLOW VOLUME (VIN + VOUT)  :  9.00267E-02 - always check


# myNode_atmbc_plot <- as.data.frame(t(myNode_atmbc))
# myNode_atmbc_plot <- cbind(myNode_atmbc_plot , xyz_p)
# names(myNode_atmbc_plot)[1] <- "atmbc"
# ggplot(myNode_atmbc_plot,
#        mapping=aes(V2, V3)) +
#   geom_raster(aes(fill = atmbc))

print(paste0("NQMAX is: ", nrow(xyz_p_a), sep=""))
xyz_p <- xyz_p_copy
rm(xyz_p_copy)

r_arenod <- r_arenod_copy
rm(r_arenod_copy)



dev.off()
#
#
# # save time to print solutions to embed in parm
# if (addndays == 0) {
#   a <- seq(0,max(myAtmbc$Date_pos)-min(myAtmbc$Date_pos), by = 900) # 15 minutes
#   aa <- myAtmbc$Date_pos-min(myAtmbc$Date_pos)
# } else {
#   a <- seq(0,addndays-1, by=86400) # end of array will be in the next array to concatenate
#   a <- c(a , seq(0,max(myAtmbc$Date_pos)-min(myAtmbc$Date_pos), by = 900) + addndays )
#   aa <- addndays + myAtmbc$Date_pos-min(myAtmbc$Date_pos)
#   j <- match(aa, a);
#   a <- as.data.frame(a)
#   a$atmBC <- NA
#   a$atmBC[j] <- myAtmbc$Irr
# }
# j <- head(which(a$atmBC>0),1)
# a <- a[(j-10):nrow(a),]
# write.table(a,
#             paste0("input/parmtime_neun_DLC.txt", sep=""),
#             append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# print(paste0("length a:", nrow(a), sep=""))


max(myAtmbc$Date_pos)-min(myAtmbc$Date_pos)
