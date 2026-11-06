
Sys.setlocale("LC_TIME", "English")

rm(list=ls());
#.rs.restartR()

library(dplyr)
library(ggplot2)
library(reshape2)
library(raster)
# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3

namefolder<- "Salerno_20250131_ROSETTA_dirnew_mars_wobaseflow"
# setwd(paste0("C:/CATHY/cathy2_0/REWATERING/", namefolder, sep=""))
setwd(paste0("C:/UNIPD/CATHY/cathy2_0/REWATERING/", namefolder, sep=""))

dobaseflow <- 0 # remove variable so that it is not used, oterwise use a number
DOsetbasefl <- 0
dobaseflowlinear <- 0
DO3zone <- 0

myrow <- 72
mycol <- 74
dx <- 80
dy <- 80

date_st_warmup <- "01.06.2023"
date_en_warmup <- "30.11.2023"
date_st <- "01.12.2023"
date_en <- "31.01.2025"
# date_st_pre <- "01.09.2024"

a <- as.numeric(as.POSIXlt(date_st_warmup, format = "%d.%m.%Y")$yday) # first day of the year is 0, thus I add 1
aa <- as.numeric(as.POSIXlt(date_en_warmup, format = "%d.%m.%Y")$yday) # first day of the year is 0, thus I add 1
warmupdays <- length(seq(a,aa,1))
rm(a,aa)


DOmoverain <- 0 # move rain from greenhouse cells to cells with no greenhouse downstream, simulate fast flow
DOopen <- 0 # this makes a files with catchment all vegetation,
if (DOopen == 1) {
  idveg <- 4
}
# Kc typical of vegetation at italian latitudes
# remember to change file zone, or have same soil parameters for all zones
# you can keep managed catchment or need to change file for prepro to have natural conditions for drainage channel creation and KS

# working at hour at the moment for speed and for ET formula

dt_weather <- 60*60 # going to 1 hour

nameout <- paste0("input/atmbc", sep="") # postprocessed to match DTM active cells
nameoutplot <- paste0("input/waterbalance", sep="") # postprocessed to match DTM active cells

namelanduse <- paste0("other-dem/veg-type_map", sep="") # postprocessed to match DTM active cells
nameinflow <- paste0("prepro-dem/dtm_A_inflow", sep="") # to deliver rain to closest draining channel with larger area
namechannel <- paste0("prepro-dem/dtm_hcID", sep="") # to deliver rain to closest draining channel with larger area
namezone <- paste0("prepro-dem/zone_map", sep="") # to deliver rain to closest draining channel with larger area
namexyz <- paste0("output/xyz", sep="")

r_landuse <- read.table(namelanduse, header=FALSE, sep=" ", dec = ".", skip=6)
if (is.na(r_landuse[1,1])) {r_landuse <- r_landuse[,-1]}
if (nrow(r_landuse) != myrow | (ncol(r_landuse) != mycol)) {stop("row or col do not match")}

if (DOopen == 1) {
  r_landuse[r_landuse > 0] <- idveg
}

r_inflow <- read.table(nameinflow, header=FALSE, sep="", dec = ".", skip=6)
if (is.na(r_inflow[1,1])) {r_inflow <- r_inflow[,-1]}
if (nrow(r_inflow) != myrow | (ncol(r_inflow) != mycol)) {stop("row or col do not match")}

r_channel <- read.table(namechannel, header=FALSE, sep="", dec = ".", skip=6)
if (is.na(r_channel[1,1])) {r_channel <- r_channel[,-1]}
if (nrow(r_channel) != myrow | (ncol(r_channel) != mycol)) {stop("row or col do not match")}

r_zone <- read.table(namezone, header=FALSE, sep=" ", dec = ".", skip=6)
if (is.na(r_zone[1,1])) {r_zone <- r_zone[,-1]}
if (nrow(r_zone) != myrow | (ncol(r_zone) != mycol)) {stop("row or col do not match")}

xyz_header <- read.table(namexyz,
                         head = FALSE,
                         nrows = 1)
NNOD <- xyz_header[1,1]
N <- xyz_header[1,2]

xyz <- read.table(namexyz, header=FALSE, sep="", dec = ".", skip=1)
xyz_p <- xyz[1:NNOD,]
xyz_p$V2 <- (xyz_p$V2 + 0) /dx
xyz_p$V3 <- (xyz_p$V3 + 0) /dy

tmp <- xyz_p
# tmp <- tmp[myrow:1,] # root map is flipped upside down between cathy and here
# names(tmp) <- 1:mycol
# tmp <- melt(tmp) # , id.vars=c("subject", "sex")
# names(tmp)[which(names(tmp)=="variable")] <- "V2"
# tmp$V3 <- rep(1:myrow, by=mycol)

ggplot(tmp,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = V4)) +
  theme_bw() +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  xlab("Easting") +
  ylab("Northing") +
  labs(fill = "DTM (m)")
ggsave("DTM.jpg", width = 12, height = 8, units = 'in', dpi=300)


# where greenhouse discharge
r_landuse_copy <- r_landuse
r_landuse <- r_landuse[myrow:1,] # root map is flipped upside down between cathy and here
mask_landuse <- r_landuse
# create a 10x10 raster
r_l <- raster(ncol=mycol,nrow=myrow, xmn=1, xmx=mycol, ymn=1, ymx=myrow)
r_l[] <- as.matrix(r_landuse)
sum(r_landuse-r_l) # check consistency
#
# plot raster to see alignment
# par(mar=c(5, 5, 1, 1), mfrow=c(1,1), mgp=c(0,0.8,0))
cols <- c("1" = "red", "2" = "yellow", "3" = "orange", "4" = "darkgreen", "5" = "pink", "6" = "brown") # 0 as na.value
tmp <- r_landuse
names(tmp) <- 1:mycol
tmp <- melt(tmp) # , id.vars=c("subject", "sex")
names(tmp)[which(names(tmp)=="variable")] <- "V2"
tmp$V2 <- as.numeric(tmp$V2)
tmp$V3 <- rep(1:myrow, by=mycol)

ggplot(tmp,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = as.factor(value))) +
  theme_bw() +
  scale_fill_manual(values = cols, na.value=0) +
  xlab("Easting") +
  ylab("Northing") +
  labs(fill = "Land cover")
ggsave("Land_cover.jpg", width = 12, height = 8, units = 'in', dpi=300)

tabulate(tmp$value)/sum(tmp$value!=0)*100 # skip the 0's and values are sorted by class increasing
tabulate(tmp$value)*dx*dy # skip the 0's and values are sorted by class increasing
# sub1, 65 ha , imp is 16%
# sub2, 248 ha, imp is 43%
# sub3, 442 ha, imp is 34%
# sub4, 259 ha, imp is 36%
(0.16*65+0.43*248+0.34*442+0.36*259)/(65+248+442+259) * 100

#
tmp <- r_channel
tmp <- tmp[myrow:1,] # root map is flipped upside down between cathy and here
names(tmp) <- 1:mycol
tmp <- melt(tmp) # , id.vars=c("subject", "sex")
names(tmp)[which(names(tmp)=="variable")] <- "V2"
tmp$V3 <- rep(1:myrow, by=mycol)

ggplot(tmp,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = value))
ggsave("Channel.jpg", width = 12, height = 8, units = 'in', dpi=300)

#
r_inflow_copy <- r_inflow
r_inflow <- r_inflow[myrow:1,] # root map is flipped upside down between cathy and here
r_f <- raster(ncol=mycol,nrow=myrow, xmn=1, xmx=mycol, ymn=1, ymx=myrow)
r_f[] <- as.matrix(r_inflow)
sum(r_inflow-r_f) # check consistency

#
# plot raster to see alignment
# par(mar=c(5, 5, 1, 1), mfrow=c(1,1), mgp=c(0,0.8,0))
tmp <- r_inflow
names(tmp) <- 1:mycol
tmp <- melt(tmp) # , id.vars=c("subject", "sex")
names(tmp)[which(names(tmp)=="variable")] <- "V2"
tmp$V2 <- as.numeric(tmp$V2)
tmp$V3 <- rep(1:myrow, by=mycol)

tmp$value[tmp$value == 0] <- NA
ggplot(tmp,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = value/1e6)) +
  theme_bw() +
  scale_fill_continuous(low = "gray", high = "blue",  na.value="white") +
  xlab("Easting") +
  ylab("Northing") +
  labs(fill = "Inflow area (km2)")
ggsave("Inflow_area.jpg", width = 12, height = 8, units = 'in', dpi=300)

#
r_zone_copy <- r_zone
r_zone <- r_zone[myrow:1,] # root map is flipped upside down between cathy and here
r_f <- raster(ncol=mycol,nrow=myrow, xmn=1, xmx=mycol, ymn=1, ymx=myrow)
r_f[] <- as.matrix(r_zone)
sum(r_zone-r_f) # check consistency
#
# plot raster to see alignment
# par(mar=c(5, 5, 1, 1), mfrow=c(1,1), mgp=c(0,0.8,0))
tmp <- r_zone
names(tmp) <- 1:mycol
tmp <- melt(tmp) # , id.vars=c("subject", "sex")
names(tmp)[which(names(tmp)=="variable")] <- "V2"
tmp$V3 <- rep(1:myrow, by=mycol)

if (length(unique(tmp$value)) == 2) {
  cols <- c("0" = "white", "1" = "darkgreen", "2" = "red")
} else {
  cols <- c("0" = "white", "1" = "darkgreen", "2" = "red", "3" = "steelblue")
}

ggplot(tmp,
       mapping=aes(V2, V3)) +
  geom_tile(aes(fill = as.factor(value))) +
  scale_fill_manual(values = cols)
ggsave("Zone.jpg", width = 12, height = 8, units = 'in', dpi=300)

#
if (DOsetbasefl == 0) {
  # which node high drainage enough to be channel and apply baseflow
  baseflow <- tmp %>% # keep tmp <- r_inflow
    dplyr::arrange(desc(value)) %>%
    dplyr::slice(30)
  # OR could which node is channel and has the smallest area, that is the most upstream, where to apply baseflow
} else {
  # OR select node based on channel upstream
  # perhaps the 40, first number, must be max(y)-row number you see on map, because it is flipped in output wtd map
  baseflow <- as.data.frame(matrix(c(43, NA, 60), nrow=1, byrow=1))
}

#
if (max(unique(tmp$value)) == 2 & DO3zone == 1) {
  tmp <- r_channel
  tmp <- tmp[myrow:1,] # root map is flipped upside down between cathy and here
  j <- which(tmp == 1, arr.ind=T)
  for (i in 1:nrow(j)) {
    r_zone[j[i,1],j[i,2]] <- 3
  }

  write.table(r_zone, file=paste0("prepro-dem/zone_map_inflow", sep=""), dec=".", sep=" ", col.names = TRUE, quote=FALSE, row.names = FALSE)

  cols <- c("0" = "white", "1" = "darkgreen", "2" = "red", "3" = "steelblue")
  r_zone_copy <- r_zone
  r_zone <- r_zone[myrow:1,] # root map is flipped upside down between cathy and here
  r_f <- raster(ncol=mycol,nrow=myrow, xmn=1, xmx=mycol, ymn=1, ymx=myrow)
  r_f[] <- as.matrix(r_zone)
  sum(r_zone-r_f) # check consistency
  #
  # plot raster to see alignment
  # par(mar=c(5, 5, 1, 1), mfrow=c(1,1), mgp=c(0,0.8,0))
  tmp <- r_zone
  names(tmp) <- 1:mycol
  tmp <- melt(tmp) # , id.vars=c("subject", "sex")
  names(tmp)[which(names(tmp)=="variable")] <- "V2"
  tmp$V3 <- rep(1:myrow, by=mycol)

  ggplot(tmp,
         mapping=aes(V2, V3)) +
    geom_tile(aes(fill = as.factor(value))) +
    scale_fill_manual(values = cols)
  ggsave("Zone_inflow.jpg", width = 12, height = 8, units = 'in', dpi=300)
}




#
# KC dynamic - moved down in atmbc loop - here just for checking plot
namekc_all <- list.files("OPAC/", pattern = "\\.csv$")
for (i in 1:length(namekc_all)) {
  namekc <- namekc_all[i]
  mykc <- read.csv(paste0("OPAC/", namekc, sep=""), header = FALSE, sep = ";", dec = ".", stringsAsFactors = FALSE)

  tmp <- mykc
  tmp <- tmp[nrow(tmp):1,]
  names(tmp) <- 1:mycol

  j <- which(mask_landuse==0, arr.ind=T)
  tmp[j] <- NA

  tmp <- melt(tmp) # , id.vars=c("subject", "sex")
  names(tmp)[which(names(tmp)=="variable")] <- "V2"
  tmp$V2 <- as.numeric(tmp$V2)
  tmp$V3 <- rep(1:myrow, by=mycol)

  cols <- c("0" = "orange", "1" = "darkgreen") # 0 as na.value
  ggplot(tmp,
         mapping=aes(V2, V3)) +
    geom_tile(aes(fill = as.factor(value))) +
    theme_bw() +
    scale_fill_manual(values = cols, na.value="white") +
    xlab("Easting") +
    ylab("Northing") +
    labs(fill = "Kc (-)")
  ggsave(paste0("Kc_", i, ".jpg",sep=""), width = 12, height = 8, units = 'in', dpi=300)
}

#
# prepare move rain
if (DOmoverain == 1) {
  atun <- as.data.frame(which(r_landuse == 1, arr.ind=TRUE))
  atun$row_to <- NA
  atun$col_to <- NA
  ltun <- nrow(atun)
  for (i in 1:ltun) {
    nowrow <- atun$row[i]
    nowcol <- atun$col[i]

    # cellFromRowCol(r_l,11,4)

    e <- extent(max(1,nowcol-3),
                min(nowcol+3, mycol),
                max(1, myrow - nowrow -3),
                min(myrow - nowrow +3, myrow)) # switch row col and flip row
    e_l <- as.data.frame(extract(r_l, e, cellnumbers=TRUE))
    names(e_l) <- paste0(names(e_l) , "_l", sep="")
    # plot(r_l)
    # plot(e,add=TRUE)

    e_f <- as.data.frame(extract(r_f, e, cellnumbers=TRUE))
    names(e_f) <- paste0(names(e_f) , "_f", sep="")
    # plot(r_f)
    # plot(e,add=TRUE)
    e_ <- cbind(e_l,e_f)
    e_ <- e_ %>%
      dplyr::filter(e_l > 1) %>% # different than null cells and greenhouse
      dplyr::arrange(desc(value_f)) %>%
      dplyr::slice(1)

    atun$row_to[i] <- rowFromCell(r_l, e_$cell_f)
    atun$col_to[i] <- colFromCell(r_l, e_$cell_f)
  }

  atun$ncel <- 0
  for (i in 1:nrow(atun)) {
    xyz_y_nrow <- atun$row[i]
    xyz_x_ncol <- atun$col[i]
    if ( (xyz_y_nrow == 0 && xyz_x_ncol == 0) || # bottom left
         (xyz_y_nrow == 0 && xyz_x_ncol == mycol) || # bottom right
         (xyz_y_nrow == myrow && xyz_x_ncol == 0) || # top left
         (xyz_y_nrow == myrow && xyz_x_ncol == mycol) ) { # top right
      # all these are 1
      atun$ncel[i] <- 1 # cells per node
    } else if ( (xyz_y_nrow == 0 && xyz_x_ncol > 0 && xyz_x_ncol < mycol) || # bottom
                (xyz_y_nrow == myrow && xyz_x_ncol > 0 && xyz_x_ncol < mycol) || # top
                (xyz_y_nrow > 0 && xyz_y_nrow < myrow && xyz_x_ncol == 0) || # left
                (xyz_y_nrow > 0 && xyz_y_nrow < myrow && xyz_x_ncol == mycol) ) { # right
      # all these are 2
      atun$ncel[i] <- 2 # cells per node
    } else if (xyz_y_nrow > 0 && xyz_y_nrow < myrow && xyz_x_ncol > 0 && xyz_x_ncol < mycol) {
      # all these are 4
      atun$ncel[i] <- 4 # cells per node
    }
  }
}





### Weather data warmup
nameMETEOin <- "input/Shiny_Rain_BC_warmup.csv"
myRain <- read.csv(nameMETEOin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

names(myRain)[which(names(myRain) == "Date_dum")] <- "Date"
names(myRain)[which(names(myRain) == "rainsum")] <- "Rain_mm"

if (!is.na( as.numeric(as.POSIXlt(as.character(myRain$Date[1]), format = "%Y-%m-%d %H:%M")) )) {
  myRain$Date_pos <- as.numeric(as.POSIXlt(as.character(myRain$Date), format = "%Y-%m-%d %H:%M", tz="UTC"))
  myRain$Date <- as.character(format( as.POSIXct(myRain$Date_pos, origin="1970-01-01", tz="UTC"), "%d.%m.%Y %H:%M"))
} else {
  myRain$Date_pos <- as.numeric(as.POSIXlt(as.character(myRain$Date), format = "%d.%m.%Y %H:%M", tz="UTC"))
}

myRain <- subset(myRain , select=c("Date","Date_pos","Rain_mm"))


j <- which(is.na(myRain$Date))
if (length(j)>0) {
  myRain <- myRain[-j,]
  print("removing change of solar hours")
  print("removing NA in Date_pos due to CET and not UTC, j=2907 31.03.2024 02:09")
}

################################################################################
# C:\REWATERING\Monitoraggio
# could be that length not the same
# nameMETEOin <- "input/Shiny_ET0_BC.csv"
# tmp <- read.csv(nameMETEOin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
# myRain$ET0 <- tmp$ET0
# myRain$ET0 <- 0
nameET0in <- "input/Shiny_ET0_BC_warmup.csv"
ET0 <- read.csv(nameET0in, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

if (!is.na( as.numeric(as.POSIXlt(as.character(ET0$Date[1]), format = "%Y-%m-%d %H:%M:%S")) )) {
  ET0$Date_pos <- as.numeric(as.POSIXlt(as.character(ET0$Date), format = "%Y-%m-%d %H:%M", tz="UTC"))
  ET0$Date <- as.character(format( as.POSIXct(ET0$Date_pos, origin="1970-01-01", tz="UTC"), "%d.%m.%Y %H:%M"))
} else {
  ET0$Date_pos <- as.numeric(as.POSIXlt(as.character(ET0$Date), format = "%d.%m.%Y %H:%M", tz="UTC"))
}

j <- which(is.na(ET0$Date))
if (length(j)>0) {
  ET0 <- ET0[-j,]
  print("removing change of solar hours")
}

################################################################################
nameET0green <- "input/Shiny_ET0_BC_climatemodel_warmup.csv"
ET0green <- read.csv(nameET0green, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
ET0green <- ET0green %>%
  dplyr::select(c(Date,ET))
if (!is.na( as.numeric(as.POSIXlt(as.character(ET0green$Date[1]), format = "%Y-%m-%d %H:%M")) )) {
  ET0green$Date_pos <- as.numeric(as.POSIXlt(as.character(ET0green$Date), format = "%Y-%m-%d %H:%M", tz="UTC"))
  ET0green$Date <- as.character(format( as.POSIXct(ET0green$Date_pos, origin="1970-01-01", tz="UTC"), "%d.%m.%Y %H:%M"))
} else {
  ET0green$Date_pos <- as.numeric(as.POSIXlt(as.character(ET0green$Date), format = "%d.%m.%Y %H:%M", tz="UTC"))
}

j <- which(is.na(ET0green$Date))
if (length(j)>0) {
  ET0green <- ET0green[-j,]
  print("removing change of solar hours")
}
j <- which(duplicated(ET0green$Date))
if (length(j)>0) {
  ET0green <- ET0green[-j,]
  print("removing duplicated value")
}

################################################################################
j <- head(which(substr(myRain$Date,12,13) == "00") , 1)
if (j > 1) {
  print("remove first DOY Rain if not 24 observations")
  myRain <- myRain[j:nrow(myRain),]
}
#
j <- head(which(substr(ET0$Date,12,13) == "00") , 1)
if (j > 1) {
  print("remove first DOY ET if not 24 observations")
  ET0 <- ET0[j:nrow(ET0),]
}

################################################################################
j1 <- head(which(substr(myRain$Date,1,10) == date_st_warmup),1)
j2 <- tail(which(substr(myRain$Date,1,10) == date_en_warmup),1)
myRain <- myRain[j1:j2,]
rownames(myRain) <- 1:nrow(myRain)

j1 <- head(which(substr(ET0$Date,1,10) == date_st_warmup),1)
j2 <- tail(which(substr(ET0$Date,1,10) == date_en_warmup),1)
ET0 <- ET0[j1:j2,]
rownames(ET0) <- 1:nrow(ET0)

j1 <- head(which(substr(ET0green$Date,1,10) == date_st_warmup),1)
j2 <- tail(which(substr(ET0green$Date,1,10) == date_en_warmup),1)
ET0green <- ET0green[j1:j2,]
rownames(ET0green) <- 1:nrow(ET0green)

# if (ET0green$Date[nrow(ET0green)] == "30.11.2023 22:00") {
#   ET0green <- rbind(ET0green , ET0green[nrow(ET0green),])
#   ET0green$Date[nrow(ET0green)] <- "30.11.2023 23:00"
#   ET0green$Date_pos[nrow(ET0green)] <- as.numeric(as.POSIXlt(ET0green$Date[nrow(ET0green)], format = "%d.%m.%Y %H:%M"))
# }

print("accounting for solarization")
ET0green$DOY <- as.numeric(as.POSIXlt(ET0green$Date, format = "%d.%m.%Y %H:%M")$yday) # first day of the year is 0, thus I add 1
jSolar <- which(ET0green$DOY >=151 & ET0green$DOY <= 211) # june july solarization
if (length(jSolar) > 0) {
  ET0green$ET[jSolar] <- 0
  rm(jSolar)
}
ET0green <- ET0green %>%
  dplyr::select(-DOY)

################################################################################

myRain$ET0 <- ET0$ET0
myRain$ET0green <- ET0green$ET

j <- which(is.na(myRain$Rain_mm))
j <- which(is.na(myRain$ET0))
j <- which(is.na(myRain$ET0green))

################################################################################

nighttime <- c("00","01",  "02",  "03",  "04",  "05",  "20", "21", "22", "23")
j <- which(substr(myRain$Date,12,13) %in% nighttime)
myRain$ET0[j] <- 0
myRain$ET0green[j] <- 0

################################################################################
# Irrigation
# myRain$DOY <- as.numeric(as.POSIXlt(as.character(myRain$Date), format = "%d.%m.%Y %H:%M")$yday) # first day of the year is 0, thus I add 1
myRain$YMD <- substr(myRain$Date,1,10)
myRain$Irr <- 0
myRain$Irrgreen <- 0
#
myirr <- myRain %>%
  dplyr::group_by(YMD) %>%
  dplyr::mutate(ET0 = sum(ET0),
                ET0green = sum(ET0green)) %>%
  # dplyr::slice(10) %>% # 10 to select 9 am
  dplyr::ungroup() %>%
  dplyr::arrange(Date_pos)
myirr <- myirr[which(substr(myirr$Date, 12,13) == "09"), ] # slice 10 to select 9 am)], but found also other things once, odd


################################################################################
j <- which(substr(myRain$Date, 12,13) == "09")
# myRain$Irr[j] <- myirr$Irr
myRain$Irr[j] <- myirr$ET0
myRain$Irrgreen[j] <- myirr$ET0green
rm(myirr)

myAtmbc <- myRain # this is for every cell

################################################################################
### Weather data with water level measured
nameMETEOin <- "input/Shiny_Rain_BC.csv"
myRain <- read.csv(nameMETEOin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

names(myRain)[which(names(myRain) == "Date_dum")] <- "Date"
names(myRain)[which(names(myRain) == "rainsum")] <- "Rain_mm"

if (!is.na( as.numeric(as.POSIXlt(as.character(myRain$Date[1]), format = "%Y-%m-%d %H:%M")) )) {
  myRain$Date_pos <- as.numeric(as.POSIXlt(as.character(myRain$Date), format = "%Y-%m-%d %H:%M", tz="UTC"))
  myRain$Date <- as.character(format( as.POSIXct(myRain$Date_pos, origin="1970-01-01", tz="UTC"), "%d.%m.%Y %H:%M"))
} else {
  myRain$Date_pos <- as.numeric(as.POSIXlt(as.character(myRain$Date), format = "%d.%m.%Y %H:%M", tz="UTC"))
}

myRain <- subset(myRain , select=c("Date","Date_pos","Rain_mm"))

j <- which(is.na(myRain$Date))
if (length(j)>0) {
  myRain <- myRain[-j,]
  print("removing change of solar hours")
  print("removing NA in Date_pos due to CET and not UTC, j=2907 31.03.2024 02:09")
}

################################################################################
# C:\REWATERING\Monitoraggio
# could be that length not the same
# nameMETEOin <- "input/Shiny_ET0_BC.csv"
# tmp <- read.csv(nameMETEOin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
# myRain$ET0 <- tmp$ET0
# myRain$ET0 <- 0
nameET0in <- "input/Shiny_ET0_BC.csv"
ET0 <- read.csv(nameET0in, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

if (!is.na( as.numeric(as.POSIXlt(as.character(ET0$Date[1]), format = "%Y-%m-%d %H:%M:%S")) )) {
  ET0$Date_pos <- as.numeric(as.POSIXlt(as.character(ET0$Date), format = "%Y-%m-%d %H:%M", tz="UTC"))
  ET0$Date <- as.character(format( as.POSIXct(ET0$Date_pos, origin="1970-01-01", tz="UTC"), "%d.%m.%Y %H:%M"))
} else {
  ET0$Date_pos <- as.numeric(as.POSIXlt(as.character(ET0$Date), format = "%d.%m.%Y %H:%M", tz="UTC"))
}

j <- which(is.na(ET0$Date))
if (length(j)>0) {
  ET0 <- ET0[-j,]
  print("removing change of solar hours")
}

################################################################################
nameET0green <- "input/Shiny_ET0_BC_climatemodel.csv"
ET0green <- read.csv(nameET0green, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

if (!is.na( as.numeric(as.POSIXlt(as.character(ET0green$Date[1]), format = "%Y-%m-%d %H:%M")) )) {
  ET0green$Date_pos <- as.numeric(as.POSIXlt(as.character(ET0green$Date), format = "%Y-%m-%d %H:%M", tz="UTC"))
  ET0green$Date <- as.character(format( as.POSIXct(ET0green$Date_pos, origin="1970-01-01", tz="UTC"), "%d.%m.%Y %H:%M"))
} else {
  ET0green$Date_pos <- as.numeric(as.POSIXlt(as.character(ET0green$Date), format = "%d.%m.%Y %H:%M", tz="UTC"))
}

j <- which(is.na(ET0green$Date))
if (length(j)>0) {
  ET0green <- ET0green[-j,]
  print("removing change of solar hours")
}
j <- which(duplicated(ET0green$Date))
if (length(j)>0) {
  ET0green <- ET0green[-j,]
  print("removing duplicated value")
}

################################################################################
j <- head(which(substr(myRain$Date,12,13) == "00") , 1)
if (j > 1) {
  print("remove first DOY Rain if not 24 observations")
  myRain <- myRain[j:nrow(myRain),]
}
#
j <- head(which(substr(ET0$Date,12,13) == "00") , 1)
if (j > 1) {
  print("remove first DOY ET if not 24 observations")
  ET0 <- ET0[j:nrow(ET0),]
}

################################################################################
j1 <- head(which(substr(myRain$Date,1,10) == date_st),1)
j2 <- tail(which(substr(myRain$Date,1,10) == date_en),1)
myRain <- myRain[j1:j2,]
rownames(myRain) <- 1:nrow(myRain)

j1 <- head(which(substr(ET0$Date,1,10) == date_st),1)
j2 <- tail(which(substr(ET0$Date,1,10) == date_en),1)
ET0 <- ET0[j1:j2,]
rownames(ET0) <- 1:nrow(ET0)

j1 <- head(which(substr(ET0green$Date,1,10) == date_st),1)
j2 <- tail(which(substr(ET0green$Date,1,10) == date_en),1)
ET0green <- ET0green[j1:j2,]
rownames(ET0green) <- 1:nrow(ET0green)

print("accounting for solarization")
ET0green$DOY <- as.numeric(as.POSIXlt(ET0green$Date, format = "%d.%m.%Y %H:%M")$yday) # first day of the year is 0, thus I add 1
jSolar <- which(ET0green$DOY >=151 & ET0green$DOY <= 211) # june july solarization
if (length(jSolar) > 0) {
  ET0green$ET[jSolar] <- 0
  rm(jSolar)
}
ET0green <- ET0green %>%
  dplyr::select(-DOY)

################################################################################

myRain$ET0 <- ET0$ET0
# myRain$ET0green <- ET0green$ET
# UTC CEST dummy
myRain$ET0green <- ET0green$ET


j <- which(is.na(myRain$Rain_mm))
j <- which(is.na(myRain$ET0))
j <- which(is.na(myRain$ET0green))
if (length(j)>0) {
  print("UTC CEST dummy ET0green")
  myRain$ET0green[j] <- 0
}


################################################################################

nighttime <- c("00","01",  "02",  "03",  "04",  "05",  "20", "21", "22", "23")
j <- which(substr(myRain$Date,12,13) %in% nighttime)
myRain$ET0[j] <- 0
myRain$ET0green[j] <- 0

################################################################################
# Irrigation
# myRain$DOY <- as.numeric(as.POSIXlt(as.character(myRain$Date), format = "%d.%m.%Y %H:%M")$yday) # first day of the year is 0, thus I add 1
myRain$YMD <- substr(myRain$Date,1,10)
myRain$Irr <- 0
myRain$Irrgreen <- 0
#
myirr <- myRain %>%
  dplyr::group_by(YMD) %>%
  dplyr::mutate(ET0 = sum(ET0),
                ET0green = sum(ET0green)) %>%
  # dplyr::slice(10) %>% # 10 to select 9 am
  dplyr::ungroup() %>%
  dplyr::arrange(Date_pos)
myirr <- myirr[which(substr(myirr$Date, 12,13) == "09"), ] # slice 10 to select 9 am)], but found also other things once, odd


################################################################################
j <- which(substr(myRain$Date, 12,13) == "09")
# myRain$Irr[j] <- myirr$Irr
myRain$Irr[j] <- myirr$ET0
myRain$Irrgreen[j] <- myirr$ET0green
rm(myirr)








################################################################################
myAtmbc <- rbind(myAtmbc , myRain) # this is for every cell

###
myAtmbc$Rain_m3s <- myAtmbc$Rain_mm / 1e3 / dt_weather # from mm/dt to m/s
myAtmbc$ET0_m3s <- myAtmbc$ET0 / 1e3 / dt_weather # from mm/dt to m/s

# find formulation
myAtmbc$ET0green_m3s <- myAtmbc$ET0green / 1e3 / dt_weather # from mm/dt to m/s

myAtmbc$Irr_m3s <- myAtmbc$Irr / 1e3 / dt_weather # from mm/dt to m/s
myAtmbc$Irrgreen_m3s <- myAtmbc$Irrgreen / 1e3 / dt_weather # from mm/dt to m/s

# NEUMANN greenhouse irrigation
write.table(myAtmbc, file=paste0("R/Table_NEUMANN.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)

#
sum(myAtmbc$Rain_m3s + myAtmbc$Irr_m3s - myAtmbc$ET0_m3s)
sum(myAtmbc$Rain_m3s + myAtmbc$Irrgreen_m3s - myAtmbc$ET0green_m3s)
# plot(myAtmbc$Rain_m3s - myAtmbc$ET0_m3s - myAtmbc$ET0green_m3s, type="l")
# lines(myAtmbc$Irr_m3s, col="red")
# lines(myAtmbc$Irrgreen_m3s, col="blue")

#
myAtmbc$Date_pos_R <- as.POSIXct(strptime(myAtmbc$Date, format="%d.%m.%Y %H:%M",tz="UTC"))
plot(myAtmbc$Date_pos_R, cumsum(myAtmbc$Rain_mm), ylim=c(0,max(c(cumsum(myAtmbc$Rain_mm),cumsum(myAtmbc$ET0),cumsum(myAtmbc$ET0green)))),
     type="l", ylab="Cumulative (m/s)", xlab="Date")
lines(myAtmbc$Date_pos_R, cumsum(myAtmbc$ET0), col="steelblue")
lines(myAtmbc$Date_pos_R, cumsum(myAtmbc$ET0green), col="forestgreen")



#
yo <- cumsum(myAtmbc$ET0)
yg <- cumsum(myAtmbc$ET0green)
ymin <- min( min(yo) , min(yg) )
ymax <- max( max(yo) , max(yg) )

jpeg("R_ET.jpg", width = 8, height = 6, units = 'in', res=300)
par(mar=c(5, 5.5, 1, 1), mfrow=c(1,1))
plot(myAtmbc$Date_pos_R, yo, type="l", col="black", lwd=3,
     ylab=expression(Cumulative ~ ET[0] ~ (mm)), xlab="Date", ylim=c(ymin,ymax*1.1),
     cex.axis=2, cex.lab=2)
lines(myAtmbc$Date_pos_R, yg, col="red", lwd=3)
legend("topleft", legend=c("Outdoor", "Indoor"),
       col=c("black", "red"), lty=1, lwd=3, cex=2, box.lty=0, bg=NULL, seg.len=1, horiz=F)
dev.off()


#
yo <- cumsum(myAtmbc$Rain_mm - myAtmbc$ET0)
yg <- cumsum(myAtmbc$Rain_mm - myAtmbc$ET0green)
ymin <- min( min(yo) , min(yg) )
ymax <- max( max(yo) , max(yg) )

jpeg("R_Rain_ET.jpg", width = 8, height = 6, units = 'in', res=300)
par(mar=c(5, 5.5, 1, 1), mfrow=c(1,1))
plot(myAtmbc$Date_pos_R, yo, type="l", col="black", lwd=3,
     ylab=expression(Cumulative ~ R ~ - ~ ET[0] ~ (mm)), xlab="Date", ylim=c(ymin,ymax*1.1),
     cex.axis=2, cex.lab=2)
lines(myAtmbc$Date_pos_R, yg, col="red", lwd=3)
legend("topleft", legend=c("Outdoor", "Indoor"),
       col=c("black", "red"), lty=1, lwd=3, cex=2, box.lty=0, bg=NULL, seg.len=1, horiz=F)
dev.off()

# If you want to print out
# myAtmbc_out <- myAtmbc %>%
# dplyr::select(Date,Date_pos,DOY,Rain_m3s,ET0_m3s,ET0green_m3s,Irr_m3s,Irrgreen_m3s)
# write.table(myAtmbc_out,
#             nameMETEOout,
#             append = FALSE, sep = ";", dec = ".", row.names = FALSE, col.names = TRUE, quote = FALSE)
# rm(myAtmbc_out)

# create time series to give diel fluctuations for withdraw in the night to recharge reservoirs
# between 0.1 and 0.2

if (dobaseflow == 1) {
  if (dobaseflowlinear == 1) {
    # mybaseflow <- rep(0.17, nrow(myAtmbc))
    mybaseflow <- rep(0.3, nrow(myAtmbc)) / 80/80# 0.3 m3/s to 4.6875e-5 m/s
  } else {
    mybaseflow <- rep(0.25, nrow(myAtmbc))
    mybaseflow <- mybaseflow + 0.05*cos(2*3.14/24 * (12+myAtmbc$HOUR))
  }
} else {
  mybaseflow <- 0
}

# number greenhouse cells
# Area_GH <- dx*dy*sum(r_landuse==1)
# frac_GH is the rain falls homogeneously on greenhouse cells too, but (1-frac_GH) goes to the channel
frac_GH <- 1.0

cell2nod <- function(rmap,rain,et0,irr,et0_green,irr_green,frac_GH,nowkc) {
  switch(as.character(rmap),
         "0" = { # outside domain. imagine rain is the same
           y <- rain
         },
         "1" = { # greenhouse
           #TODO
           # solarization accounted for setting et = 0, otherwise kc=1
           y <- frac_GH*rain - et0_green * 1 # "coupled with neumann in 2nd node" irr_greenhouse
           # y <- - et0_green # put rain in neigbor cell without greenhouse
         },
         "2" = { # storage vehicles
           #TODO
           y <- rain # rain should go somewhere
         },
         "3" = { # net
           #TODO
           y <- rain - et0 * nowkc # irr_greenhouse  - eto_greenhouse
         },
         "4" = { # crop
           #TODO
           y <- rain - et0 * nowkc
         },
         "5" = { # crop
           #TODO
           y <- rain
         },
         "6" = { # road
           #TODO
           y <- rain
         }
  )
  y <- y / 4 # 4 nodes per cell
  return(y)
}


# kc dynamic
# namekc <- paste0("OPAC/map_kc_", gsub("0","",substr(myAtmbc$Date[ii],4,5)), ".csv", sep="")
namekc_all <- list.files("OPAC/", pattern = "\\.csv$")



namemonth <- substr(namekc_all,8,11) # this is for matching the year and month for assigning dynamic K

mykc <- array(0, c(nrow(r_landuse),ncol(r_landuse),length(namekc_all)))
cont <- 0
for (i in 1:length(namekc_all)) {
  cont <- cont+1
  namekc <- paste0("OPAC/", namekc_all[i], sep="")
  tmp <- read.csv(namekc, header = FALSE, sep = ";", dec = ".", stringsAsFactors = FALSE)
  tmp <- tmp[nrow(tmp):1,]

  if (DOopen==1) {
    # if (namemonth[i] %in% c("04","05","06","07","08","09","10")) {
    tmp[r_landuse == idveg] <- 1
    # }
  }
  mykc[,,cont] <- data.matrix(tmp)
}



# start
if (exists("date_st_pre")) {
  j <- head(which(substr(myAtmbc$Date,1,10) == date_st_pre),1)
  contp <- j
  mindatepos <- min(myAtmbc$Date_pos)
  myAtmbc_full <- myAtmbc
  myAtmbc <- myAtmbc[j:nrow(myAtmbc),]
  print(myAtmbc$Date_pos[1]-mindatepos) # date ti delete from in atmnbc
} else {
  contp <- 1
  mindatepos <- min(myAtmbc$Date_pos)
}
sumwater <- rep(0,nrow(myAtmbc)) # counter for water mass balance

if (!exists("date_st_pre")) {
  write.table("0 1",
              nameout,
              append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  # r_landuse_copy <- r_landuse
  # r_landuse <- r_landuse[myrow:1,] # root map is flipped upside down between cathy and here
}

if (max(myAtmbc$Rain_m3s)>0) {
  iprint <- c(sort(head(sort(myAtmbc$ET0_m3s, decreasing=TRUE, index.return=TRUE)$ix,1)))
}
if (max(myAtmbc$Rain_m3s)>0) {
  iprint <- c(iprint, sort(head(sort(myAtmbc$Rain_m3s, decreasing=TRUE, index.return=TRUE)$ix,1)))
}
iprint <- sort(unique(iprint))
namelabel <- as.character(iprint) # if less than 10, namelabel add "0" to order in folder
j <- which(nchar(namelabel)==1)
if (length(j)>0) {
  namelabel[j] <- paste0("0", namelabel[j], sep="", collapse = NULL)
}
rm(j)

print("this number should be close to number of active nodes")
sum(r_landuse>0)
nrow(xyz_p)
id_map <- as.data.frame(which(r_landuse>0, arr.ind = T))
id_map <- id_map %>%
  dplyr::arrange(desc(row)) # sorting like xyz_p

# warm up
addndays <- 0 * 86400 # in seconds
nrepwarmup <- 1 # how many times to repeat the warm up?
stepndays <- addndays * nrepwarmup / dt_weather
# jfactor_warmup <- c(0.1, -0.1) # decrease irrigation
jfactor_warmup <- c(1, 1) # modify atmbc, vector of same length of nrepwarmup or longer

# # so that it looks like r_landuse plotted in geom_tile, with northing increasing bottom up and easting left right
# mymap <- r_landuse_copy
# rownames(mymap) <- nrow(r_landuse_copy):1
# names(mymap) <- 1:ncol(r_landuse_copy)
# warm up
if (stepndays > 0 & !exists("date_st_pre")) {
  for (iii in 1:nrepwarmup) {
    pb = txtProgressBar(min = 1, max = stepndays/nrepwarmup, initial = 1)
    for (ii in 1:(stepndays/nrepwarmup)) {
      setTxtProgressBar(pb,ii)
      myNode_atmbc <- matrix(0, nrow=1, ncol=nrow(xyz_p)) # use 0 because it is possibly a sum

      # which position of KC array we are now
      myAtm_month <- paste0(substr(myAtmbc$Date[ii],9,10), substr(myAtmbc$Date[ii],4,5), sep="")
      jkc <- which(namemonth == myAtm_month)

      # rain on greenhouse
      # rainoutlet <- myAtmbc$Rain_m3s[ii] * nnod_green
      # add these partitioned along stream or in the outlet cell at the end of for loop. need a find to have position

      for (i in 1:nrow(id_map)) {
        # find cell
        cell_nrow <- id_map$row[i]
        cell_ncol <- id_map$col[i]

        # find the 4 nodes
        not <- cell_nrow # node top
        nob <- cell_nrow - 1
        nol <- cell_ncol - 1
        nor <- cell_ncol

        nowkc <- mykc[cell_nrow,cell_ncol, jkc] # this should be dynamic based on month
        # to test using ii=11, set nowkc <- 0 and calculate sum(r_landuse==1) * myAtmbc$ET0green_m3s[ii] compared with sum(myNode_atmbc)
        tmp <- cell2nod(r_landuse[cell_nrow,cell_ncol],
                        myAtmbc$Rain_m3s[ii],
                        myAtmbc$ET0_m3s[ii],
                        myAtmbc$Irr_m3s[ii],
                        myAtmbc$ET0green_m3s[ii],
                        myAtmbc$Irrgreen_m3s[ii],
                        frac_GH,
                        nowkc
        )

        # # get position node
        # cell_ncol <- xyz_p$V2[i]
        # cell_nrow <- xyz_p$V3[i]
        # j <- which(xyz_p$V2 == nol & xyz_p$V3 == not)
        # myNode_atmbc[j] <- tmp
        myNode_atmbc[which(xyz_p$V2 == nol & xyz_p$V3 == not)] <- tmp + myNode_atmbc[which(xyz_p$V2 == nol & xyz_p$V3 == not)]
        myNode_atmbc[which(xyz_p$V2 == nol & xyz_p$V3 == nob)] <- tmp + myNode_atmbc[which(xyz_p$V2 == nol & xyz_p$V3 == nob)]
        myNode_atmbc[which(xyz_p$V2 == nor & xyz_p$V3 == not)] <- tmp + myNode_atmbc[which(xyz_p$V2 == nor & xyz_p$V3 == not)]
        myNode_atmbc[which(xyz_p$V2 == nor & xyz_p$V3 == nob)] <- tmp + myNode_atmbc[which(xyz_p$V2 == nor & xyz_p$V3 == nob)]

      }


      if (DOmoverain == 1 & myAtmbc$Rain_m3s[ii] > 0) {
        for (i in 1:nrow(atun)) { # skipping unique
          nowrow <- atun$row_to[i]
          nowcol <- atun$col_to[i]

          j <- which(xyz_p$V2 == nowcol & xyz_p$V3 == nowrow)
          # myNode_atmbc[1,j] <- myNode_atmbc[1,j] + myAtmbc$Rain_m3s[ii] * atun_unique$myflow[i] # times n cells dischergin there
          myNode_atmbc[1,j] <- myNode_atmbc[1,j] + (1-frac_GH)*myAtmbc$Rain_m3s[ii] / 1 # possible cells connected to node, with 4 there is less water
        }
      }

      myNode_atmbc <- myNode_atmbc * jfactor_warmup[iii]

      if (exists("baseflow")) {
        # Q og 0.1 m3/s at aoutlet
        if (DOsetbasefl == 1) {
          nowrow <- baseflow$V1
          nowcol <- baseflow$V3
        } else {
          nowrow <- baseflow$V2
          nowcol <- baseflow$V3
        }

        j <- which(xyz_p$V2 == nowcol & xyz_p$V3 == nowrow)
        myNode_atmbc[1,j] <- myNode_atmbc[1,j] + mybaseflow[ii]
      }

      write.table(myAtmbc$Date_pos[ii]-mindatepos + addndays*nrepwarmup * (iii-1),
                  nameout,
                  append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      write.table(myNode_atmbc,
                  nameout,
                  append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
  }
  close(pb)
}


pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1)
for (ii in 1:nrow(myAtmbc)) {
  setTxtProgressBar(pb,ii)
  myNode_atmbc <- matrix(0, nrow=1, ncol=nrow(xyz_p)) # use 0 because it is possibly a sum

  # which position of KC array we are now
  myAtm_month <- paste0(substr(myAtmbc$Date[ii],9,10), substr(myAtmbc$Date[ii],4,5), sep="")
  jkc <- which(namemonth == myAtm_month)

  # rain on greenhouse
  # rainoutlet <- myAtmbc$Rain_m3s[ii] * nnod_green
  # add these partitioned along stream or in the outlet cell at the end of for loop. need a find to have position

  for (i in 1:nrow(id_map)) {
    # find cell
    cell_nrow <- id_map$row[i]
    cell_ncol <- id_map$col[i]

    # find the 4 nodes
    not <- cell_nrow # node top
    nob <- cell_nrow - 1
    nol <- cell_ncol - 1
    nor <- cell_ncol

    nowkc <- mykc[cell_nrow,cell_ncol, jkc] # this should be dynamic based on month
    # to test using ii=11, set nowkc <- 0 and calculate sum(r_landuse==1) * myAtmbc$ET0green_m3s[ii] compared with sum(myNode_atmbc)
    tmp <- cell2nod(r_landuse[cell_nrow,cell_ncol],
                    myAtmbc$Rain_m3s[ii],
                    myAtmbc$ET0_m3s[ii],
                    myAtmbc$Irr_m3s[ii],
                    myAtmbc$ET0green_m3s[ii],
                    myAtmbc$Irrgreen_m3s[ii],
                    frac_GH,
                    nowkc
    )

    # # get position node
    # cell_ncol <- xyz_p$V2[i]
    # cell_nrow <- xyz_p$V3[i]
    # j <- which(xyz_p$V2 == nol & xyz_p$V3 == not)
    # myNode_atmbc[j] <- tmp
    myNode_atmbc[which(xyz_p$V2 == nol & xyz_p$V3 == not)] <- tmp + myNode_atmbc[which(xyz_p$V2 == nol & xyz_p$V3 == not)]
    myNode_atmbc[which(xyz_p$V2 == nol & xyz_p$V3 == nob)] <- tmp + myNode_atmbc[which(xyz_p$V2 == nol & xyz_p$V3 == nob)]
    myNode_atmbc[which(xyz_p$V2 == nor & xyz_p$V3 == not)] <- tmp + myNode_atmbc[which(xyz_p$V2 == nor & xyz_p$V3 == not)]
    myNode_atmbc[which(xyz_p$V2 == nor & xyz_p$V3 == nob)] <- tmp + myNode_atmbc[which(xyz_p$V2 == nor & xyz_p$V3 == nob)]

  }


  if (DOmoverain == 1 & myAtmbc$Rain_m3s[ii] > 0) {
    for (i in 1:nrow(atun)) { # skipping unique
      nowrow <- atun$row_to[i]
      nowcol <- atun$col_to[i]

      j <- which(xyz_p$V2 == nowcol & xyz_p$V3 == nowrow)
      # myNode_atmbc[1,j] <- myNode_atmbc[1,j] + myAtmbc$Rain_m3s[ii] * atun_unique$myflow[i] # times n cells dischergin there
      myNode_atmbc[1,j] <- myNode_atmbc[1,j] + (1-frac_GH)*myAtmbc$Rain_m3s[ii] / 1 # possible cells connected to node, with 4 there is less water
    }
  }

  sumwater[ii] <- sum(myNode_atmbc)

  if (ii %in% iprint) {
    myNode_atmbc_plot <- as.data.frame(t(myNode_atmbc))
    myNode_atmbc_plot <- cbind(myNode_atmbc_plot , xyz_p)
    names(myNode_atmbc_plot)[1] <- "atmbc"
    # myNode_atmbc_plot$atmbc[myNode_atmbc_plot$atmbc==0] <- NA
    p <- ggplot(myNode_atmbc_plot,
                mapping=aes(V2, V3)) +
      # geom_tile(aes(fill = sign(atmbc)*abs(log10(abs(atmbc))))) +
      geom_tile(aes(fill = atmbc*1e3*dt_weather)) +
      theme_bw() +
      scale_fill_gradient2(na.value = "black") + #   low = muted("red"),  mid = "white",  high = muted("blue"),  midpoint = 0,  space = "Lab",  na.value = "grey50",
      xlab("Easting") +
      ylab("Northing") +
      # labs(fill = "sign(log10(Atmbc)) (m/s)")
      labs(fill = "Atmbc (mm/h)")
    print(
      p
    )
    ggsave(paste0(nameout, "_", namelabel[contp], ".jpg", sep=""), width = 12, height = 8, units = 'in', dpi=300)
    contp <- contp + 1
  }

  if (exists("baseflow")) {
    # Q og 0.1 m3/s at aoutlet
    if (DOsetbasefl == 1) {
      nowrow <- baseflow$V1
      nowcol <- baseflow$V3
    } else {
      nowrow <- baseflow$V2
      nowcol <- baseflow$V3
    }

    j <- which(xyz_p$V2 == nowcol & xyz_p$V3 == nowrow)
    myNode_atmbc[1,j] <- myNode_atmbc[1,j] + mybaseflow[ii]
  }


  write.table(myAtmbc$Date_pos[ii]-mindatepos + addndays*nrepwarmup,
              nameout,
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(myNode_atmbc,
              nameout,
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
}
close(pb)
















################################################################################################################
# irrigation
myAtmbc_irr <- myAtmbc

# boundary conditions do not need to have a flux every hour but only when need to start and end.
# so skip first 0.
# then find fluxes =! 0 and put a 0 when they end
jdel <- NULL
zerobefore <- 1
for (ii in 2:(nrow(myAtmbc)-1)) {
  # if ((myAtmbc$Irr_m3s[ii]+myAtmbc$Irrgreen_m3s[ii]) == 0) {
  if (myAtmbc$Irr_m3s[ii] == 0) {
    if (zerobefore == 1) {
      jdel <- c(jdel , ii)
    } else {
      zerobefore <- 1
    }
  } else {
    zerobefore <- 0
  }
}
myAtmbc <- myAtmbc[-jdel,]
rm(jdel)

if (max(myAtmbc$Rain_m3s)>0) {
  iprint <- sort(head(sort(myAtmbc$Irr_m3s, decreasing=TRUE, index.return=TRUE)$ix,1))
}
iprint <- sort(unique(iprint))
namelabel <- as.character(iprint) # if less than 10, namelabel add "0" to order in folder
j <- which(nchar(namelabel)==1)
if (length(j)>0) {
  namelabel[j] <- paste0("0", namelabel[j], sep="", collapse = NULL)
}
rm(j)


# 1         IRR_FLAG (=1 for irrigation module)
# 0.65      IRR_SW   (threshold S_w value for application or not of available irrigation water)
# 0         IRR_HS   (=0 for spatially variable irrigation input data, in which case irrigation rates are read from a raster map; = 1 for homogeneous irrigation, in which case one value is read in and is replicated to the raster map)
# 0.0       TIMEIN ; followed by raster map of irrigation rates; the min/max rates in the 2-h blocks starting at 2 d (1.728e5 s) and at 20 d (1.728e6 s) are 5.55e-8 m/s (*1000*60*60 = 0.2 mm/hr) / 41.67e-8 m/s (*1000*60*60 = 1.54 mm/h)
if (!exists("date_st_pre")) {
  write.table("1         IRR_FLAG (=1 for irrigation module)",
              paste0("other-dem/irrigation_map", sep=""),
              append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table("0.35      IRR_SW   (threshold S_w value for application or not of available irrigation water)",
              paste0("other-dem/irrigation_map", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table("0         IRR_HS   (=0 for spatially variable irrigation input data, in which case irrigation rates are read from a raster map; = 1 for homogeneous irrigation, in which case one value is read in and is replicated to the raster map)",
              paste0("other-dem/irrigation_map", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
}


# warm up
addndays <- 0 * 86400 # in seconds
nrepwarmup <- 1 # how many times to repeat the warm up?
stepndays <- addndays * nrepwarmup / dt_weather
# jfactor_warmup <- c(0.1, -0.1) # decrease irrigation
jfactor_warmup <- c(1, 1) # modify atmbc, vector of same length of nrepwarmup or longer



# sumwater <- rep(0,nrow(myAtmbc)) # counter for water mass balance
r_landuse_irr <- r_landuse[myrow:1,]
matirr <- r_landuse_irr
# matirr[matirr!=1] <- 0
matirr[,] <- 0

pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1)
for (ii in 1:nrow(myAtmbc)) {
  setTxtProgressBar(pb,ii)
  myNode_atmbc <- matrix(0, nrow=1, ncol=nrow(xyz_p)) # use 0 because it is possibly a sum

  # which position of KC array we are now
  myAtm_month <- paste0(substr(myAtmbc$Date[ii],9,10), substr(myAtmbc$Date[ii],4,5), sep="")
  jkc <- which(namemonth == myAtm_month)
  nowkc <- mykc[myrow:1,,jkc]

  # matirrnow <- matirr*myAtmbc$Irrgreen_m3s[ii]
  matirrnow <- matirr
  matirrnow[r_landuse_irr==3] <- nowkc[r_landuse_irr==3]*myAtmbc$Irr_m3s[ii]
  matirrnow[r_landuse_irr==4] <- nowkc[r_landuse_irr==4]*myAtmbc$Irr_m3s[ii]


  # if (ii %in% iprint) {
  #   myNode_atmbc_plot <- as.data.frame(t(myNode_atmbc))
  #   myNode_atmbc_plot <- cbind(myNode_atmbc_plot , xyz_p)
  #   names(myNode_atmbc_plot)[1] <- "atmbc"
  #   # myNode_atmbc_plot$atmbc[myNode_atmbc_plot$atmbc==0] <- NA
  #   p <- ggplot(myNode_atmbc_plot,
  #               mapping=aes(V2, V3)) +
  #     # geom_tile(aes(fill = sign(atmbc)*abs(log10(abs(atmbc))))) +
  #     geom_tile(aes(fill = atmbc*1e3*dt_weather)) +
  #     theme_bw() +
  #     scale_fill_gradient2(na.value = "black") + #   low = muted("red"),  mid = "white",  high = muted("blue"),  midpoint = 0,  space = "Lab",  na.value = "grey50",
  #     xlab("Easting") +
  #     ylab("Northing") +
  #     # labs(fill = "sign(log10(Atmbc)) (m/s)")
  #     labs(fill = "Irr (mm/h)")
  #   print(
  #     p
  #   )
  #   ggsave(paste0(nameout, "_", namelabel[contp], ".jpg", sep=""), width = 12, height = 8, units = 'in', dpi=300)
  #   contp <- contp + 1
  # }

  sumwater[ii] <- sumwater[ii] + sum(matirrnow, na.rm=TRUE)

  write.table(myAtmbc$Date_pos[ii]-mindatepos + addndays*nrepwarmup,
              paste0("other-dem/irrigation_map", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(matirrnow,
              paste0("other-dem/irrigation_map", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

}
close(pb)
myAtmbc <- myAtmbc_irr
rm(myAtmbc_irr)

###############################################################################################################






r_landuse <- r_landuse_copy
rm(r_landuse_copy)
print(sum(sumwater*dt_weather))
tiff( paste0(nameoutplot, ".tiff", sep=""), width = 8, height = 6, units = 'in', res=100)
# par(mar=c(1, 1, 1, 1), mfrow=c(1,1), mgp=c(0,0.8,0))
par(mar=c(4,4,1,1), mfrow=c(1,1))

plot(sumwater, col="dodgerblue", type="l", lwd=3, cex.axis=1.8, cex.lab=1.8) # , ylab="", yaxt = "n"

dev.off()



################################################################################
# warmupdays
# vtk <- c(    0,
#              5230800,
#              5317200,
#              5403600,
#              5490000,
#              5576400,
#              5662800,
#              5749200,
#              5835600,
#              5922000,
#              addndays*nrepwarmup + max(myAtmbc$Date_pos) - min(myAtmbc$Date_pos))

# max Rain, then 10 h later
iprint <- myAtmbc$Date_pos[which.max(myAtmbc$Rain_m3s)] - min(myAtmbc$Date_pos) + addndays*nrepwarmup
iprint <- iprint + 3600 * seq(0,9,length.out=10)
vtk <- c(0, iprint)
# rainfall tht can infiltrate # Kz= 3.836878e-07 m3/s
# iprint <- myAtmbc$Date_pos[which.min(myAtmbc$Rain_m3s < 3.836878e-07)] - min(myAtmbc$Date_pos) + addndays*nrepwarmup
# j <- which(myAtmbc$Date_pos > min(myAtmbc$Date_pos) + warmupdays*86400)
iprint <- head(which(myAtmbc$Date_pos > min(myAtmbc$Date_pos) + warmupdays*86400 & myAtmbc$Rain_m3s < 3.836878e-07  & myAtmbc$Rain_m3s > 0) ,1 )
iprint <- iprint + 3600 * seq(0,9,length.out=10)
vtk <- c(vtk , iprint)
# max Irr open field, then 5 h later
iprint <- myAtmbc$Date_pos[which.max(myAtmbc$Irr_m3s)] - min(myAtmbc$Date_pos) + addndays*nrepwarmup
iprint <- iprint + 3600 * seq(0,4,length.out=5)
vtk <- c(vtk , iprint)
# max ET0 open field, then 5 h later
iprint <- myAtmbc$Date_pos[which.max(myAtmbc$ET0_m3s)] - min(myAtmbc$Date_pos) + addndays*nrepwarmup
iprint <- iprint + 3600 * seq(0,4,length.out=5)
vtk <- c(vtk , iprint)
#
vtk <- c(vtk , addndays*nrepwarmup + max(myAtmbc$Date_pos) - min(myAtmbc$Date_pos))
vtk <- sort(vtk)

if (exists("vtk")) {
  a <- vtk[2:(length(vtk)-1)]
  a <- as.data.frame(a)
  print(paste0("removing first and last, length vtk is:", nrow(a), sep=""))
} else {
  # let it run for nst days before saving
  nst <- 10
  ldays <- 48
  # to keep 287 vtk, ldays&secs, 3&900, 6&1800, 9&2700, 12&3600, 24&7200, 48&4*3600, 72&21600
  a <- seq(addndays*nrepwarmup + nst*86400 , (addndays*nrepwarmup + nst*86400 + ldays*86400), by=4*3600)
  a <- as.data.frame(a)
  print(paste0("removing first and last, length vtk is:", nrow(a)-2, sep=""))
}

write.table(a,
            paste0("input/parmtime_DLC.txt", sep=""),
            append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
print(paste0("tend in parm is:", addndays*nrepwarmup + max(myAtmbc$Date_pos) - min(myAtmbc$Date_pos), sep=""))


