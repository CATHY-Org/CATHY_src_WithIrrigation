
Sys.setlocale("LC_TIME", "English")

rm(list=ls());
#.rs.restartR()

library(dplyr)
library(ggplot2)
library(reshape2)
library(raster)
# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3


namefolder<- paste0("Salerno_20250131_ROSETTA_dirnew_mars_wobaseflow_Kzexp_hap_t6", sep="")
setwd(paste0("C:/UNIPD/CATHY/cathy2_0/REWATERING/", namefolder, sep=""))

date_st <- "01.06.2023"
date_en <- "31.01.2025"

dobaseflow <- 0 # remove variable so that it is not used, otherwise use a number
DOsetbasefl <- 0
dobaseflowlinear <- 0
DO3zone <- 0

myrow <- 72
mycol <- 74
dx <- 80
dy <- 80

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







################################################################################
### Weather data with water level measured
nameMETEOin <- paste0("input/Shiny_Rain_BC_warmup", ".csv", sep="")
tmp <- read.csv(nameMETEOin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

nameMETEOin <- paste0("input/Shiny_Rain_BC", ".csv", sep="")
myRain <- read.csv(nameMETEOin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

myRain <- rbind(tmp,myRain)

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
j <- which(is.na(myRain$Rain_mm))
if (length(j)>0) {
  myRain$Rain_mm[j] <- 0
  print("0s in Rain")
}






################################################################################
nameET0in <- "input/Shiny_ET0_BC_warmup.csv"
tmp <- read.csv(nameET0in, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
nameET0in <- "input/Shiny_ET0_BC.csv"
ET0 <- read.csv(nameET0in, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
ET0 <- rbind(tmp,ET0)

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
j <- which(is.na(ET0$ET0))

################################################################################
nameET0green <- "input/Shiny_ET0_BC_climatemodel_warmup.csv"
tmp <- read.csv(nameET0green, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
nameET0green <- "input/Shiny_ET0_BC_climatemodel.csv"
ET0green <- read.csv(nameET0green, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
ET0green <- rbind(tmp,ET0green)

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
j <- which(is.na(ET0green$ET))

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
#
j <- head(which(substr(ET0green$Date,12,13) == "00") , 1)
if (j > 1) {
  print("remove first DOY ET if not 24 observations")
  ET0green <- ET0green[j:nrow(ET0green),]
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

################################################################################
ET0green_plot <- ET0green

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


nighttime <- c("00","01",  "02",  "03",  "04",  "05",  "20", "21", "22", "23")
j <- which(substr(myRain$Date,12,13) %in% nighttime)
myRain$ET0[j] <- 0
myRain$ET0green[j] <- 0
ET0green_plot$ET[j] <- 0

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





# j <- 1:5e5
# plot(myRain$Date_pos[j],myRain$Irr[j], type="l")

################################################################################
myAtmbc <- myRain # this is for every cell
j <- which(is.na(myRain$Rain_mm))
j <- which(is.na(myRain$ET0green))

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
abline(a=-2.5e4,b=51539.23/23740.96/86400) # to fix, with fitting initial years for slopes, to see trend


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

# cell2nod <- function(rmap,rain,et0,irr,et0_green,irr_green,frac_GH,nowkc) {
#   switch(as.character(rmap),
#          "0" = { # outside domain. imagine rain is the same
#            y <- rain
#          },
#          "1" = { # greenhouse
#            #TODO
#            # solarization accounted for setting et = 0, otherwise kc=1
#            y <- frac_GH*rain - et0_green * 1 # "coupled with neumann in 2nd node" irr_greenhouse
#            # y <- - et0_green # put rain in neigbor cell without greenhouse
#          },
#          "2" = { # storage vehicles
#            #TODO
#            y <- rain # rain should go somewhere
#          },
#          "3" = { # net
#            #TODO
#            y <- rain - et0 * nowkc # irr_greenhouse  - eto_greenhouse
#          },
#          "4" = { # crop
#            #TODO
#            y <- rain - et0 * nowkc
#          },
#          "5" = { # crop
#            #TODO
#            y <- rain
#          },
#          "6" = { # road
#            #TODO
#            y <- rain
#          }
#   )
#   y <- y / 4 # 4 nodes per cell
#   return(y)
# }


# kc dynamic
# namekc <- paste0("OPAC/map_kc_", gsub("0","",substr(myAtmbc$Date[ii],4,5)), ".csv", sep="")
namekc_all <- list.files("OPAC/", pattern = "\\.csv$")
j1 <- which(namekc_all == "map_kc_2306.csv")
j2 <- which(namekc_all == "map_kc_2501.csv")
namekc_all <- namekc_all[j1:j2]
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

#
# KC mean for open field conditions
mykc_plot <- as.data.frame(matrix(0,nrow=length(namekc_all),ncol=2))
j <- which(r_landuse == c(3,4), arr.ind=T) # to select open fields
jall <- nrow(j)
j <- which(r_landuse != c(3,4), arr.ind=T) # to select open fields
for (i in 1:length(namekc_all)) {

  tmp <- mykc[,,i]
  tmp[j] <- 0

  mykc_plot$V1[i] <- namemonth[i]
  mykc_plot$V2[i] <- sum(tmp) / jall

}
names(mykc_plot) <- c("namemonth","Kc")

mykc_plot$Date <- paste0("01.", substr(mykc_plot$namemonth,3,4), ".20", substr(mykc_plot$namemonth,1,2), " 00:00", sep="")
mykc_plot$Date_pos_R <- as.POSIXct(strptime(mykc_plot$Date, format="%d.%m.%Y %H:%M",tz="UTC"))

# tiff( "Kc_OpenField.tiff", width = 8, height = 6, units = 'in', res=300)
# par(mar=c(4,5,1,2), mfrow=c(1,1))
# plot(mykc_plot$Date_pos_R, mykc_plot$Kc, col="black", type="l", lwd=3, cex.axis=1.8, cex.lab=1.8, ylim=c(0, 1), xlab="Date", ylab=expression(paste("Monthly K"[C] *" (-)", sep=""))) #, yaxt = "n"
# text(mykc_plot$Date_pos_R[nrow(mykc_plot)] + 0*86400, y = 50, labels = expression(bold("b")), cex=1.8)
# dev.off()

myRain_plot <- myRain
names(myRain_plot)[which(names(myRain_plot) == "ET0green")] <- "ET0g_solar"
myRain_plot$ET0g <- ET0green_plot$ET
myRain_plot$namemonth <- paste0(substr(myRain_plot$Date,9,10),substr(myRain_plot$Date,4,5),sep="")
ttmp <- mykc_plot %>%
  dplyr::select("Kc","namemonth")
myRain_plot <- left_join(myRain_plot,ttmp, by=c("namemonth"))
rm(ttmp)
myRain_plot$ET0reference <- myRain_plot$ET0 * myRain_plot$Kc
myRain_plot$ETCsummer <- myRain_plot$ET0; j <- which(substr(myRain_plot$Date,4,5) %in% c("11","12","01","02","03","04")); myRain_plot$ETCsummer[j] <- 0
myRain_plot$ETCwinter <- myRain_plot$ET0; j <- which(substr(myRain_plot$Date,4,5) %in% c("05","06","07","08","09","10")); myRain_plot$ETCwinter[j] <- 0
myRain_plot$Date_pos_R <- as.POSIXct(strptime(myRain_plot$Date, format="%d.%m.%Y %H:%M",tz="UTC"))

j <- which(myRain_plot$Date %in% c("01.06.2023 00:09",
                                   "01.07.2023 00:09",
                                   "01.08.2023 00:09",
                                   "01.09.2023 00:09",
                                   "01.10.2023 00:09",
                                   "01.11.2023 00:09",
                                   "01.12.2023 00:09",
                                   "01.01.2024 00:09",
                                   "01.02.2024 00:09",
                                   "01.03.2024 00:09",
                                   "01.04.2024 00:09",
                                   "01.05.2024 00:09",
                                   "01.06.2024 00:09",
                                   "01.07.2024 00:09",
                                   "01.08.2024 00:09",
                                   "01.09.2024 00:09",
                                   "01.10.2024 00:09",
                                   "01.11.2024 00:09",
                                   "01.12.2024 00:09",
                                   "01.01.2025 00:09"))
myx <- myRain_plot$Date_pos_R[j]
myx_lab <- as.character(myx)
myx_lab <- substr(myx_lab,1,7)

myx <- c(myx , myRain_plot$Date_pos_R[nrow(myRain_plot)])
myx_lab <- c(myx_lab , "2025-02")

tiff( "Rain_ET0open_ET0greenh.tiff", width = 8, height = 6, units = 'in', res=300)
par(mar=c(8,5,1,1), mfrow=c(1,1))
plot(myRain_plot$Date_pos_R, cumsum(myRain_plot$Rain_mm), col="black", type="l", lwd=3, cex.axis=1.8, cex.lab=1.8, xaxt="n", ylim=c(0, 2000), xlab="", ylab="Cumulative depth (mm)") #, yaxt = "n"
lines(myRain_plot$Date_pos_R, cumsum(myRain_plot$ET0), col="forestgreen", type="l", lwd=3) # , ylab="", yaxt = "n"
lines(myRain_plot$Date_pos_R, cumsum(myRain_plot$ET0g), col="dodgerblue", type="l", lwd=3) # , ylab="", yaxt = "n"
axis(side=1,at=myx,labels=myx_lab, las=2, cex.axis=1.8)
mtext("Date", side=1, line=7, cex = 1.8)
legend(x=myRain_plot$Date_pos_R[1]-86400*20,2200, legend=c("Rain",
                           expression(paste("ET"[0] *"" ^ "OF", sep="")),
                           expression(paste("ET"[0] *"" ^ "G", sep=""))),
       col=c("black", "forestgreen","dodgerblue"), lty=1, lwd=3, cex=2, box.lty=0, bg=NULL, seg.len=1, horiz=F,
       y.intersp=1.4)
text(myRain_plot$Date_pos_R[nrow(myRain_plot)] + 0*86400, y = 50, labels = expression(bold("a")), cex=1.8)
dev.off()

tiff( "Rain_ETCopen_ETCgreenh.tiff", width = 8, height = 6, units = 'in', res=300)
par(mar=c(8,5,1,1), mfrow=c(1,1))
plot(myRain_plot$Date_pos_R, cumsum(myRain_plot$Rain_mm), col="black", type="l", lwd=3, cex.axis=1.8, cex.lab=1.8, xaxt="n", ylim=c(0, 2000), xlab="", ylab="Cumulative depth (mm)") #, yaxt = "n"
lines(myRain_plot$Date_pos_R, cumsum(myRain_plot$ET0reference), col="forestgreen", type="l", lwd=3) # , ylab="", yaxt = "n"
lines(myRain_plot$Date_pos_R, cumsum(myRain_plot$ET0g_solar), col="dodgerblue", type="l", lwd=3) # , ylab="", yaxt = "n"
lines(myRain_plot$Date_pos_R, cumsum(myRain_plot$ETCwinter), col="gold", type="l", lwd=3) # , ylab="", yaxt = "n"
lines(myRain_plot$Date_pos_R, cumsum(myRain_plot$ETCsummer), col="darkred", type="l", lwd=3) # , ylab="", yaxt = "n"
axis(side=1,at=myx,labels=myx_lab, las=2, cex.axis=1.8)
mtext("Date", side=1, line=7, cex = 1.8)
legend(x=myRain_plot$Date_pos_R[1]-86400*20,2200, legend=c("Rain",
                           expression(paste("ET"[C] *"" ^ "OF", sep="")),
                           expression(paste("ET"[C] *"" ^ "G", sep="")),
                           expression(paste("ET"[C] *"" ^ "OF-winter", sep="")),
                           expression(paste("ET"[C] *"" ^ "OF-summer", sep=""))),
       col=c("black", "forestgreen","dodgerblue","gold","darkred"), lty=1, lwd=3, cex=2, box.lty=0, bg=NULL, seg.len=1, horiz=F,
       y.intersp=1.4)
text(myRain_plot$Date_pos_R[nrow(myRain_plot)] + 0*86400, y = 50, labels = expression(bold("c")), cex=1.8)
dev.off()



mykc_plot_copy <- mykc_plot
mykc_plot$Kc_summer <- c(1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0)
mykc_plot$Kc_winter <- c(0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1)
mykc_plot$Kc_greenhouse <- c(0,0,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1)
jj <- nrow(mykc_plot)
tiff( "Kc_OpenField.tiff", width = 8, height = 6, units = 'in', res=300)
par(mar=c(8,5,1,1), mfrow=c(1,1))
#plot(mykc_plot$Date_pos_R, mykc_plot$Kc, col="forestgreen", type="s", lwd=3, cex.axis=1.8, cex.lab=1.8, xaxt="n", xlim=c(myRain_plot$Date_pos_R[1],myRain_plot$Date_pos_R[nrow(myRain_plot)]), ylim=c(0, 1), xlab="", ylab=expression(paste("Monthly K"[C] *" (-)", sep=""))) #, yaxt = "n"
plot(mykc_plot$Date_pos_R, mykc_plot$Kc, col="forestgreen", type="s", lwd=3, cex.axis=1.8, cex.lab=1.8, xaxt="n", xlim=c(myRain_plot$Date_pos_R[1],myRain_plot$Date_pos_R[nrow(myRain_plot)]), ylim=c(0, 1), xlab="", ylab=expression(paste("K"[C] *"" ^ "OF", sep="")),) #, yaxt = "n"
lines(c(mykc_plot$Date_pos_R[jj],mykc_plot$Date_pos_R[jj] + 86400*31), c(mykc_plot$Kc[jj],mykc_plot$Kc[jj]), col="forestgreen", type="l", lwd=3)
# lines(mykc_plot$Date_pos_R, mykc_plot$Kc_greenhouse, col="dodgerblue", type="s", lwd=3)
# lines(c(mykc_plot$Date_pos_R[jj],mykc_plot$Date_pos_R[jj] + 86400*31), c(mykc_plot$Kc_greenhouse[jj],mykc_plot$Kc_greenhouse[jj]), col="dodgerblue", type="l", lwd=3)
# lines(mykc_plot$Date_pos_R, mykc_plot$Kc_winter, col="gold", type="s", lwd=3)
# lines(c(mykc_plot$Date_pos_R[jj],mykc_plot$Date_pos_R[jj] + 86400*31), c(mykc_plot$Kc_winter[jj],mykc_plot$Kc_winter[jj]), col="gold", type="l", lwd=3)
# lines(mykc_plot$Date_pos_R, mykc_plot$Kc_summer, col="darkred", type="s", lwd=3)
# lines(c(mykc_plot$Date_pos_R[jj],mykc_plot$Date_pos_R[jj] + 86400*31), c(mykc_plot$Kc_summer[jj],mykc_plot$Kc_summer[jj]), col="darkred", type="l", lwd=3)
axis(side=1,at=myx,labels=myx_lab, las=2, cex.axis=1.8)
mtext("Date", side=1, line=7, cex = 1.8)
text(mykc_plot$Date_pos_R[nrow(mykc_plot)] + 30*86400, y = 0.03, labels = expression(bold("b")), cex=1.8)
dev.off()
mykc_plot <- mykc_plot_copy
rm(mykc_plot_copy,jj)

sum(myRain_plot$ET0)
sum(myRain_plot$ET0g)

sum(myRain_plot$ET0reference)
sum(myRain_plot$ET0g_solar)

sum(myRain_plot$ETCwinter)
sum(myRain_plot$ETCsummer)

rm(myRain_plot, ET0green_plot, mykc_plot)
###
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

#
addndays <- 0 * 86400 # in seconds
nrepwarmup <- 1 # how many times to repeat the warm up?
stepndays <- addndays * nrepwarmup / dt_weather
# jfactor_warmup <- c(0.1, -0.1) # decrease irrigation
jfactor_warmup <- c(1, 1) # modify atmbc, vector of same length of nrepwarmup or longer

# # so that it looks like r_landuse plotted in geom_tile, with northing increasing bottom up and easting left right
# mymap <- r_landuse_copy
# rownames(mymap) <- nrow(r_landuse_copy):1
# names(mymap) <- 1:ncol(r_landuse_copy)

mindatepos <- min(myAtmbc$Date_pos)
# YYreplace1 <- substr(myAtmbc$Date[1],9,10)


lMY <- length(unique(substr(myRain$YMD,4,10)))
myKC <- matrix(0, nrow=lMY, ncol=nrow(id_map)) # landuse
for (i in 1:nrow(id_map)) {
  cell_nrow <- id_map$row[i]
  cell_ncol <- id_map$col[i]

  # find the 4 nodes
  not <- cell_nrow # node top
  nob <- cell_nrow - 1
  nol <- cell_ncol - 1
  nor <- cell_ncol

  for (ii in 1:lMY) {
    myKC[ii,i] <- mykc[cell_nrow,cell_ncol, ii]
  }
}

# avoid second for loop
myID <- matrix(0, nrow=4, ncol=nrow(id_map)) # ID
myLU <- matrix(0, nrow=1, ncol=nrow(id_map)) # landuse

#transform i for id_map to class, which are just 6
for (i in 1:nrow(id_map)) {
  # find cell
  cell_nrow <- id_map$row[i]
  cell_ncol <- id_map$col[i]

  # find the 4 nodes
  not <- cell_nrow # node top
  nob <- cell_nrow - 1
  nol <- cell_ncol - 1
  nor <- cell_ncol

  myLU[1,i] <- r_landuse[cell_nrow,cell_ncol]

  myID[1,i] <- which(xyz_p$V2 == nol & xyz_p$V3 == not)
  myID[2,i] <- which(xyz_p$V2 == nol & xyz_p$V3 == nob)
  myID[3,i] <- which(xyz_p$V2 == nor & xyz_p$V3 == not)
  myID[4,i] <- which(xyz_p$V2 == nor & xyz_p$V3 == nob)

}

myLUc <- sort(unique(myLU[1,]))
myLUl <- length(myLUc)

j1 <- which(myLU[1,] == myLUc[1])
j2 <- which(myLU[1,] == myLUc[2])
j3 <- which(myLU[1,] == myLUc[3])
j4 <- which(myLU[1,] == myLUc[4])
j5 <- which(myLU[1,] == myLUc[5])
j6 <- which(myLU[1,] == myLUc[6])


myAtmbc_copy <- myAtmbc
jdel <- NULL
zerobefore <- 1
pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1)
for (ii in 2:(nrow(myAtmbc)-1)) {
  setTxtProgressBar(pb,ii)
  # if ((myAtmbc$Irr_m3s[ii]+myAtmbc$Irrgreen_m3s[ii]) == 0) {
  if (sum(myAtmbc[ii,c(9,10,11)]) == 0) {
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
myAtmbc <- myAtmbc[-jdel,]
rm(jdel)


pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1)
for (ii in 1:nrow(myAtmbc)) {
  setTxtProgressBar(pb,ii)
  myNode_atmbc <- matrix(0, nrow=1, ncol=nrow(xyz_p)) # use 0 because it is possibly a sum

  # which position of KC array we are now
  myAtm_month <- paste0(substr(myAtmbc$Date[ii],9,10), substr(myAtmbc$Date[ii],4,5), sep="")
  # substr(myAtm_month,1,2) <- "24"
  jkc <- which(namemonth == myAtm_month)

  rain <- myAtmbc$Rain_m3s[ii]
  et0 <- myAtmbc$ET0_m3s[ii]
  et0green <- myAtmbc$ET0green_m3s[ii]

  # sort of remove second for loop
  # LU all
  myNode_atmbc[1,] <- rain

  # LU 1
  myNode_atmbc[1,j1] <- myNode_atmbc[1,j1] - et0green

  # LU 3 e 4
  myNode_atmbc[1,c(j3,j4)] <- myNode_atmbc[1,c(j3,j4)] - et0*myKC[jkc,c(j3,j4)]

  write.table(myAtmbc$Date_pos[ii]-mindatepos + addndays*nrepwarmup,
              nameout,
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(myNode_atmbc,
              nameout,
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
}
close(pb)















myAtmbc <- myAtmbc_copy
################################################################################################################
# irrigation
myAtmbc_irr <- myAtmbc

# boundary conditions do not need to have a flux every hour but only when need to start and end.
# so skip first 0.
# then find fluxes =! 0 and put a 0 when they end
jdel <- NULL
zerobefore <- 1
pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1)
for (ii in 2:(nrow(myAtmbc)-1)) {
  setTxtProgressBar(pb,ii)
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
close(pb)
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


#
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
  # substr(myAtm_month,1,2) <- "24"
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

max(myAtmbc$Date_pos) - min(myAtmbc$Date_pos)
#
#
#
# ################################################################################
# # warmupdays
# # vtk <- c(    0,
# #              5230800,
# #              5317200,
# #              5403600,
# #              5490000,
# #              5576400,
# #              5662800,
# #              5749200,
# #              5835600,
# #              5922000,
# #              addndays*nrepwarmup + max(myAtmbc$Date_pos) - min(myAtmbc$Date_pos))
#
# # max Rain, then 10 h later
# iprint <- myAtmbc$Date_pos[which.max(myAtmbc$Rain_m3s)] - min(myAtmbc$Date_pos) + addndays*nrepwarmup
# iprint <- iprint + 3600 * seq(0,9,length.out=10)
# vtk <- c(0, iprint)
# # rainfall tht can infiltrate # Kz= 3.836878e-07 m3/s
# # iprint <- myAtmbc$Date_pos[which.min(myAtmbc$Rain_m3s < 3.836878e-07)] - min(myAtmbc$Date_pos) + addndays*nrepwarmup
# # j <- which(myAtmbc$Date_pos > min(myAtmbc$Date_pos) + warmupdays*86400)
# iprint <- head(which(myAtmbc$Date_pos > min(myAtmbc$Date_pos) + warmupdays*86400 & myAtmbc$Rain_m3s < 3.836878e-07  & myAtmbc$Rain_m3s > 0) ,1 )
# iprint <- iprint + 3600 * seq(0,9,length.out=10)
# vtk <- c(vtk , iprint)
# # max Irr open field, then 5 h later
# iprint <- myAtmbc$Date_pos[which.max(myAtmbc$Irr_m3s)] - min(myAtmbc$Date_pos) + addndays*nrepwarmup
# iprint <- iprint + 3600 * seq(0,4,length.out=5)
# vtk <- c(vtk , iprint)
# # max ET0 open field, then 5 h later
# iprint <- myAtmbc$Date_pos[which.max(myAtmbc$ET0_m3s)] - min(myAtmbc$Date_pos) + addndays*nrepwarmup
# iprint <- iprint + 3600 * seq(0,4,length.out=5)
# vtk <- c(vtk , iprint)
# #
# vtk <- c(vtk , addndays*nrepwarmup + max(myAtmbc$Date_pos) - min(myAtmbc$Date_pos))
# vtk <- sort(vtk)
#
# if (exists("vtk")) {
#   a <- vtk[2:(length(vtk)-1)]
#   a <- as.data.frame(a)
#   print(paste0("removing first and last, length vtk is:", nrow(a), sep=""))
# } else {
#   # let it run for nst days before saving
#   nst <- 10
#   ldays <- 48
#   # to keep 287 vtk, ldays&secs, 3&900, 6&1800, 9&2700, 12&3600, 24&7200, 48&4*3600, 72&21600
#   a <- seq(addndays*nrepwarmup + nst*86400 , (addndays*nrepwarmup + nst*86400 + ldays*86400), by=4*3600)
#   a <- as.data.frame(a)
#   print(paste0("removing first and last, length vtk is:", nrow(a)-2, sep=""))
# }
#
# write.table(a,
#             paste0("input/parmtime_DLC.txt", sep=""),
#             append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# print(paste0("tend in parm is:", addndays*nrepwarmup + max(myAtmbc$Date_pos) - min(myAtmbc$Date_pos), sep=""))
#

