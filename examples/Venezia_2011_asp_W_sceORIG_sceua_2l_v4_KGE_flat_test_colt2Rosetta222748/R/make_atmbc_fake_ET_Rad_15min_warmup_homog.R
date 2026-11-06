# remove irr because all done as subirr
# but leave warmup to be consistent with original calibration
# to test whether this approach can be used in CATHY.
# top soil set impervious

library(dplyr)
library(ggplot2)
# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3

rm(list=ls());
# .rs.restartR()
setwd("C:/CATHY/cathy1_0/runs/Calibration/Venezia_2011_asp_W_sceORIG_sceua_2l_v4_KGE_flat_test_colt2")

tau <- 0.7

# enddate <- ""
enddate <- "14.12.2010 23:45"

# plot(diff(mydf$Date_pos))
# which(diff(mydf$Date_pos)!=900)
datedel <- "14.10.2010 15:20"
# datedel <- ""

# circa 1 m2 for scaling
myrow <- 10
mycol <- 13
dx <- 0.1
dy <- 0.1

# substituting NA from rain with 0 mm
# working at hour at the moment for speed

dt_weather <- 15*60 # 15 minutes in seconds
dt_weather <- 60*60 # going to 1 hour
dt_weather <- 15*60 # going to 1 hour

namelanduse <- "input/root_map" # postprocessed to match DTM active cells
namexyz <- "output/xyz"

r_landuse <- read.table(namelanduse, header=FALSE, sep=" ", dec = ".", skip=6)
if (is.na(r_landuse[1,1])) {r_landuse <- r_landuse[,-1]}
if (nrow(r_landuse) != myrow | (ncol(r_landuse) != mycol)) {stop("row or col do not match")}

xyz_header <- read.table(namexyz,
                         head = FALSE,
                         nrows = 1)
NNOD <- xyz_header[1,1]
N <- xyz_header[1,2]

xyz <- read.table(namexyz, header=FALSE, sep="", dec = ".", skip=1)
xyz_p <- xyz[1:NNOD,]
xyz_p$V2 <- (xyz_p$V2 + 0) /dx
xyz_p$V3 <- (xyz_p$V3 + 0) /dy




###
# temperature
nameMETEOin <- "../../Venezia_2011/rdatireport20092010/4gam2_meteo_soil.csv"
myMETEO_SOIL <- read.csv(nameMETEOin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

myMETEO_SOIL$Date <- paste0(myMETEO_SOIL$giorno_irr, " ", myMETEO_SOIL$orario_irr, sep="")
myMETEO_SOIL$Date_pos <- as.numeric(as.POSIXct(strptime(myMETEO_SOIL$Date, format="%d.%m.%Y %H:%M"), origin="1970-01-01", tz="CET"))
myMETEO_SOIL$Date_pos_R <- as.POSIXct(strptime(myMETEO_SOIL$Date, format="%d.%m.%Y %H:%M"), origin="1970-01-01", tz="CET")

myMETEO_SOIL <- myMETEO_SOIL %>%
  dplyr::select(Date,Date_pos,Date_pos_R,coltura,irrigazione,T_aria,RH_perc)

myMETEO_SOIL <- myMETEO_SOIL %>%
  dplyr::filter(coltura==2 & irrigazione=="asp")

myMETEO_SOIL$DOY <- as.numeric(strftime(
  as.POSIXct(myMETEO_SOIL$Date, format="%d.%m.%Y %H:%M", origin="1970-01-01", tz="CET"),
  format = "%j", tz="CET")) # doy

myMETEO_SOIL$hour <- as.numeric(substr(myMETEO_SOIL$Date,12,13))

names(myMETEO_SOIL)[which(names(myMETEO_SOIL)=="T_aria")] <- "Tmean_C"
names(myMETEO_SOIL)[which(names(myMETEO_SOIL)=="RH_perc")] <- "RH"

# myMETEO_SOIL <- myMETEO_SOIL %>%
#   dplyr::group_by(DOY,hour) %>%
#   dplyr::mutate(Tmean_C = mean(Tmean_C, na.rm=TRUE),
#                 RH_in = mean(RH, na.rm=TRUE)) %>%
#   dplyr::slice(1) %>%
#   dplyr::ungroup() %>%
#   dplyr::arrange(Date_pos)

mydf <- myMETEO_SOIL
rm(myMETEO_SOIL)

if (nchar(enddate)>0) {
  j <- which(mydf$Date == enddate)
  mydf <- mydf[1:j,]
  rm(j)
}
rm(enddate)

if (nchar(datedel)>0) {
  j <- which(mydf$Date == datedel)
  if (length(j) > 0) {
    mydf <- mydf[-j,]
  }
  rm(j)
}
rm(datedel)

mydf <- mydf %>%
  dplyr::select(-c("coltura","irrigazione"))

# replace NA
jna <- which(is.na(mydf$Tmean_C))
for (i in 1:length(jna)) { # because if in sequence, copy earlier just replaced
  mydf$Tmean_C[jna[i]] <- mydf$Tmean_C[jna[i]-1]
  mydf$RH[jna[i]] <- mydf$RH[jna[i]-1]
}
##############################################################
myAtmbc <- mydf

# mydf_copy <- mydf

mydf <- mydf %>%
  dplyr::mutate(dummy = substr(Date,1,13)) %>%
  dplyr::group_by(dummy) %>%
  dplyr::mutate(Tmean_C = mean(Tmean_C),
               RH = mean(RH)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Date_pos) %>%
  dplyr::select(-dummy)

##############################################################
# watchcout that ET is calculated hourly. but input data are 15 minutes!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
###
# global radiance
nameRADout <- "../../Venezia_2011/Battipaglia_Rad_Venezia_2011.csv"
myRAD <- read.csv(nameRADout, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
names(myRAD)[which(names(myRAD) == "Data")] <- "Date"
names(myRAD)[which(names(myRAD) == "BATTIPAGLIA...Rad.Solare.ist...Val..W.m2.")] <- "RadGlo_Wm2"
myRAD$RadGlo_Wm2 <- as.numeric(myRAD$RadGlo_Wm2)

myRAD$Date <- substr(myRAD$Date, 1, 16)
yy <- substr(myRAD$Date, 1, 4)
mm <- substr(myRAD$Date, 6, 7)
dd <- substr(myRAD$Date, 9, 10)
hhmm <- substr(myRAD$Date, 11, 16)
myRAD$Date <- paste0(dd, ".", mm, ".", yy, hhmm, sep="")

# measurements are hourly but sometimes there are times every 10 minutes, maybe for other variables
j <- which(substr(myRAD$Date,15,16) == "00")
myRAD <- myRAD[j,]

mydf <- left_join(mydf , myRAD, by=c("Date"))

###################################################################################################################

# instead of na, use mean all over period
Radmean <- mydf %>%
  dplyr::group_by(hour) %>%
  dplyr::summarise(RadGlo_Wm2 = round(mean(RadGlo_Wm2, na.rm=TRUE), digits=1)) %>%
  dplyr::ungroup()

# duplicate n times dataframe
Radmean <- do.call("rbind", replicate(nrow(mydf)/nrow(Radmean), Radmean, simplify = FALSE))

# replace na with hourly mean
j <- which(is.na(mydf$RadGlo_Wm2))
mydf$RadGlo_Wm2[j] <- Radmean$RadGlo_Wm2[j]

###############################################################

mydf$RadGlo_Wm2 <- mydf$RadGlo_Wm2 * tau # transmissivity of incoming global solar radiation between 40% and 50%

###
mydf$WindSp_ms <- 0.1 # info from Fernandez 2010, it fluctuates between 0.1 and 0.3 m/s

###
mydf$Date <- format(as.POSIXct(mydf$Date_pos, origin="1970-01-01"), format="%Y-%m-%d %H:%M:%S")

# add these to be consistent with the script to source and calculate a summary for the day
mydf$Rain_mm <- 0
# ET0 greenhouse
# FAO56
###
source("C:/REWATERING/Rain/data_h/fao_Snyder_greenhouse_DLC_R_Rain_source.R")

#
# write.table(mydf, file="../Venezia_2011/rdatireport20092010/4gam2_meteo_soil_ET_asp.csv", dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)

#
myAtmbc$ET0 <- rep(mydf$ET0green/4, each=4) # groing from 60 minutes to 15 mminutes




# myAtmbc$ET0 <- runif(nrow(myAtmbc)) ##############################################################
# calendar Kc
# do the cuts decrease ET, or then soil transpiration predominate?
jtime <- c("14.10.2010 00:00", "21.10.2010 00:00",#after seeding
           "21.10.2010 00:15", "28.10.2010 00:00",#growing
           "28.10.2010 00:15", "14.12.2010 23:45"# grown
)
jirr <- c(0, 0,
          0, 1,
          1, 1
)

myk <- rep(0, nrow(myAtmbc))
for (i in seq(1, length(jtime), 2)) {
  j1 <- which(myAtmbc$Date == jtime[i])
  j2 <- which(myAtmbc$Date == jtime[i+1])
  myk[j1:j2] <- seq(jirr[i], jirr[i+1], length.out=j2-j1+1)
}
myAtmbc$ET0 <- myAtmbc$ET0 * myk
rm(myk)

##############################################################
# Irr match ET
# j <- which(myAtmbc$id %in% c(round(seq(1,40,length.out=10))) & myAtmbc$hour == 10)
# myAtmbc$Irr[j] <- sum(myAtmbc$ET0 / length(j))

# Irr given
myAtmbc$Irr <- 0
# given the plot, there is a 1 hour lag between irrigation schedule and the sensor measurements
# so the times here are scheduled times minus 1 hour
jtime <- c("14.10.2010 16:00", "14.10.2010 16:15", "14.10.2010 16:30", "14.10.2010 16:45",
           "15.10.2010 10:00", "15.10.2010 10:15", "15.10.2010 10:30", "15.10.2010 10:45",
           "16.10.2010 09:45", "16.10.2010 10:00",
           "17.10.2010 10:45",
           "18.10.2010 11:00",
           "20.10.2010 08:45", "20.10.2010 09:00",
           "22.10.2010 13:45",
           "28.10.2010 13:30", "28.10.2010 13:45",
           "03.11.2010 09:30",
           "06.12.2010 10:45", "06.12.2010 11:00"
)
jirr <- c(rep(20.021/4, 4),
          rep(20.517/4, 4),
          rep(10.135/2, 2),
          4.994,
          5,
          rep(9.833/2, 2),
          4.971,
          rep(10.004/2, 2),
          4.996,
          rep(9.988/2, 2)
)
sum(jirr) # water in with irrigation

# Irr match ET
# j <- which(myAtmbc$id %in% c(round(seq(1,40,length.out=10))) & myAtmbc$hour == 10)
# myAtmbc$Irr[j] <- sum(myAtmbc$ET0 / length(j))

j <- which(myAtmbc$Date %in% jtime) # length(unique(j))
myAtmbc$Irr[j] <- jirr

# what if aspersion not really homogeneous and sensor receives less water?
# pictures show that there was ponding, plants interception... ecc. so decrease irrigation
# jirr_factor <- 1.0
# myAtmbc$Irr[j] <- jirr            * jirr_factor
# print("reducing irrigation by a factor!!!!!!!!!!!!!!!!!!!!!!!!")

print(paste0("sum of ET water is: ", sum(myAtmbc$ET0), " mm", sep=""))
print(paste0("sum of irrigation water is: ", sum(myAtmbc$Irr), " mm", sep=""))
#

myAtmbc$ET0_m3s <- myAtmbc$ET0 / 1e3 / dt_weather * 1 # from mm/dt to m/s
myAtmbc$Irr_m3s <- myAtmbc$Irr / 1e3 / dt_weather # from mm/dt to m/s

a <- myAtmbc %>% dplyr::select(c("Date_pos","Irr_m3s","ET0_m3s"))
a$Date_pos <- a$Date_pos - min(a$Date_pos)
write.table(a, file="R/Table_atmbc.csv", dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
rm(a)

# just to match cathcment scale function
myAtmbc$Rain_m3s <- 0
myAtmbc$ET0green_m3s <- 0
myAtmbc$Irrgreen_m3s <- 0

addndays <- 3 * 86400 # in seconds
nrepwarmup <- 2 # how many times to repeat the warm up?
stepndays <- addndays * nrepwarmup / dt_weather
jfactor_warmup <- c(1,1) # only ET

write.table("1 0",
            paste0("input/atmbc_DLC", sep=""),
            append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

# warm up
for (iii in 1:nrepwarmup) {
  pb = txtProgressBar(min = 1, max = stepndays/nrepwarmup, initial = 1) 
  for (ii in 1:(stepndays/nrepwarmup)) {
    setTxtProgressBar(pb,ii)
    
    # myNode_atmbc <- myAtmbc$Irr_m3s[ii] - myAtmbc$ET0_m3s[ii]
    myNode_atmbc <- 0 - myAtmbc$ET0_m3s[ii]
    
    write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays*(iii-1),
                paste0("input/atmbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    
    write.table(myNode_atmbc * jfactor_warmup[iii],
                paste0("input/atmbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  close(pb)
}





#
sumwater_m2 <- rep(0, nrow(myAtmbc)) # CATHY wants m/2 and then uses area to node for having m3/s
# data series
pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1) 
for (ii in 1:nrow(myAtmbc)) {
  setTxtProgressBar(pb,ii)
  
  myNode_atmbc <- myAtmbc$Irr_m3s[ii] - myAtmbc$ET0_m3s[ii]
  
  sumwater_m2[ii] <- sum(myNode_atmbc, na.rm=TRUE)
  
  write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays*nrepwarmup,
              paste0("input/atmbc_DLC", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(myNode_atmbc,
              paste0("input/atmbc_DLC", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
}
close(pb)

# check water balance
sumwater_m2 <- sumwater_m2 * 1e3 * dt_weather# from m/s to mm/dt, and divided by roughly irrigated area corresponding to 1 m2 (number nodes)
sum_myAtmbc <- (myAtmbc$Irr - myAtmbc$ET0)
plot(sum_myAtmbc , sumwater_m2)
abline(a=0, b=1, "red")
sum(sumwater_m2)
sum(sum_myAtmbc)
plot(myAtmbc$Date_pos_R , sum_myAtmbc, type="l")
lines(myAtmbc$Date_pos_R , sumwater_m2, col="cyan")
# CATHY in "risul" calculates CUMULATIVE TOTAL NET FLOW VOLUME (VIN + VOUT)  :  

#
# myNode_atmbc_plot <- as.data.frame(t(myNode_atmbc))
# myNode_atmbc_plot <- cbind(myNode_atmbc_plot , xyz_p)
# names(myNode_atmbc_plot)[1] <- "atmbc"
# ggplot(myNode_atmbc_plot,
#        mapping=aes(V2, V3)) +
#   geom_raster(aes(fill = atmbc))


# save time to print solutions to embed in parm
if (addndays == 0) {
  a <- seq(0,max(myAtmbc$Date_pos)-min(myAtmbc$Date_pos), by = 900) # 15 minutes
  aa <- myAtmbc$Date_pos-min(myAtmbc$Date_pos)
} else {
  a <- seq(0,addndays*nrepwarmup-1, by=86400) # end of array will be in the next array to concatenate
  a <- c(a , seq(0,max(myAtmbc$Date_pos)-min(myAtmbc$Date_pos), by = 900) + addndays*nrepwarmup )
  aa <- addndays*nrepwarmup + myAtmbc$Date_pos-min(myAtmbc$Date_pos)
  j <- match(aa, a);
  a <- as.data.frame(a)
  a$atmBC <- NA
  a$atmBC[j] <- myAtmbc$Irr_m3s
}
write.table(a,
            paste0("input/parmtime_DLC.txt", sep=""),
            append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
print(paste0("length a:", nrow(a), sep=""))


# rewrite to save only first 3 days of simulation after warmup. in theory could remove time 0 and end because saved by CATHY
if (addndays == 0) {
  a <- seq(0,max(myAtmbc$Date_pos)-min(myAtmbc$Date_pos), by = 900) # 15 minutes
  aa <- myAtmbc$Date_pos-min(myAtmbc$Date_pos)
} else {
  a <- seq(addndays*nrepwarmup , (addndays*nrepwarmup + 48*86400), by=4*3600) # to keep 287, 3&900, 6&1800, 9&2700, 12&3600, 24&7200, 48&4*3600, 72&21600
  a <- as.data.frame(a)
  # a$atmBC <- NA
  # a$atmBC[j] <- myAtmbc$Irr_m3s
}
write.table(a,
            paste0("input/parmtime_DLC.txt", sep=""),
            append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
print(paste0("removing first and last, length vtk is:", nrow(a)-2, sep=""))
print(paste0("tend in parm is:", max(a), sep=""))
