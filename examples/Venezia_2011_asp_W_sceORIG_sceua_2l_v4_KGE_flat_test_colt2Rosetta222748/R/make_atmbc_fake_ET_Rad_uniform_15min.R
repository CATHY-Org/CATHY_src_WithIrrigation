
rm(list=ls());
# .rs.restartR()
setwd("C:/CATHY/cathy1_0/runs/Venezia_2011_asp_W_sceORIG_short_nolag")

enddate <- "14.12.2010 23:45"

# plot(diff(mydf$Date_pos))
# which(diff(mydf$Date_pos)!=900)
datedel <- "14.10.2010 15:20"

library(dplyr)
library(ggplot2)
library(zoo)
# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3

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
nameMETEOin <- "../Venezia_2011/rdatireport20092010/4gam2_meteo_soil.csv"
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

j <- which(mydf$Date == enddate)
mydf <- mydf[1:j,]
rm(j, enddate)

j <- which(mydf$Date == datedel)
mydf <- mydf[-j,]
rm(j, datedel)


### Weather data
# save a mock up example from the real Weather_EB. data to be edited to have irrigation only
# write.table(myRain, paste0("R/Weather_EB.csv", sep=""),
#             append = FALSE, sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)
ndays <- length(unique(mydf$DOY))
tmpname <- c("idrow","id","hour","hourcum","ET0","Irr")
dfrow <- ndays * 24 * 4 # dt = 15 min for 40 days
myAtmbc <- as.data.frame(matrix(NA,nrow=dfrow,ncol=length(tmpname)))
names(myAtmbc) <- tmpname
myAtmbc$idrow <- 1:dfrow
myAtmbc$id <- rep(1:ndays, each=24*4)
myAtmbc$hour <- rep(rep(0:23, each=4), ndays)
# myAtmbc$hourcum <- 1:nrow(myAtmbc)
myAtmbc$hourcum <- rep(1:(ndays * 24), each=4)




# ###
# # global radiance
# nameRADout <- "../Venezia_2011/rdatireport20092010/dati pontecagnano_radglo.csv"
# myRAD <- read.csv(nameRADout, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
# 
# j1 <- which(myRAD$DATA == substr(mydf$Date[1], 1,10))
# j2 <- which(myRAD$DATA == substr(mydf$Date[nrow(mydf)], 1,10))
# myRAD <- myRAD[j1:j2,]
# 
# # locf from zoo, copy element before
# j <- sum(is.na(myRAD$RGT_TOTALE_kJ_m2)) # find 1 column but locf on all columns
# if (length(j)>0) {
#   # jpos <- which(is.na(myRAD$RGT_TOTALE_kJ_m2))
#   print(paste0(j, " NA", sep=""))
#   
#   myRAD <- na.locf(object = myRAD)
#   
#   rm(j)
# }
# 
# 
# mydf$RadGlo_Wm2 <- 0
# for (i in 1:nrow(myRAD)) {
#   tmax <- myRAD$RGT_ORA_MAX[i]
#   Rmax <- myRAD$RGT_VAL_MAX_kJ_m2[i]
#   if (Rmax > 0) {
#     Rtot <- myRAD$RGT_TOTALE_kJ_m2[i]
#     ttot <- Rtot/Rmax
#     
#     if (ttot >= 3) {
#       t1 <- floor(tmax - ttot/2)
#       t2 <- ceiling(tmax + ttot/2)
#       ttot_adj <- t2-t1
#       Rmax_adj <- Rtot / ttot_adj
#       Rmax_adj <- Rmax_adj / 3600 * 1000 # from kJ to W assuming hourly sum of kJ that is kJ/m2/hour
#       
#       t1 <- as.character(t1);
#       if (nchar(t1) == 1) { t1 <- paste0("0", t1, sep="")}
#       t2 <- as.character(t2);
#       if (nchar(t2) == 1) { t2 <- paste0("0", t2, sep="")}
#       j1 <- which( paste0(myRAD$DATA[i], " ", t1, sep="") == substr(mydf$Date,1,13))
#       j2 <- which( paste0(myRAD$DATA[i], " ", t2, sep="") == substr(mydf$Date,1,13))
#       mydf$RadGlo_Wm2[j1:j2] <- round(Rmax_adj, digits=0)
#     } else if (ttot == 1) {
#       t1 <- as.character(tmax);
#       if (nchar(t1) == 1) { t1 <- paste0("0", t1, sep="")}
#       Rmax <- Rmax / 3600 * 1000 # from kJ to W assuming hourly sum of kJ that is kJ/m2/hour
#       j1 <- which( paste0(myRAD$DATA[i], " ", t1, sep="") == substr(mydf$Date,1,13))
#       mydf$RadGlo_Wm2[j1] <- round(Rmax, digits=0)
#     } else if (ttot == 2) {
#       t1 <- as.character(tmax);
#       if (nchar(t1) == 1) { t1 <- paste0("0", t1, sep="")}
#       t2 <- as.character(tmax+1); # working hypothesis to use the hour after
#       if (nchar(t2) == 1) { t2 <- paste0("0", t2, sep="")}
#       Rmax <- Rmax/2 / 3600 * 1000 # from kJ to W assuming hourly sum of kJ that is kJ/m2/hour
#       j1 <- which( paste0(myRAD$DATA[i], " ", t1, sep="") == substr(mydf$Date,1,13))
#       j2 <- which( paste0(myRAD$DATA[i], " ", t2, sep="") == substr(mydf$Date,1,13))
#       mydf$RadGlo_Wm2[j1:j2] <- round(Rmax, digits=0)
#     }
#     
#   }
#   
# }
# rm(tmax, Rmax, Rtot, ttot, t1,t2, ttot_adj, Rmax_adj, j1,j2)
# 
# 
# tau <- 0.7
# 
# mydf$RadGlo_Wm2 <- mydf$RadGlo_Wm2 * tau # transmissivity of incoming global solar radiation between 40% and 50%
# 
# # write.table(myRAD, file=paste0("myrad.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
# 
# # # assume sun for 9 hours a day to scale the daily
# # maxrad <- myRAD$RGT_TOTALE_kJ_m2  * 1000 / 3600 / 9
# # # w/m2, sun from 5:00 to 19:00
# # 
# # myk1 <- matrix(0,nrow=length(maxrad), ncol=9)
# # 
# # myk2 <- matrix( rep(maxrad, each=9), nrow=length(maxrad), ncol=9, byrow=TRUE)
# # 
# # myk3 <- matrix(0,nrow=length(maxrad), ncol=6)
# # 
# # myk <- cbind(myk1, myk2, myk3)
# # 
# # myk <-as.vector(t(myk))
# # 
# # mydf$RadGlo_Wm2 <- myk
# # rm(myk, myk1, myk2, myk3)
# # 
# # mydf$RadGlo_Wm2 <- mydf$RadGlo_Wm2 * tau # transmissivity of incoming global solar radiation between 40% and 50%
# 
# ###
# mydf$WindSp_ms <- 0.1 # info from Fernandez 2010, it fluctuates between 0.1 and 0.3 m/s
# 
# ###
# mydf$Date <- format(as.POSIXct(mydf$Date_pos, origin="1970-01-01"), format="%Y-%m-%d %H:%M:%S")
# 
# # add these to be consistent with the script to source and calculate a summary for the day
# mydf$Rain_mm <- 0
# # ET0 greenhouse
# # FAO56
# ###
# source("C:/REWATERING/Rain/data_h/fao_Snyder_greenhouse_DLC_R_Rain_source.R")
# 
# myAtmbc$ET0 <- mydf$ET0green
myAtmbc$ET0 <- 0
myAtmbc$Date_pos <- mydf$Date_pos

# a <- myAtmbc %>%
#   dplyr::group_by(id) %>%
#   dplyr::mutate(sumET = sum(ET0)) %>%
#   dplyr::slice(1) %>%
#   dplyr::ungroup()
# ggplot(a, mapping=aes(x=hourcum, y=sumET)) +
#   geom_step()






# 
# # no cultivar because cut
# # sensor on april 1
# # cuts on april 6 and on april 24
# myk <- c(rep(1,7), seq(0,1,length.out=7), rep(1,10), seq(0,1,length.out=7), rep(1,9))
# myk <- rep(myk,each=24)
# myAtmbc$ET0 <- myAtmbc$ET0 * myk
# plot(myAtmbc$ET0, type="l")

# Irr
myAtmbc$Irr <- 0
#
# j <- which(myAtmbc$id %in% c(round(seq(1,40,length.out=10))) & myAtmbc$hour == 10)
# myAtmbc$Irr[j] <- sum(myAtmbc$ET0 / length(j))
#
# jtime <- c(0,1,2,3,4,6)*24 + c(17,11,11,12,12,10)
# jirr <- c(20.5,10.1,5.0,5.0,9.8,5.0) # this is correct
# jirr <-   c(20.5,13.1,5.0,5.0,5.0,5.0) # fifth event is not reflected in data. could also be aspersion is intercepted by plants
jtime <- c(0,1,2,3,4,6,8,14,20,53)*24 + c(17,11,11,12,12,10,15,14,11,12)
jirr <- c(20,20.5,10.1,5.0,5.0,9.8,5.0,10.0,5.0,10.0) # this is correct
jirr <- c(rep(5,4),rep(5,4), rep(5,2), 5.0, 5.0, rep(5,2), 5.0, rep(5,2), 5.0, rep(5,2)) # this is correct, from hours to 15 minutes

j <- which(myAtmbc$hourcum %in% jtime)
# j <- j[seq(1,length(j),by=4)]
j <- j[c(1,2,3,4, 5,6,7,8, 9,10, 13, 17, 21,22, 25, 29,30, 33, 37,38)]
# myAtmbc$Irr[j] <- sum(myAtmbc$ET0 / length(j))
# myAtmbc$Irr[j] <- c(10,10,6,4,4,4,8)


# what if aspersion not really homogeneous and sensor receives less water?
# pictures show that there was ponding, plants interception... ecc. so decrease irrigation
# jirr_factor <- c(0.4, 0.8, 0.8, 0.8, 0.8, 0.8)
# jirr_factor <- 0.6
jirr_factor <- 1.0
myAtmbc$Irr[j] <- jirr            * jirr_factor
print("reducing irrigation by a factor!!!!!!!!!!!!!!!!!!!!!!!!")

print(paste0("sum of irrigation water is: ", sum(myAtmbc$Irr), " mm", sep=""))
#

myAtmbc$ET0_m3s <- myAtmbc$ET0 / 1e3 / dt_weather * 1 # from mm/dt to m/s
myAtmbc$Irr_m3s <- myAtmbc$Irr / 1e3 / dt_weather # from mm/dt to m/s

myAtmbc$ET0_m3s <- 0

print(paste0("sum of ET is: ", sum(myAtmbc$ET0_m3s), " m/s", sep=""))
print(paste0("sum of irrigation water is: ", sum(myAtmbc$Irr_m3s), " m/s", sep=""))
# cell2nod <- function(rmap,rain,et0,irr) {
#   switch(as.character(rmap),
#          "0" = { # outside domain. imagine rain is the same
#            y <- rain
#          },
#          "1" = { # urban
#            y <- rain
#          },
#          "8" = { # forest
#            y <- rain - et0
#          }, # or agriculture
#          y <- rain - et0 + irr
#   )
#   y <- y / 4 # 4 nodes per cell
#   return(y)
# }
cell2nod <- function(rmap,rain,et0,irr,et0_green,irr_green) {
  switch(as.character(rmap),
         # "0" = { # outside domain. imagine rain is the same
         #   y <- rain
         # },
         "1" = { # interrow
           #TODO
           y <- 0
         },
         "2" = { # crop
           #TODO
           y <- - et0 + irr # first aspersion to water the seeds. then subirr
         }
  )
  y <- y / 4 # 4 nodes per cell
  return(y)
}
# rain2sto <- function(rmap,rain) {
#   switch(as.character(rmap),
#          "1" = { # greenhouse
#            y <- rain
#          }
#   )
#   y <- y / 4 # 4 nodes per cell
#   return(y)
# }

addndays <- 0 * 86400 # in seconds
stepndays <- addndays / 3600

write.table("0 1",
            paste0("input/atmbc_DLC", sep=""),
            append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
if (addndays > 0) {
  for (ii in 1:stepndays) {
    write.table(dt_weather * (ii-1),
                paste0("input/atmbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    write.table(t(rep(0, nrow(xyz_p))),
                paste0("input/atmbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
}
r_landuse_copy <- r_landuse
r_landuse <- r_landuse[myrow:1,] # root map is flipped upside down between cathy and here
pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1) 
for (ii in 1:nrow(myAtmbc)) {
  setTxtProgressBar(pb,ii)
  myNode_atmbc <- matrix(NA, nrow=1, ncol=nrow(xyz_p)) # use NA to check if there are issues
  
  # rain on greenhouse
  # rainoutlet <- myAtmbc$Rain_m3s[ii] * nnod_green
  # add these partitioned along stream or in the outlet cell at the end of for loop. need a find to have position
  
  for (i in 1:nrow(xyz_p)) {
    # get position node
    xyz_x_ncol <- xyz_p$V2[i]
    xyz_y_nrow <- xyz_p$V3[i]
    
    # find cell(s) contributing to that node
    cell_nrow <- xyz_y_nrow
    cell_ncol <- xyz_x_ncol
    
    if (xyz_y_nrow == 0 && xyz_x_ncol == 0) {
      # bottom left
      myNode_atmbc[1,i] <- cell2nod(r_landuse[cell_nrow+1,cell_ncol+1],
                                    myAtmbc$Rain_m3s[ii],
                                    myAtmbc$ET0_m3s[ii],
                                    myAtmbc$Irr_m3s[ii],
                                    myAtmbc$ET0green_m3s[ii],
                                    myAtmbc$Irrgreen_m3s[ii]
      )
    } else if (xyz_y_nrow == 0 && xyz_x_ncol == mycol) {
      # bottom right
      myNode_atmbc[1,i] <- cell2nod(r_landuse[cell_nrow+1,cell_ncol],
                                    myAtmbc$Rain_m3s[ii],
                                    myAtmbc$ET0_m3s[ii],
                                    myAtmbc$Irr_m3s[ii],
                                    myAtmbc$ET0green_m3s[ii],
                                    myAtmbc$Irrgreen_m3s[ii]
      )
    } else if (xyz_y_nrow == myrow && xyz_x_ncol == 0) {
      # top left
      myNode_atmbc[1,i] <- cell2nod(r_landuse[cell_nrow,cell_ncol+1],
                                    myAtmbc$Rain_m3s[ii],
                                    myAtmbc$ET0_m3s[ii],
                                    myAtmbc$Irr_m3s[ii],
                                    myAtmbc$ET0green_m3s[ii],
                                    myAtmbc$Irrgreen_m3s[ii]
      )
    } else if (xyz_y_nrow == myrow && xyz_x_ncol == mycol) {
      # top right
      myNode_atmbc[1,i] <- cell2nod(r_landuse[cell_nrow,cell_ncol],
                                    myAtmbc$Rain_m3s[ii],
                                    myAtmbc$ET0_m3s[ii],
                                    myAtmbc$Irr_m3s[ii],
                                    myAtmbc$ET0green_m3s[ii],
                                    myAtmbc$Irrgreen_m3s[ii]
      )
      
      
    } else if (xyz_y_nrow == 0 && xyz_x_ncol > 0 && xyz_x_ncol < mycol) {
      # bottom
      a1 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      a2 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol+1],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      myNode_atmbc[1,i] <- a1+a2
      
      
      
    } else if (xyz_y_nrow == myrow && xyz_x_ncol > 0 && xyz_x_ncol < mycol) {
      # top
      a1 <- cell2nod(r_landuse[cell_nrow,cell_ncol],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      a2 <- cell2nod(r_landuse[cell_nrow,cell_ncol+1],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      myNode_atmbc[1,i] <- a1+a2
      
      
    } else if (xyz_y_nrow > 0 && xyz_y_nrow < myrow && xyz_x_ncol == 0) {
      # left
      a1 <- cell2nod(r_landuse[cell_nrow,cell_ncol+1],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      a2 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol+1],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      myNode_atmbc[1,i] <- a1+a2
      
      
    } else if (xyz_y_nrow > 0 && xyz_y_nrow < myrow && xyz_x_ncol == mycol) {
      # right
      a1 <- cell2nod(r_landuse[cell_nrow,cell_ncol],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      a2 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      myNode_atmbc[1,i] <- a1+a2
      
      
    } else if (xyz_y_nrow > 0 && xyz_y_nrow < myrow && xyz_x_ncol > 0 && xyz_x_ncol < mycol) {
      # we are in the middle, clockwise index
      a1 <- cell2nod(r_landuse[cell_nrow,cell_ncol],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      a2 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      a3 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol+1],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      a4 <- cell2nod(r_landuse[cell_nrow,cell_ncol+1],
                     myAtmbc$Rain_m3s[ii],
                     myAtmbc$ET0_m3s[ii],
                     myAtmbc$Irr_m3s[ii],
                     myAtmbc$ET0green_m3s[ii],
                     myAtmbc$Irrgreen_m3s[ii]
      )
      myNode_atmbc[1,i] <- a1+a2+a3+a4
      
    }
    
  }
  
  write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays,
              paste0("input/atmbc_DLC", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(myNode_atmbc,
              paste0("input/atmbc_DLC", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
}
close(pb)
r_landuse <- r_landuse_copy
rm(r_landuse_copy)

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
  a <- seq(0,addndays-1, by=86400) # end of array will be in the next array to concatenate
  a <- c(a , seq(0,max(myAtmbc$Date_pos)-min(myAtmbc$Date_pos), by = 900) + addndays )
  aa <- addndays + myAtmbc$Date_pos-min(myAtmbc$Date_pos)
  j <- match(aa, a);
  a <- as.data.frame(a)
  a$atmBC <- NA
  a$atmBC[j] <- myAtmbc$Irr_m3s
}
write.table(a,
            paste0("input/parmtime_DLC.txt", sep=""),
            append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
print(paste0("length a:", nrow(a), sep=""))



# print("ET times 10")





# ###
# # Root Growth
# root2nod <- function(rmap) {
#   switch(as.character(rmap),
#          # "0" = { # outside domain. imagine rain is the same
#          #   y <- rain
#          # },
#          "1" = { # interrow
#            #TODO
#            y <- 0.001
#          },
#          "2" = { # crop
#            #TODO
#            y <- 0.6 # first aspersion to water the seeds. then subirr
#          }
#   )
#   return(y)
# }
# 
# nameROOTin <- "../Venezia_2011/Table_RootGrowth.csv"
# myROOT <- read.csv(nameROOTin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
# 
# r_landuse_copy <- r_landuse
# r_landuse <- r_landuse[myrow:1,] # root map is flipped upside down between cathy and here
# 
# write.table("0 1",
#             paste0("input/ZROOT_DLC", sep=""),
#             append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# if (addndays > 0) {
#   for (ii in 1:stepndays) {
#     write.table(dt_weather * (ii-1),
#                 paste0("input/ZROOT_DLC", sep=""),
#                 append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
#     write.table(t(rep(0.001, nrow(xyz_p))),
#                 paste0("input/ZROOT_DLC", sep=""),
#                 append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
#   }
# }
# 
# pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1) 
# for (ii in 1:nrow(myAtmbc)) {
#   setTxtProgressBar(pb,ii)
#   myNode_atmbc <- matrix(NA, nrow=1, ncol=nrow(xyz_p)) # use NA to check if there are issues
#   
#   for (i in 1:nrow(xyz_p)) {
#     # get position node
#     xyz_x_ncol <- xyz_p$V2[i]
#     xyz_y_nrow <- xyz_p$V3[i]
#     
#     # find cell(s) contributing to that node
#     cell_nrow <- xyz_y_nrow
#     cell_ncol <- xyz_x_ncol
#     
#     if (xyz_y_nrow == 0 && xyz_x_ncol == 0) {
#       # bottom left
#       myNode_atmbc[1,i] <- root2nod(r_landuse[cell_nrow+1,cell_ncol+1],
#                                     myAtmbc$Rain_m3s[ii],
#                                     myAtmbc$ET0_m3s[ii],
#                                     myAtmbc$Irr_m3s[ii],
#                                     myAtmbc$ET0green_m3s[ii],
#                                     myAtmbc$Irrgreen_m3s[ii]
#       )
#     } else if (xyz_y_nrow == 0 && xyz_x_ncol == mycol) {
#       # bottom right
#       myNode_atmbc[1,i] <- cell2nod(r_landuse[cell_nrow+1,cell_ncol],
#                                     myAtmbc$Rain_m3s[ii],
#                                     myAtmbc$ET0_m3s[ii],
#                                     myAtmbc$Irr_m3s[ii],
#                                     myAtmbc$ET0green_m3s[ii],
#                                     myAtmbc$Irrgreen_m3s[ii]
#       )
#     } else if (xyz_y_nrow == myrow && xyz_x_ncol == 0) {
#       # top left
#       myNode_atmbc[1,i] <- cell2nod(r_landuse[cell_nrow,cell_ncol+1],
#                                     myAtmbc$Rain_m3s[ii],
#                                     myAtmbc$ET0_m3s[ii],
#                                     myAtmbc$Irr_m3s[ii],
#                                     myAtmbc$ET0green_m3s[ii],
#                                     myAtmbc$Irrgreen_m3s[ii]
#       )
#     } else if (xyz_y_nrow == myrow && xyz_x_ncol == mycol) {
#       # top right
#       myNode_atmbc[1,i] <- cell2nod(r_landuse[cell_nrow,cell_ncol],
#                                     myAtmbc$Rain_m3s[ii],
#                                     myAtmbc$ET0_m3s[ii],
#                                     myAtmbc$Irr_m3s[ii],
#                                     myAtmbc$ET0green_m3s[ii],
#                                     myAtmbc$Irrgreen_m3s[ii]
#       )
#       
#       
#     } else if (xyz_y_nrow == 0 && xyz_x_ncol > 0 && xyz_x_ncol < mycol) {
#       # bottom
#       a1 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       a2 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol+1],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       myNode_atmbc[1,i] <- a1+a2
#       
#       
#       
#     } else if (xyz_y_nrow == myrow && xyz_x_ncol > 0 && xyz_x_ncol < mycol) {
#       # top
#       a1 <- cell2nod(r_landuse[cell_nrow,cell_ncol],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       a2 <- cell2nod(r_landuse[cell_nrow,cell_ncol+1],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       myNode_atmbc[1,i] <- a1+a2
#       
#       
#     } else if (xyz_y_nrow > 0 && xyz_y_nrow < myrow && xyz_x_ncol == 0) {
#       # left
#       a1 <- cell2nod(r_landuse[cell_nrow,cell_ncol+1],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       a2 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol+1],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       myNode_atmbc[1,i] <- a1+a2
#       
#       
#     } else if (xyz_y_nrow > 0 && xyz_y_nrow < myrow && xyz_x_ncol == mycol) {
#       # right
#       a1 <- cell2nod(r_landuse[cell_nrow,cell_ncol],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       a2 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       myNode_atmbc[1,i] <- a1+a2
#       
#       
#     } else if (xyz_y_nrow > 0 && xyz_y_nrow < myrow && xyz_x_ncol > 0 && xyz_x_ncol < mycol) {
#       # we are in the middle, clockwise index
#       a1 <- cell2nod(r_landuse[cell_nrow,cell_ncol],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       a2 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       a3 <- cell2nod(r_landuse[cell_nrow+1,cell_ncol+1],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       a4 <- cell2nod(r_landuse[cell_nrow,cell_ncol+1],
#                      myAtmbc$Rain_m3s[ii],
#                      myAtmbc$ET0_m3s[ii],
#                      myAtmbc$Irr_m3s[ii],
#                      myAtmbc$ET0green_m3s[ii],
#                      myAtmbc$Irrgreen_m3s[ii]
#       )
#       myNode_atmbc[1,i] <- a1+a2+a3+a4
#       
#     }
#     
#   }
#   
#   write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays,
#               paste0("input/ZROOT_DLC", sep=""),
#               append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
#   write.table(myNode_atmbc,
#               paste0("input/atmbcZROOT_DLC_DLC", sep=""),
#               append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# }
# close(pb)
# r_landuse <- r_landuse_copy
# rm(r_landuse_copy)