# .rs.restartR()
rm(list=ls())
setwd("C:/CATHY/cathy1_0/runs/Venezia_2011_ic_ET_soil_sub")

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
library(zoo)
# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3

myrow <- 10
mycol <- 13
dx <- 0.1
dy <- 0.1

dt_weather <- 5*60 # 5 minutes in seconds
dt_weather <- 60*60 # going to 1 hour

nameout <- paste0("input/nansfneubc_DLC", sep="") # postprocessed to match DTM active cells

namelanduse <- paste0("input/root_map", sep="") # postprocessed to match DTM active cells
namexyz <- paste0("output/xyz", sep="")

r_landuse <- read.table(namelanduse, header=FALSE, sep=" ", dec = ".", skip=6)
if (is.na(r_landuse[1,1])) {r_landuse <- r_landuse[,-1]}
if (nrow(r_landuse) != myrow | (ncol(r_landuse) != mycol)) {stop("row or col do not match")}

xyz_header <- read.table(namexyz,
                         head = FALSE,
                         nrows = 1)
if (!exists("xyz_header")) { stop("create file xyz running CATHY with input/parm/IPRT1=3") }
NNOD <- xyz_header[1,1]
N <- xyz_header[1,2]
Nlayer <- 4 # 1 for atmbc, 2... for subsurface. plastic says that Nlayer=2 has Kz=0

xyz <- read.table(namexyz, header=FALSE, sep="", dec = ".", skip=1) # do not specify delimiter to treat multiple space as delimiter
xyz_p <- xyz[(NNOD*(Nlayer-1)+1) : (NNOD*Nlayer), ]
if (nrow(xyz_p) != NNOD) {stop("nrow do not match")}
xyz_p$V2 <- (xyz_p$V2 + 0) /dx
xyz_p$V3 <- (xyz_p$V3 + 0) /dy

# area nodes, var ARENOD
r_arenod <- read.csv("fort.111", header=FALSE, sep="", dec = ".", skip=0) # do not specify delimiter to treat multiple space as delimiter
if (ncol(r_arenod) != NNOD) {stop("nrow do not match")}



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
  dplyr::filter(coltura==2 & irrigazione=="sub")

myMETEO_SOIL$DOY <- as.numeric(strftime(
  as.POSIXct(myMETEO_SOIL$Date, format="%d.%m.%Y %H:%M", origin="1970-01-01", tz="CET"),
  format = "%j", tz="CET")) # doy

myMETEO_SOIL$hour <- as.numeric(substr(myMETEO_SOIL$Date,12,13))

names(myMETEO_SOIL)[which(names(myMETEO_SOIL)=="T_aria")] <- "Tmean_C"
names(myMETEO_SOIL)[which(names(myMETEO_SOIL)=="RH_perc")] <- "RH"

myMETEO_SOIL <- myMETEO_SOIL %>%
  dplyr::group_by(DOY,hour) %>%
  dplyr::mutate(Tmean_C = mean(Tmean_C, na.rm=TRUE),
                RH_in = mean(RH, na.rm=TRUE)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Date_pos)

mydf <- myMETEO_SOIL
rm(myMETEO_SOIL)






### Weather data
# save a mock up example from the real Weather_EB. data to be edited to have irrigation only
# write.table(myRain, paste0("R/Weather_EB.csv", sep=""),
#             append = FALSE, sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)
ndays <- length(unique(mydf$DOY))
tmpname <- c("idrow","id","hour","hourcum","ET0","Irr")
dfrow <- ndays * 24 # dt = 1h for 40 days
myAtmbc <- as.data.frame(matrix(NA,nrow=dfrow,ncol=length(tmpname)))
names(myAtmbc) <- tmpname
myAtmbc$idrow <- 1:dfrow
myAtmbc$id <- rep(1:ndays, each=24)
myAtmbc$hour <- rep(0:23, ndays)
myAtmbc$hourcum <- 1:nrow(myAtmbc)




###
# global radiance
nameRADout <- "../Venezia_2011/rdatireport20092010/dati pontecagnano_radglo.csv"
myRAD <- read.csv(nameRADout, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

j1 <- which(myRAD$DATA == substr(mydf$Date[1], 1,10))
j2 <- which(myRAD$DATA == substr(mydf$Date[nrow(mydf)], 1,10))
myRAD <- myRAD[j1:j2,]

# locf from zoo, copy element before
j <- sum(is.na(myRAD$RGT_TOTALE_kJ_m2)) # find 1 column but locf on all columns
if (length(j)>0) {
  # jpos <- which(is.na(myRAD$RGT_TOTALE_kJ_m2))
  print(paste0(j, " NA", sep=""))
  
  myRAD <- na.locf(object = myRAD)
  
  rm(j)
}


mydf$RadGlo_Wm2 <- 0
for (i in 1:nrow(myRAD)) {
  tmax <- myRAD$RGT_ORA_MAX[i]
  Rmax <- myRAD$RGT_VAL_MAX_kJ_m2[i]
  if (Rmax > 0) {
    Rtot <- myRAD$RGT_TOTALE_kJ_m2[i]
    ttot <- Rtot/Rmax
    
    if (ttot >= 3) {
      t1 <- floor(tmax - ttot/2)
      t2 <- ceiling(tmax + ttot/2)
      ttot_adj <- t2-t1
      Rmax_adj <- Rtot / ttot_adj
      Rmax_adj <- Rmax_adj / 3600 * 1000 # from kJ to W assuming hourly sum of kJ that is kJ/m2/hour
      
      t1 <- as.character(t1);
      if (nchar(t1) == 1) { t1 <- paste0("0", t1, sep="")}
      t2 <- as.character(t2);
      if (nchar(t2) == 1) { t2 <- paste0("0", t2, sep="")}
      j1 <- which( paste0(myRAD$DATA[i], " ", t1, sep="") == substr(mydf$Date,1,13))
      j2 <- which( paste0(myRAD$DATA[i], " ", t2, sep="") == substr(mydf$Date,1,13))
      mydf$RadGlo_Wm2[j1:j2] <- round(Rmax_adj, digits=0)
    } else if (ttot == 1) {
      t1 <- as.character(tmax);
      if (nchar(t1) == 1) { t1 <- paste0("0", t1, sep="")}
      Rmax <- Rmax / 3600 * 1000 # from kJ to W assuming hourly sum of kJ that is kJ/m2/hour
      j1 <- which( paste0(myRAD$DATA[i], " ", t1, sep="") == substr(mydf$Date,1,13))
      mydf$RadGlo_Wm2[j1] <- round(Rmax, digits=0)
    } else if (ttot == 2) {
      t1 <- as.character(tmax);
      if (nchar(t1) == 1) { t1 <- paste0("0", t1, sep="")}
      t2 <- as.character(tmax+1); # working hypothesis to use the hour after
      if (nchar(t2) == 1) { t2 <- paste0("0", t2, sep="")}
      Rmax <- Rmax/2 / 3600 * 1000 # from kJ to W assuming hourly sum of kJ that is kJ/m2/hour
      j1 <- which( paste0(myRAD$DATA[i], " ", t1, sep="") == substr(mydf$Date,1,13))
      j2 <- which( paste0(myRAD$DATA[i], " ", t2, sep="") == substr(mydf$Date,1,13))
      mydf$RadGlo_Wm2[j1:j2] <- round(Rmax, digits=0)
    }
    
  }
  
}
rm(tmax, Rmax, Rtot, ttot, t1,t2, ttot_adj, Rmax_adj, j1,j2)


tau <- 0.7

mydf$RadGlo_Wm2 <- mydf$RadGlo_Wm2 * tau # transmissivity of incoming global solar radiation between 40% and 50%

# write.table(myRAD, file=paste0("myrad.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)

# # assume sun for 9 hours a day to scale the daily
# maxrad <- myRAD$RGT_TOTALE_kJ_m2  * 1000 / 3600 / 9
# # w/m2, sun from 5:00 to 19:00
# 
# myk1 <- matrix(0,nrow=length(maxrad), ncol=9)
# 
# myk2 <- matrix( rep(maxrad, each=9), nrow=length(maxrad), ncol=9, byrow=TRUE)
# 
# myk3 <- matrix(0,nrow=length(maxrad), ncol=6)
# 
# myk <- cbind(myk1, myk2, myk3)
# 
# myk <-as.vector(t(myk))
# 
# mydf$RadGlo_Wm2 <- myk
# rm(myk, myk1, myk2, myk3)
# 
# mydf$RadGlo_Wm2 <- mydf$RadGlo_Wm2 * tau # transmissivity of incoming global solar radiation between 40% and 50%

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

myAtmbc$ET0 <- mydf$ET0green
myAtmbc$Date_pos <- mydf$Date_pos

a <- myAtmbc %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(sumET = sum(ET0)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()
ggplot(a, mapping=aes(x=hourcum, y=sumET)) +
  geom_step()








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
# jtime <- c(0, 1, 2, 3, 4, 6, 8, 14, 20, 53)*24 + c(17,11,11,12,12,10,15,14,11,12)
# jirr <- c(20.0,20.5,10.1,5.0,5.0,9.8,5.0,10.0,5.0,10.0)
jtime <- c(8, 11, 14, 20, 53)*24 + c(15, 10, 13, 11, 12)
jirr <- c(10, 2, 10, 5, 10)
#
j <- which(myAtmbc$hourcum %in% jtime)
myAtmbc$Irr[j] <- jirr
#
myAtmbc$ET0_m3s <- myAtmbc$ET0 / 1e3 / dt_weather # from mm/dt to m/s
myAtmbc$Irr_m3s <- myAtmbc$Irr / 1e3 / dt_weather # from mm/dt to m/s



# boundary conditions do not need to have a flux every hour but only when need to start and end.
# so skip first 0.
# then find fluxes =! 0 and put a 0 when they end
jdel <- NULL
zerobefore <- 1
for (ii in 2:(nrow(myAtmbc)-1)) {
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

myAtmbc$Rain_m3s <- 0
myAtmbc$ET0green_m3s <- myAtmbc$ET0_m3s
myAtmbc$Irrgreen_m3s <- myAtmbc$Irr_m3s

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
           y <- irr_green
         }
  )
  y <- y / 4 # 4 nodes per cell
  return(y)
}

addndays <- 7 * 86400 # in seconds
stepndays <- addndays / 3600

# # this line will be deleted, it is to avoid an if ii==1 in the loop
# write.table("to delete",
#             paste0("nansfneubc_DLC", sep=""),
#             append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)


# for subsurface use only selected nodes
r_landuse_copy <- as.data.frame(matrix(0, nrow=nrow(r_landuse), ncol=ncol(r_landuse)))
r_landuse <- r_landuse[myrow:1,] # subsurface grid is coherent with xyz, not need for flipping

j <- which(r_landuse == 2, arr.ind=TRUE) # 2 is crop
a <- paste0(j[,1], "_", j[,2], sep="")
aa <- paste0(xyz_p$V3, "_", xyz_p$V2, sep="")
jj <- which(aa %in% a)

myNodeID_atmbc <- xyz_p$V1[jj]
myNodeID_atmbc <- as.data.frame(matrix(myNodeID_atmbc, nrow=1))

print("neumann condition require m/s to be multiplied by area pertaining to the node")
r_arenod_copy <- r_arenod
r_arenod <- r_arenod[1,jj]


xyz_p_copy <- xyz_p
xyz_p <- xyz_p[jj,]
# print(paste0("NQMAX is: ", nrow(xyz_p), sep=""))

iprint <- c(2, 4) # ET0-Irr and Irr
namelabel <- c("ET0_Irr","Irr")
# iprint <- 1 + (1:length(namelabel))
contp <- 1




pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1) 
for (ii in 1:nrow(myAtmbc)) {
  setTxtProgressBar(pb,ii)
  myNode_atmbc <- matrix(NA, nrow=1, ncol=nrow(xyz_p)) # use NA to check if there are issues
  
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
  
  myNode_atmbc <- myNode_atmbc * r_arenod
  
  if (ii %in% iprint) {
    myNode_atmbc_plot <- as.data.frame(t(myNode_atmbc))
    myNode_atmbc_plot <- cbind(myNode_atmbc_plot , xyz_p)
    names(myNode_atmbc_plot)[1] <- "atmbc"
    p <- ggplot(myNode_atmbc_plot,
                mapping=aes(V2, V3)) +
      geom_tile(aes(fill = atmbc))
    print(
      p
    )
    ggsave(paste0(nameout, "_", namelabel[contp], ".jpg", sep=""), width = 12, height = 8, units = 'in', dpi=300)
    contp <- contp + 1
  }
  
  if (ii == 1) {
    write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos),
                nameout,
                append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  } else {
    write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays,
                nameout,
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }

  write.table(paste0("0 ", length(myNode_atmbc), sep=""),
              nameout,
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

  write.table(myNodeID_atmbc,
              nameout,
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

  write.table(myNode_atmbc,
              nameout,
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

print(paste0("NQMAX is: ", nrow(xyz_p), sep=""))
xyz_p <- xyz_p_copy
rm(xyz_p_copy)

r_arenod <- r_arenod_copy
rm(r_arenod_copy)
