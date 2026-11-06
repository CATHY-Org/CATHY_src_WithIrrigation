
Sys.setlocale("LC_TIME", "English")
# .rs.restartR()
rm(list=ls())
library(shiny)
library(shinyWidgets) # for pickerinput
library(dplyr)
library(ggplot2)
library(reshape2)

# USE BATTIPAGLIA AND FILLA NA WITH EBOLI

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# rain is now joined thank to the fact that measurements are taken at 0 ending minutes. if there were other characters (e.g., 1,2,3,4,   6,7,8,9) it would not join anything
# rain has resolution of 10 minutes. after joining, there will be NA in stack_data.

setwd("C:/UNIPD/CATHY/cathy2_0/REWATERING/Salerno_20250131_ROSETTA_dirnew_mars_wobaseflow_Kzlow_free/input")

file <- "Shiny_Rain_BC_warmup.csv"
stack_data <- read.csv(file, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
stack_data <- stack_data[-c(1:168),]
rownames(stack_data) <- 1:nrow(stack_data)
file <- "Shiny_Rain_BC.csv"
stack_data2 <- read.csv(file, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
stack_data <- rbind(stack_data,stack_data2)
stack_data$Date_pos <- stack_data$Date_pos-min(stack_data$Date_pos)
rm(stack_data2)

###
# EB has all data in general, but rainfall is underestimated
file <- "Shiny_WL_BC.csv"
stack_WL <- read.csv(file, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)







#
# j <- which(is.na(a$Tair))
#
# j <- which(diff(stack_rain$Date_pos, na.rm=TRUE) > 610) # 600 seconds, 10 minutes readings
# j <- which(diff(stack_rain$Date_pos, na.rm=TRUE) < 590) # 600 seconds, 10 minutes readings
#
# if (length(j)>0) {
#   print("data not trasmitted")
# }
#
# # remaining NA? It is important to have the maximum number of rain data because very unsteady. when summing without na.rm=T, possible values are lost
# j <- which(is.na(stack_rain$Rain))
#
# if (length(j) > 0) {
#   print(paste0("filling with 0s ",length(j), " missing rain values"))
#   stack_rain$Rain[j] <- 0
#   print(paste0("filling with 24 hours previous data ",length(j), " missing weather values"))
#   # use
#   jind <- -24*6 # + - days (24 hours and hourly values)
#   # jind <- jind * c(-2,-1,1,2)
#   for (i in 1:length(j)) {
#     stack_rain$Tair[j[i]] <- mean(stack_rain$Tair[j[i]+jind], na.rm=T)
#     stack_rain$RH[j[i]] <- mean(stack_rain$RH[j[i]+jind], na.rm=T)
#     stack_rain$RadSol[j[i]] <- mean(stack_rain$RadSol[j[i]+jind], na.rm=T)
#     stack_rain$ST[j[i]] <- mean(a$ST[j[i]+jind], na.rm=T)
#     stack_rain$Wind[j[i]] <- mean(stack_rain$Wind[j[i]+jind], na.rm=T)
#     stack_rain$Tdew[j[i]] <- mean(stack_rain$Tdew[j[i]+jind], na.rm=T)
#   }
#   rm(jind,i,j)
# }

# #############################################################################

# rain measured every 10 minutes ending with 0
# CTD every 5 minutes ending with 4 or 9
# for joining I remove 1 minute from rain
stack_rain$Date_pos <- as.numeric(as.POSIXct(strptime(as.character(stack_rain$Date), format="%Y-%m-%d %H:%M:%S",tz="UTC")))
stack_rain$Date_pos <- stack_rain$Date_pos - 60 # seconds
stack_rain$Date_dum <- substr(as.character(as.POSIXct(stack_rain$Date_pos, origin="1970-01-01", tz="UTC")), 1,16)

# export variables for calculating ET as in Venezia2011/script_R_Rain_fillNA_remove_subhourly.R
stack_rain_ET <- stack_rain %>%
  dplyr::select(c("Date","RadSol","Tair","RH","Wind","ST"))

# select variables for shiny
stack_rain <- stack_rain %>%
  dplyr::select(c("Date_dum","Rain","Tair"))

###
stack_data$Date_dum <- substr(stack_data$Date, 1,16)

### data not transmitted
### first 35 rows, ends with 1 minute or 6 minutes.
# then 0 or 5
j <- which(diff(stack_data$Date_pos, na.rm=TRUE) > 400) # 300 seconds, 5 minutes readings
if (length(j) > 0) {
  for (i in length(j):1) {
    rowlast <- j[i] + 1 # move to row that has gap
    a <- stack_data[rowlast : nrow(stack_data), ]

    mytime <- seq(stack_data$Date_pos[rowlast - 1] + 5*60, stack_data$Date_pos[rowlast] - 5*60, by=5*60)
    mytimechar <- substr(as.character(as.POSIXct(mytime, origin="1970-01-01", tz="UTC")), 1,16)
    b <- as.data.frame(matrix(NA, nrow=length(mytime), ncol=ncol(stack_data)))
    names(b) <- names(stack_data)
    b$Date_pos <- mytime
    b$Date <- mytimechar
    b$Date_dum <- mytimechar

    stack_data <- rbind(stack_data[1:(rowlast - 1) , ] , b , a)
    rownames(stack_data) <- 1:nrow(stack_data)
  }
  rm(j,b,i,mytime,mytimechar)
}

# now can join with meteo data
stack_data$Date_pos_r <- as.POSIXct(strptime(as.character(stack_data$Date), format="%Y-%m-%d %H:%M",tz="UTC"))
##################################################################
stack_data <- left_join(stack_data , stack_rain , by=c("Date_dum"))
stack_data <- stack_data %>%
  dplyr::select(-Date_dum)

###
# note I take in case of tests on the sensor etc... for which measurements will be discarded
filename <- "0_note_CTD_UTC.txt"
mynote <- read.csv(filename, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

# join note to mask out non-observations
# character # used in file to comment the line
stack_data$Note <- NA
for (i in 1:nrow(mynote)) {
  if (substr(mynote$Datefrom[i],1,1) != "#") { # otherwise skip the line
    jfrom <- which(stack_data$Date == mynote$Datefrom[i])
    jto <- which(stack_data$Date == mynote$Dateto[i])
    stack_data$Note[jfrom:jto] <- mynote$Note[i]
  }
}

jmaskin <- which(is.na(stack_data$Note)) # keep good ones, without notes

stack_data <- stack_data %>%
  dplyr::select(-Note)

stack_data_copy <- stack_data

#stack_data <- stack_data_copy[jmaskin,] # otherwise I do not read the comments in the info chart
stack_data_note <- stack_data_copy[-jmaskin,]

# stack_data_wide <- stack_data # not necessary, in case to compare after "melt"

###
stack_data <- melt(stack_data, id.vars = c("Date","Date_pos","Date_pos_r"))
name_variable <- sort(unique(as.character(stack_data$variable)))

stack_data_note <- melt(stack_data_note, id.vars = c("Date","Date_pos","Date_pos_r"))
name_variable <- sort(unique(as.character(stack_data_note$variable)))

# # # find duplicate
# myBA <- myBA %>%
#   dplyr::distinct(, .keep_all = TRUE)
# print(paste0("nrow after distinct ", nrow(myBA), sep=""))
#
# print(paste0("nrow before distinct ", nrow(myEB), sep=""))
# myEB <- myEB %>%
#   dplyr::distinct(, .keep_all = TRUE)
# print(paste0("nrow after distinct ", nrow(myEB), sep=""))

###
tsize <- 18

# for BC of event
# Rain
# with sensor 1, CTD and weather were at 0 and 5 minutes
# with sensor 2, CTD is 4 or 9 minuutes. thus weather was put to 9 minutes
# sensor 2 starts at 11:29 on November 30
DOexport <- 0
DOwarmup <- 0
DOBIND <- 1
if (DOwarmup == 1) {
  # warm up period without water level data
  date1 <- "2023-05-25 00:09"; date2 <- "2023-11-30 23:59"; date1ET <- "2023-05-25 00:00:00"; date2ET <- "2023-11-30 23:00:00"
  j1 <- which(stack_rain$Date_dum == date1)
  j2 <- which(stack_rain$Date_dum == date2)
  a <- stack_rain[j1:j2,]
  a$hour <- substr(a$Date_dum,1,13)

  aRain <- a %>%
    dplyr::mutate(ID=1:nrow(a)) %>% # otherwise if over the month, R sort by first the early next month and then late previous month
    dplyr::group_by(hour) %>%
    dplyr::mutate(rainsum=sum(Rain)) %>%
    dplyr::slice(1) %>%
    dplyr::arrange(ID) %>%
    dplyr::select(-ID) %>%
    dplyr::ungroup()

  ###
  ### data not trasmitted ?
  aRain$Date_pos <- as.numeric(as.POSIXct(strptime(aRain$Date_dum, format="%Y-%m-%d %H:%M",tz="UTC")))
  j <- which(diff(aRain$Date_pos, na.rm=TRUE) > 3700) # 3600 seconds, 60 minutes readings

  if (length(j)>0) {
    print("data not trasmitted")
  }

  ###
  ### NA values
  j <- which(is.na(aRain$rainsum))
  if (length(j)>0) {
    print(paste0(length(j), " missing rain values"))
    aRain$rainsum[j] <- 0
  }

  # # find duplicate
  aRain <- aRain %>%
    dplyr::distinct(, .keep_all = TRUE)

  # find change hour if not UTC
  # grouping by hour remove change time
  tmp <- data.frame(as.numeric(substr(aRain$Date_dum, 12,13)))
  names(tmp) <- "test"
  j <- which(diff(tmp$test) > 1)
  if (length(j)>0) {
    j <- c(1, j)
    for (i in 1:(length(j)-1)) {
      tmp1 <- aRain[j[i]:j[i+1],]
      tmp2 <- aRain[j[i+1],]
      tmp3 <- aRain[(j[i+1]+1):nrow(aRain),]
      #
      myedit <-  tmp$test[j[i+1]] + 1
      if (nchar(myedit) == 1) {
        substr(tmp2$Date_dum, 13,13) <- as.character(myedit)
      } else {
        substr(tmp2$Date_dum, 12,13) <- as.character(myedit)
      }
      tmp2$Rain <- 0 # do not add rain
      # leave Tair
      tmp2$hour <- substr(tmp2$Date_dum, 1,13)
      tmp2$rainsum <- 0 # do not add rain
      # leave Date_pos
      #
      aRain <- rbind(tmp1, tmp2, tmp3)
    }
    rm(tmp,tmp1,tmp2,tmp3,i,j)
  }

  # write.table(aRain, file=paste0("../Shiny_Rain_BC_", jname, ".csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  write.table(aRain, file=paste0("../Shiny_Rain_BC_warmup.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)

  # ET
  # do once for ET0 and once for ET0greenhouse
  j1 <- which(stack_rain_ET$Date == date1ET)
  j2 <- which(stack_rain_ET$Date == date2ET)
  a <- stack_rain_ET[j1:j2,]
  #
  a$hour <- substr(a$Date,1,13)
  a <- a %>%
    dplyr::mutate(ID=1:nrow(a)) %>% # otherwise if over the month, R sort by first the early next month and then late previous month
    dplyr::group_by(hour) %>%
    dplyr::mutate(RadSol=round(mean(RadSol),digits=0),
                  Tair=round(mean(Tair),digits=1),
                  RH=round(mean(RH),digits=0),
                  Wind=round(mean(Wind),digits=1)) %>%
    dplyr::slice(1) %>%
    dplyr::arrange(ID) %>%
    dplyr::select(-ID) %>%
    dplyr::ungroup()

  a <- a %>%
    dplyr::select(-hour)

  # find change hour if not UTC
  # grouping by hour remove change time
  tmp <- data.frame(as.numeric(substr(a$Date, 12,13)))
  names(tmp) <- "test"
  j <- which(diff(tmp$test) > 1)
  if (length(j)>0) {
    j <- c(1, j)
    for (i in 1:(length(j)-1)) {
      tmp1 <- a[j[i]:j[i+1],]
      tmp2 <- a[j[i+1],]
      tmp3 <- a[(j[i+1]+1):nrow(a),]
      #
      myedit <-  tmp$test[j[i+1]] + 1
      if (nchar(myedit) == 1) {
        substr(tmp2$Date, 13,13) <- as.character(myedit)
      } else {
        substr(tmp2$Date, 12,13) <- as.character(myedit)
      }
      # leave everything as it is
      #
      a <- rbind(tmp1, tmp2, tmp3)
    }
    rm(tmp,tmp1,tmp2,tmp3,i,j)
  }

  # add these to be consistent with the script to source and calculate a summary for the day
  a$Rain_mm <- 0
  a$Date_pos <- as.numeric(as.POSIXct(strptime(as.character(a$Date), format="%Y-%m-%d %H:%M:%S",tz="UTC")))
  names(a)[which(names(a)=="Tair")] <- "Tmean_C"
  names(a)[which(names(a)=="Wind")] <- "WindSp_ms"
  names(a)[which(names(a)=="RadSol")] <- "RadGlo_Wm2"

  ### missing data?
  j <- which(diff(a$Date_pos, na.rm=TRUE) > 3700) # 3600 seconds, 60 minutes readings

  if (length(j)>0) {
    print("data not trasmitted")
  }

  ###
  ### NA values
  j <- which(is.na(a$RadGlo_Wm2))
  if (length(j) > 0) {
    print(paste0("filling ",length(j), " missing ewather values"))
    jind <- -24 # + - days (24 hours and hourly values)
    # jind <- jind * c(-2,-1,1,2)
    for (i in 1:length(j)) {
      a$Tmean_C[j[i]] <- mean(a$Tmean_C[j[i]+jind], na.rm=T)
      a$RH[j[i]] <- mean(a$RH[j[i]+jind], na.rm=T)
      a$RadGlo_Wm2[j[i]] <- mean(a$RadGlo_Wm2[j[i]+jind], na.rm=T)
      a$ST[j[i]] <- mean(a$ST[j[i]+jind], na.rm=T)
      a$WindSp_ms[j[i]] <- mean(a$WindSp_ms[j[i]+jind], na.rm=T)
    }
    rm(jind,i)
  }


  # ET0 greenhouse
  # FAO56
  ###
  mydf <- a
  a_model <- a %>%
    dplyr::select(c(Date,RadGlo_Wm2,Tmean_C,RH,WindSp_ms,ST))
  rm(a)

  # calculate ET field
  tmplist <- ls() # to remove FAO variables
  source("C:/REWATERING/Rain/data_h/fao_Snyder_DLC_R_Rain_source.R")
  j <- which(is.na(mydf$ET0))
  if (length(j) > 0) {
    mydf$ET0[j] <- 0
    print("setting NA in mydf$ET0 as 0")
  }
  rm(list=setdiff(ls(),tmplist))
  mydfET_out <- mydf %>% dplyr::select(Date,ET0)
  write.table(mydfET_out, file=paste0("../Shiny_ET0_BC_warmup.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  mydf <- mydf %>%
    dplyr::select(-c("Tdew_C","ET0","DOY"))

  # # calculate ET field T effect
  # tmplist <- ls() # to remove FAO variables
  # source("C:/REWATERING/Rain/data_h/fao_Snyder_DLC_R_Rain_source_Teffect.R")
  # j <- which(is.na(mydf$ET0))
  # if (length(j) > 0) {
  #   mydf$ET0[j] <- 0
  #   print("setting NA in mydf$ET0 as 0")
  # }
  # rm(list=setdiff(ls(),tmplist))
  # mydfET_out <- mydf %>% dplyr::select(Date,ET0)
  # # write.table(mydfET_out, file=paste0("../Shiny_ET0_BC_warmup.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  # mydf <- mydf %>%
  #   dplyr::select(-c("Tdew_C","ET0","DOY"))

  # calculate ET indoor climate model
  a_model <- a_model %>%
    dplyr::select(-c(ST))
  write.table(a_model, file=paste0("../ET_climatemodel/Shiny_climate_BC_warmup.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  rm(a_model)
  file.remove("../ET_climatemodel/Shiny_ET_BC_warmup.csv")
  system("matlab -nodisplay -r \"run('../ET_climatemodel/functn_par_istru0_Fernand_warmup'); exit\"")
  while (!file.exists("../ET_climatemodel/Shiny_ET_BC_warmup.csv")) {
    print("waiting 5 seconds for climate model to run")
    Sys.sleep(5)
  }
  filename <- "../ET_climatemodel/Shiny_ET_BC_warmup.csv"
  myAtmbc <- read.csv(filename, header = FALSE, sep = ";", dec = ".", stringsAsFactors = FALSE)
  names(myAtmbc) <- c("Date","Tin","RHin","ET")
  j <- which(is.na(myAtmbc$ET))
  if (length(j) > 0) {
    myAtmbc$ET[j] <- 0
    print("setting NA in ET_h$ET as 0")
  }
  # write
  write.table(myAtmbc, file=paste0("../Shiny_ET0_BC_climatemodel_warmup.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  rm(myAtmbc)
}

###
if (DOexport == 1) {
  ### simulation with
  # with water level data
  date1 <- "2023-10-21 00:00"; date2 <- "2023-10-21 23:50"; date2b <- "2023-10-21 23:55"; jname <- "20231021";
  date1 <- "2023-10-25 00:00"; date2 <- "2023-10-28 23:50"; date2b <- "2023-10-28 23:55"; jname <- "20231025";
  date1 <- "2023-11-01 00:00"; date2 <- "2023-11-11 23:50"; date2b <- "2023-11-11 23:55"; jname <- "20231101";
  date1 <- "2023-11-30 08:59"; date2 <- "2023-12-01 23:59"; date2b <- "2023-12-01 23:59"; jname <- "20231130";
  date1 <- "2023-12-04 23:59"; date2 <- "2023-12-06 23:59"; date2b <- "2023-12-06 23:59"; jname <- "20231205";
  date1 <- "2023-12-08 23:59"; date2 <- "2023-12-10 23:59"; date2b <- "2023-12-10 23:59"; jname <- "20231209";
  # date1 <- "2023-11-30 23:59"; date2 <- "2023-12-20 23:59"; date2b <- "2023-12-20 23:59"; jname <- "20231130_long"; # Long
  date1 <- "2023-12-01 00:09"; date2 <- "2023-12-10 23:59"; date2b <- "2023-12-10 23:59"; jname <- "Sensor2_long"; date1ET <- "2023-12-01 00:00:00"; date2ET <- "2023-12-10 23:00:00"; # Long
  date1 <- "2023-12-01 00:09"; date2 <- "2024-03-22 23:59"; date2b <- "2024-03-22 23:59"; jname <- "Sensor2_long"; date1ET <- "2023-12-01 00:00:00"; date2ET <- "2024-03-22 23:00:00"; # Long
  date1 <- "2024-06-01 00:09"; date2 <- "2024-08-31 23:59"; date2b <- "2024-08-31 23:59"; jname <- "Sensor2_long"; date1ET <- "2024-06-01 00:00:00"; date2ET <- "2024-08-31 23:00:00"; # Long
  date1 <- "2024-09-01 00:09"; date2 <- "2024-10-31 23:59"; date2b <- "2024-10-31 23:59"; jname <- "Sensor2_long"; date1ET <- "2024-09-01 00:00:00"; date2ET <- "2024-10-31 23:00:00"; # Long
  date1 <- "2023-12-01 00:09"; date2 <- "2025-01-31 23:59"; date2b <- "2025-01-31 23:59"; jname <- "Sensor2_long"; date1ET <- "2023-12-01 00:00:00"; date2ET <- "2025-01-31 23:00:00"; # Long
  j1 <- which(stack_rain$Date_dum == date1)
  j2 <- which(stack_rain$Date_dum == date2)
  a <- stack_rain[j1:j2,]
  # a$hour <- rep(0:23,each=6) # from 10 minutes to hours
  a$hour <- substr(a$Date_dum,1,13)
  # # find duplicate
  # adup <- a %>%
  #   group_by(Date_dum) %>%
  #   filter(n()>1)
  aRain <- a %>%
    dplyr::mutate(ID=1:nrow(a)) %>% # otherwise if over the month, R sort by first the early next month and then late previous month
    dplyr::group_by(hour) %>%
    dplyr::mutate(rainsum=sum(Rain)) %>%
    dplyr::slice(1) %>%
    dplyr::arrange(ID) %>%
    dplyr::select(-ID) %>%
    dplyr::ungroup()

  ###
  ### data not transmitted ?
  aRain$Date_pos <- as.numeric(as.POSIXct(strptime(aRain$Date_dum, format="%Y-%m-%d %H:%M",tz="UTC")))
  j <- which(diff(aRain$Date_pos, na.rm=TRUE) > 3700) # 3600 seconds, 60 minutes readings

  if (length(j)>0) {
    print("data not trasmitted")
    # mytime <- as.data.frame(seq(aRain$Date_pos[1], aRain$Date_pos[nrow(tmplong)], by=3600))
    # names(mytime) <- "Date_pos"
    # mytime$Date <- as.character(as.POSIXct(mytime$Date_pos, origin="1970-01-01", tz="UTC"), format="%Y-%m-%d %H:%M:%S")
    # aRain <- aRain %>%
    #   dplyr::select(-Date_pos)
    # aRain <- left_join(mytime , aRain, by=c("Date"))
    #
    # rm(mytime)
  }

  ###
  ### NA values
  j <- which(is.na(aRain$rainsum))
  if (length(j)>0) {
    print(paste0(length(j), " missing rain values"))
    aRain$rainsum[j] <- 0
  }

  # # find duplicate
  aRain <- aRain %>%
    dplyr::distinct(, .keep_all = TRUE)
  # tmp <- data.frame(substr(aRain$Date_dum, 6,10))
  # names(tmp) <- "test"
  # tmp$id <- 1:nrow(tmp)
  # tmp <- tmp %>%
  #   dplyr::group_by(test) %>%
  #   dplyr::mutate(n=length(id)) %>%
  #   dplyr::slice(1) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::arrange(id)

  # find change hour if not UTC
  # grouping by hour remove change time
  tmp <- data.frame(as.numeric(substr(aRain$Date_dum, 12,13)))
  names(tmp) <- "test"
  j <- which(diff(tmp$test) > 1)
  if (length(j)>0) {
    j <- c(1, j)
    for (i in 1:(length(j)-1)) {
      tmp1 <- aRain[j[i]:j[i+1],]
      tmp2 <- aRain[j[i+1],]
      tmp3 <- aRain[(j[i+1]+1):nrow(aRain),]
      #
      myedit <-  tmp$test[j[i+1]] + 1
      if (nchar(myedit) == 1) {
        substr(tmp2$Date_dum, 13,13) <- as.character(myedit)
      } else {
        substr(tmp2$Date_dum, 12,13) <- as.character(myedit)
      }
      tmp2$Rain <- 0 # do not add rain
      # leave Tair
      tmp2$hour <- substr(tmp2$Date_dum, 1,13)
      tmp2$rainsum <- 0 # do not add rain
      # leave Date_pos
      #
      aRain <- rbind(tmp1, tmp2, tmp3)
    }
    rm(tmp,tmp1,tmp2,tmp3,i,j)
  }

  filename <- "../Shiny_Rain_BC.csv"
  if (DOBIND) {
    # filename <- paste0("../Shiny_Rain_BC_", jname, ".csv", sep="")
    aRain_0 <- read.csv(filename, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
    aRain <- rbind(aRain_0,aRain)
    # write.table(aRain, file=filename, dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
    write.table(aRain, file=filename, dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
    rm(aRain_0)
  } else {
    # write.table(aRain, file=paste0("../Shiny_Rain_BC_", jname, ".csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
    write.table(aRain, file=paste0("../Shiny_Rain_BC.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  }

  # Rain SWMM
  # aRain is already bound
  write.table("[TIMESERIES]", file=paste0("../Shiny_Rain_BC_SWMM.csv", sep=""), append = FALSE, dec=".", sep=";", col.names = FALSE, quote=FALSE, row.names = FALSE)
  write.table(";;Name           Date       Time       Value     ", file=paste0("../Shiny_Rain_BC_SWMM.csv", sep=""), append = TRUE, dec=".", sep=";", col.names = FALSE, quote=FALSE, row.names = FALSE)
  write.table(";;-------------- ---------- ---------- ----------", file=paste0("../Shiny_Rain_BC_SWMM.csv", sep=""), append = TRUE, dec=".", sep=";", col.names = FALSE, quote=FALSE, row.names = FALSE)
  mySWMM <- aRain %>% dplyr::select(Date_pos,rainsum)
  mySWMM$Date_pos <- (mySWMM$Date_pos - min(mySWMM$Date_pos))/3600
  mySWMM <- mySWMM %>% dplyr::filter(rainsum>0)
  mySWMM$out <- paste0("TS1                         ", mySWMM$Date_pos, "        ", mySWMM$rainsum, sep="")
  mySWMM <- mySWMM %>% dplyr::select(out)
  write.table(mySWMM, file=paste0("../Shiny_Rain_BC_SWMM.csv", sep=""), append = T, dec=".", sep=";", col.names = FALSE, quote=FALSE, row.names = FALSE)
  rm(mySWMM)

  # WL
  if (date1 == "2023-11-30 08:59") { # start of sensor 2, rains from before. water level was already increasing
    date1 <- "2023-11-30 11:29"
  }
  j1 <- which(stack_data_copy$Date == date1)
  j2 <- which(stack_data_copy$Date == date2b)
  a <- stack_data_copy[j1:j2,]
  # a$hour <- rep(0:23,each=12) # hours
  # aWL <- a %>%
  #   dplyr::group_by(hour) %>%
  #   dplyr::mutate(WLmax=max(WL)) %>%
  #   dplyr::slice(1)
  aWL <- a
  aWL$WL[aWL$EC==0] <- 0
  aWL <- aWL %>%
    dplyr::select(Date,EC,WL)
  filename <- "../Shiny_WL_BC.csv"
  if (DOBIND) {
    # filename <- paste0("../Shiny_WL_BC_", jname, ".csv", sep="")
    aWL_0 <- read.csv(filename, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
    aWL <- rbind(aWL_0,aWL)
    # write.table(aWL, file=paste0("../Shiny_WL_BC_", jname, ".csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
    write.table(aWL, file=paste0("../Shiny_WL_BC.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
    rm(aWL_0)
  } else {
    # write.table(aWL, file=filename, dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
    write.table(aWL, file=filename, dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  }

  # ET
  # do once for ET0 and once for ET0greenhouse
  j1 <- which(stack_rain_ET$Date == date1ET)
  j2 <- which(stack_rain_ET$Date == date2ET)
  a <- stack_rain_ET[j1:j2,]
  # tau for greenhouse
  # tau <- 0.7
  # a$RadSol <- a$RadSol * tau # transmissivity of incoming global solar radiation between 40% and 50%
  #
  a$hour <- substr(a$Date,1,13)
  a <- a %>%
    dplyr::mutate(ID=1:nrow(a)) %>% # otherwise if over the month, R sort by first the early next month and then late previous month
    dplyr::group_by(hour) %>%
    dplyr::mutate(RadSol=round(mean(RadSol),digits=0),
                  Tair=round(mean(Tair),digits=1),
                  RH=round(mean(RH),digits=0),
                  Wind=round(mean(Wind),digits=1)) %>%
    dplyr::slice(1) %>%
    dplyr::arrange(ID) %>%
    dplyr::select(-ID) %>%
    dplyr::ungroup()

  a <- a %>%
    dplyr::select(-hour)

  # find change hour if not UTC
  # grouping by hour remove change time
  tmp <- data.frame(as.numeric(substr(a$Date, 12,13)))
  names(tmp) <- "test"
  j <- which(diff(tmp$test) > 1)
  if (length(j)>0) {
    j <- c(1, j)
    for (i in 1:(length(j)-1)) {
      tmp1 <- a[j[i]:j[i+1],]
      tmp2 <- a[j[i+1],]
      tmp3 <- a[(j[i+1]+1):nrow(a),]
      #
      myedit <-  tmp$test[j[i+1]] + 1
      if (nchar(myedit) == 1) {
        substr(tmp2$Date, 13,13) <- as.character(myedit)
      } else {
        substr(tmp2$Date, 12,13) <- as.character(myedit)
      }
      # leave everything as it is
      #
      a <- rbind(tmp1, tmp2, tmp3)
    }
    rm(tmp,tmp1,tmp2,tmp3,i,j)
  }

  #
  # # write.table(myRAD, file=paste0("myrad.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  #
  # a$Date <- format(as.POSIXct(a$Date_pos, origin="1970-01-01"), format="%Y-%m-%d %H:%M:%S")
  #
  # add these to be consistent with the script to source and calculate a summary for the day
  a$Rain_mm <- 0
  a$Date_pos <- as.numeric(as.POSIXct(strptime(as.character(a$Date), format="%Y-%m-%d %H:%M:%S",tz="UTC")))
  names(a)[which(names(a)=="Tair")] <- "Tmean_C"
  names(a)[which(names(a)=="Wind")] <- "WindSp_ms"
  names(a)[which(names(a)=="RadSol")] <- "RadGlo_Wm2"

  ### missing data?
  j <- which(diff(a$Date_pos, na.rm=TRUE) > 3700) # 3600 seconds, 60 minutes readings

  if (length(j)>0) {
    print("data not trasmitted")
    # mytime <- as.data.frame(seq(aRain$Date_pos[1], aRain$Date_pos[nrow(tmplong)], by=3600))
    # names(mytime) <- "Date_pos"
    # mytime$Date <- as.character(as.POSIXct(mytime$Date_pos, origin="1970-01-01", tz="UTC"), format="%Y-%m-%d %H:%M:%S")
    # aRain <- aRain %>%
    #   dplyr::select(-Date_pos)
    # aRain <- left_join(mytime , aRain, by=c("Date"))
    #
    # rm(mytime)
  }

  ###
  ### NA values
  j <- which(is.na(a$RadGlo_Wm2))
  # j <- which(is.na(a), arr.ind = T)
  if (length(j) > 0) {
    print(paste0("filling ",length(j), " missing weather values"))
    jind <- 24*1 # + - days (24 hours and hourly values)
    jind <- jind * c(-2,-1,1,2)
    for (i in 1:length(j)) {
      # a$Rain[j[i]] <- mean(a$Rain[j[i]+jind], na.rm=T)
      a$Tmean_C[j[i]] <- mean(a$Tmean_C[j[i]+jind], na.rm=T)
      a$RH[j[i]] <- mean(a$RH[j[i]+jind], na.rm=T)
      a$RadGlo_Wm2[j[i]] <- mean(a$RadGlo_Wm2[j[i]+jind], na.rm=T)
      a$ST[j[i]] <- mean(a$ST[j[i]+jind], na.rm=T)
      a$WindSp_ms[j[i]] <- mean(a$WindSp_ms[j[i]+jind], na.rm=T)
    }
    rm(jind,i)
  }


  # ET0 greenhouse
  # FAO56
  ###
  mydf <- a
  a_model <- a %>%
    dplyr::select(c(Date,RadGlo_Wm2,Tmean_C,RH,WindSp_ms,ST))
  rm(a)

  # calculate ET field
  tmplist <- ls() # to remove FAO variables
  source("C:/REWATERING/Rain/data_h/fao_Snyder_DLC_R_Rain_source.R")
  j <- which(is.na(mydf$ET0))
  if (length(j) > 0) {
    mydf$ET0[j] <- 0
    print("setting NA in mydf$ET0 as 0")
  }
  rm(list=setdiff(ls(),tmplist))
  mydfET_out <- mydf %>% dplyr::select(Date,ET0)
  filename <- "../Shiny_ET0_BC.csv"
  if (DOBIND) {
    mydfET_out_0 <- read.csv(filename, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
    mydfET_out <- rbind(mydfET_out_0,mydfET_out)
    write.table(mydfET_out, file=filename, dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
    rm(mydfET_out_0)
  } else {
    write.table(mydfET_out, file=filename, dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  }

  mydf <- mydf %>%
    dplyr::select(-c("Tdew_C","ET0","DOY"))

  # use previous step output values as ic
  filename <- "../Shiny_ET0_BC_climatemodel.csv"
  if (file.exists(filename)) {
    mydfET_in_0 <- read.csv(filename, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
  }

  # calculate ET indoor climate model
  a_model <- a_model %>%
    dplyr::select(-c(ST))
  write.table(a_model, file=paste0("../ET_climatemodel/Shiny_climate_BC.csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  rm(a_model)
  file.remove("../ET_climatemodel/Shiny_ET_BC.csv")
  system("matlab -nodisplay -r \"run('../ET_climatemodel/functn_par_istru0_Fernand'); exit\"")
  while (!file.exists("../ET_climatemodel/Shiny_ET_BC.csv")) {
    print("waiting 5 seconds for climate model to run")
    Sys.sleep(5)
  }
  filename <- "../ET_climatemodel/Shiny_ET_BC.csv"
  myAtmbc <- read.csv(filename, header = FALSE, sep = ";", dec = ".", stringsAsFactors = FALSE)
  names(myAtmbc) <- c("Date","Tin","RHin","ET")
  j <- which(is.na(myAtmbc$ET))
  if (length(j) > 0) {
    myAtmbc$ET[j] <- 0
    print("setting NA in ET_h$ET as 0")
  }
  filename <- "../Shiny_ET0_BC_climatemodel.csv"
  if (DOBIND) {
    mydfET_in <- rbind(mydfET_in_0,myAtmbc)
    # write.table(mydfET_out, file=paste0("../Shiny_ET0_BC_", jname, ".csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
    # mydfET_out <- rbind(mydfET_out , mydfET_out[nrow(mydfET_out),])
    # mydfET_out$ET[4392:6599] <- mydfET_out$ET[4392:6599] / 60
    # mydfET_out$Date[nrow(mydfET_out)] <- "2024-10-31 23:00:00"
    write.table(mydfET_in, file=filename, dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
    rm(mydfET_in_0)
  } else {
    # write.table(mydfET_out, file=paste0("../Shiny_ET0_BC_", jname, ".csv", sep=""), dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
    mydfET_in <- myAtmbc
    write.table(myAtmbc, file=filename, dec=".", sep=";", col.names = TRUE, quote=FALSE, row.names = FALSE)
  }
  rm(myAtmbc)

  par(mar=c(9, 7.5, 1, 6), mfrow=c(1,1))
  plot(cumsum(mydfET_out$ET), type="l")
  lines(cumsum(mydfET_in$ET), col="red")

  par(mar=c(9, 7.5, 1, 6), mfrow=c(1,1))
  plot(cumsum(mydfET_out$ET0) - cumsum(mydfET_in$ET), type="l")

  # #
  # par(mar=c(9, 7.5, 1, 6), mfrow=c(1,1))
  # plot(mydf$Tmean_C, type="l")
  # lines(mydfET_in$Tin, col="red")
  #
  # plot(mydf$RH, type="l")
  # lines(mydfET_in$RHin, col="red")
  #
  # aplot <- data.frame(mydf$Tmean_C)
  # names(aplot) <- "value"
  # aplot$id <- "outdoor"
  # aplot$group <- "T"
  # a <- aplot
  # aplot <- data.frame(mydfET_in$Tin)
  # names(aplot) <- "value"
  # aplot$id <- "indoor"
  # aplot$group <- "T"
  # a <- rbind(a, aplot)
  # ggplot() +
  #   geom_boxplot(a, mapping=aes(x = id, y=value)) +
  #   # xlab("Date") +
  #   # ylab(NULL) +
  #   # scale_size_manual( values = c(3,3,3), guide = "none" ) +
  #   # scale_color_manual( values = c("black","brown3","gold"), labels=c("Kassim","Indoor","Outdoor") )  +
  #   theme_bw() +
  #   theme(
  #     legend.position = "top",
  #     legend.box = "horizontal",
  #     # axis.title.x = element_blank(),
  #     axis.title.x = element_text(size=tsize, hjust = 0.5, color="black"),
  #     axis.title.y = element_text(size=tsize, hjust = 0.5, color="black"),
  #     legend.title = element_text(size=tsize, hjust = 0.5, color="black"),
  #     legend.text = element_text(size=tsize, hjust = 0.5, color="black"),
  #     axis.text.x = element_text(size=tsize, vjust = 0.5, angle = 0, color="black"),
  #     axis.text.y = element_text(size=tsize, hjust = 0.5, color="black"),
  #     strip.background = element_blank(),
  #     strip.text.x = element_blank(),
  #     strip.placement = "outside",
  #     strip.text.y = element_text(size=tsize, hjust = 0.5, color="black")
  #   ) +
  #   # guides(color = guide_legend(override.aes = list(size = 3)))
  #   facet_wrap(~group, scale="free", ncol=2,
  #              strip.position = "left")

  # labeller = as_labeller(c(Inside = "T inside vs T Kassim (°C)", Outside = "T inside vs T outside (°C)" ) ) )# scale = "fixed"
  # ggsave(paste0(nameout, "_boxplot_", nameplot, ".jpg", sep=""), width = 14, height = 6, units = 'in', dpi=300)

  # 3D surface plot with diff ET over T and RH to find hotspot
  #
}


# events
# aa <- stack_data %>%
#      filter(variable %in% "p_CTD") %>% dplyr::mutate(value = value  * 10.1972) %>%
#      dplyr::filter(Date_pos >= 1709885045 + 86400*19 & Date_pos <= 1714938244 - 86400*38)
# plot(aa$Date_pos_r,aa$value)
#
# aa <- stack_data %>%
#      filter(variable %in% "p_CTD") %>% dplyr::mutate(value = value  * 10.1972) %>%
#      dplyr::filter(Date_pos >= 1709885045 + 86400*41 & Date_pos <= 1714938244 - 86400*14)
# plot(aa$Date_pos_r,aa$value)



###
ui <- fluidPage(
  titlePanel(h2("CTD",align = "center")),
  sidebarLayout(
    sidebarPanel(width=2,

                 # selectInput(inputId = "dataset",
                 #             label = "Choose a variable",
                 #             choices = c("p_CTD","T_CTD","EC"),
                 #             selected = "EC")),

                 sliderInput(
                   "slider",
                   "Slider",
                   # min = Sys.Date() - 90,
                   min = as.Date("2023-07-27"),# beginning of monitoring DLC
                   max = Sys.Date() + 1,
                   value =  c(Sys.Date() - 3, Sys.Date() + 1) # show last week DLC
                 ),

                 pickerInput(inputId = "dataset",
                             label = "Choose variable(s)",
                             choices = name_variable,
                             # selected = name_variable[1:3], # to visualize top 3
                             selected = c("Rain","WL","T_CTD","EC"),
                             options = list(`actions-box` = TRUE),
                             multiple = T)#,

                 # sliderInput("ec", "EC", min = 0, max = max(stack_data_copy$EC), value = c(max(stack_data_copy$EC)))
    ),

    mainPanel(
      column(width = 12, heigth=20, class = "well",
             h4("Brush and double-click to zoom"),
             plotOutput("plot1",
                        hover = "plot_hover",
                        dblclick = "plot1_dblclick",
                        brush = brushOpts(
                          id = "plot1_brush",
                          resetOnNew = TRUE
                        )
             ),

             verbatimTextOutput("info"),

             tableOutput("summar")

      )
    )

  )
)







server <- shinyServer(

  function(input,output, session){

    ranges <- reactiveValues(x = NULL, y = NULL)
    # ranges$x <- as.numeric( as.POSIXct(strptime(c(input$slider[[1]], input$slider[[2]]), format="%Y-%m-%d", tz="UTC"), origin="1970-01-01") )

    ## Avoid chain reaction
    reactdelay <- 0 # it was 1, but then does not start from my imposed starting date from the beginning DLC
    change_slider <- reactiveVal(Sys.time())
    change_daterange <- reactiveVal(Sys.time())


    observeEvent(input$slider, {
      if (difftime(Sys.time(), change_slider()) > reactdelay) {
        change_daterange(Sys.time())
        updateDateRangeInput(session,
                             "daterange",
                             # start = input$slider[[1]],
                             # end = input$slider[[2]])
                             ranges$x <- as.numeric( as.POSIXct(strptime(c(input$slider[[1]], input$slider[[2]]), format="%Y-%m-%d", tz="UTC"), origin="1970-01-01") ))
      }
    })



    datasetInput <- reactive({
      stack_data %>%
        filter(variable %in% input$dataset) %>%
        group_by(variable) %>%
        dplyr::mutate(value_norm = value/max(abs(value))) %>%
        dplyr::ungroup()

    })

    datasetInput_note <- reactive({
      stack_data_note %>%
        filter(variable %in% input$dataset) %>%
        group_by(variable) %>%
        dplyr::mutate(value_norm = value/max(abs(value))) %>%
        dplyr::ungroup()

    })


    # # Generate a summary of the dataset ----
    # output$summary <- renderPrint({ # need verbatimTextOutput("summary") in input

    #   dataset <- datasetInput()
    #   summary(dataset$Concentration)
    # })

    # Single zoomable plot (on left)
    # ranges <- reactiveValues(x = NULL, y = NULL)

    output$plot1 <- renderPlot({
      dataset <- datasetInput()

      dataset_note <- datasetInput_note()

      # Filter by time range, if present
      if (!is.null(ranges$x)) {
        dataset <- dataset[dataset$Date_pos_r >= min(ranges$x) & dataset$Date_pos_r <= max(ranges$x), ]
        dataset_note <- dataset_note[dataset_note$Date_pos_r >= min(ranges$x) & dataset_note$Date_pos_r <= max(ranges$x), ]

      }

      # ggplot(dataset, aes(x = Date_pos_r, y=value, col=variable)) +
      ggplot() +
        # scale_x_datetime(breaks = seq(min(dataset$Date_pos_r),max(dataset$Date_pos), length.out=8), labels = as.POSIXct(seq(min(dataset$Date_pos),max(dataset$Date_pos), length.out=8), origin="1970-01-01", tz="UTC")) +
        # ggplot(dataset, aes(x = Date_pos, y=value_norm, col=variable)) +
        # geom_step() +
        geom_point(dataset, mapping=aes(x = Date_pos_r, y=value, col=variable)) +
        geom_point(dataset_note, mapping=aes(x = Date_pos_r, y=value), col="red",size=2) +
        theme(
          # legend.position = "none",
          # axis.title.x = element_blank(),
          axis.title.x = element_text(size=tsize, hjust = 0.5, color="black"),
          axis.title.y = element_text(size=tsize, hjust = 0.5, color="black"),
          legend.title = element_text(size=tsize, hjust = 0.5, color="black"),
          legend.text = element_text(size=tsize, hjust = 0.5, color="black"),
          axis.text.x = element_text(size=tsize, vjust = 0.5, angle = 0, color="black"),
          axis.text.y = element_text(size=tsize, hjust = 0.5, color="black"),
          strip.background = element_blank(),
          strip.text.x = element_blank()
        ) +
        # coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
        facet_wrap(~variable, scale="free", ncol=1) # scale = "fixed"
    })

    # # When a double-click happens, check if there's a brush on the plot.
    # # If so, zoom to the brush bounds; if not, reset the zoom.
    # observeEvent(input$plot1_dblclick, {
    #   brush <- input$plot1_brush
    #   if (!is.null(brush)) {
    #     ranges$x <- c(brush$xmin, brush$xmax)
    #     # ranges$y <- c(brush$ymin, brush$ymax)
    #     ranges$y <- NULL
    #
    #   } else {
    #     ranges$x <- NULL
    #     ranges$y <- NULL
    #   }
    # })

    output$info <- renderPrint({
      # With ggplot2, no need to tell it what the x and y variables are.
      # threshold: set max distance, in pixels
      # maxpoints: maximum number of rows to return
      # addDist: add column with distance, in pixels
      dataset <- datasetInput()

      # # Filter by time range, if present
      # if (!is.null(ranges$x)) {
      #   dataset <- dataset[dataset$Date_pos_r >= min(ranges$x) & dataset$Date_pos_r <= max(ranges$x), ]
      # }

      nearPoints(dataset, input$plot_hover, threshold = 10, maxpoints = 1,
                 addDist = FALSE)
    })


    output$summar <- renderTable({
      # With ggplot2, no need to tell it what the x and y variables are.
      # threshold: set max distance, in pixels
      # maxpoints: maximum number of rows to return
      # addDist: add column with distance, in pixels
      dataset <- datasetInput()

      dataset <- dataset[which(dataset$variable == "Rain"),]

      # Filter by time range, if present
      if (!is.null(ranges$x)) {
        dataset <- dataset[dataset$Date_pos_r >= min(ranges$x) & dataset$Date_pos_r <= max(ranges$x), ]
      }
      paste0("Sum rain ", c(sum(dataset$value,na.rm=TRUE)), " mm", sep="")
    })




  })



shiny::shinyApp(ui = ui, server = server)
