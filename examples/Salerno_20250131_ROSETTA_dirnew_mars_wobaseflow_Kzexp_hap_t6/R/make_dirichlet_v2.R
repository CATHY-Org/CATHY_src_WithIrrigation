# .rs.restartR()
rm(list=ls())
namefolder<- "Salerno_20250131_ROSETTA_dirnew"
setwd(paste0("C:/UNIPD/CATHY/cathy2_0/REWATERING/", namefolder, sep=""))

# dirichlet fix water table depth in m
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
# library(ggplot2)
# library(reshape2)
# library(raster)
# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3

constantDIRI <- 1

date_st <- "2023-06-01 00:09"
date_en <- "2025-01-31 23:09"

#
myrow <- 72
mycol <- 74
dx <- 80
dy <- 80

dt_weather <- 5*60 # 5 minutes in seconds
dt_weather <- 60*60 # going to 1 hour

#
namexyz <- paste0("output/xyz", sep="")

xyz_header <- read.table(namexyz,
                         head = FALSE,
                         nrows = 1)
if (!exists("xyz_header")) { stop("create file xyz running CATHY with input/parm/IPRT1=3") }
NNOD <- xyz_header[1,1]
N <- xyz_header[1,2]

xyz <- read.table(namexyz, header=FALSE, sep="", dec = ".", skip=1) # do not specify delimiter to treat multiple space as delimiter
xyz$V2 <- (xyz$V2 + 0) /dx
xyz$V3 <- (xyz$V3 + 0) /dy

Nlayer <- N/NNOD
mylayer <- 1
xyz_p <- xyz[(NNOD*(mylayer-1)+1) : (NNOD*mylayer), ]
if (nrow(xyz_p) != NNOD) {stop("nrow do not match")}


##############################################################################
namefile <- "R/node_dirichlet.txt"
node_dirichlet <- read.csv(namefile, header = T, sep = " ", dec = ".", stringsAsFactors = FALSE)
node_dirichlet$colrow <- paste0(node_dirichlet$col, "_", node_dirichlet$row, sep="")
xyz_p$colrow <- paste0(xyz_p$V2, "_", xyz_p$V3, sep="")
node_dirichlet <- left_join(node_dirichlet , xyz_p, by=c("colrow"))
node_dirichlet <- node_dirichlet %>%
  dplyr::arrange(V1)


##############################################################################
# namedem <- paste0("prepro-dem/dem", sep="")
# dem_header <- read.table(namedem,
#                          head = FALSE,
#                          nrows = 6)
# dem <- read.table(namedem, header=FALSE, sep="", dec = ".", skip=6) # do not specify delimiter to treat multiple space as delimiter
# dem <- dem[nrow(dem):1,]
# rownames(dem) <- 1:nrow(dem)
# ################################################################################
# GWd <- 2
# prev0 <- 1
# id_NOD <- NULL
# # basically it goes from top down
# # each row, checks nodes above
# # because top bottom nodes at the boundary are checked outside loop
# i <- 1 # top row
# for (ii in 1:mycol) {
#   if (dem[i,ii] != 0 & prev0 == 1) { #first non 0s cell at the boundary
#     id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii-1)))
#     prev0 <- 0
#   } else if (dem[i,ii] != 0 & prev0 == 0) { # contiguous cell at the boundary...
#     id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii-1)))
#   } else if (dem[i,ii] == 0 & prev0 == 0) { # just terminated series of contiguous non0s cells at the boundary
#     id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii-1)))
#     prev0 <- 1
#   }
# }
# if (dem[i,ii] != 0) { # if reached right boundary and cell has value, then node on the right is BC
#   id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii)))
# }
#
# prev0 <- 1
# for (i in 2:myrow) {
#   for (ii in 1:mycol) {
#     if (dem[i,ii] != 0 & prev0 == 1) { #first non 0s cell
#       id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii-1)))
#       prev0 <- 0
#     } else if (dem[i,ii] != 0 & prev0 == 0 & dem[i-1,ii] == 0) { #contiguous non 0s cell with freedom above
#       id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii-1)))
#     } else if (dem[i,ii] != 0 & prev0 == 0 & dem[i-1,ii] != 0 & ii > 1) { #contiguous non 0s cell with above just occupied
#       if (dem[i-1,ii-1] == 0) {
#         id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii-1)))
#       }
#     } else if (dem[i,ii] == 0 & prev0 == 0) { # just terminated series of contiguous non0s cells at the boundary
#       id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii-1)))
#       prev0 <- 1
#     } else if (dem[i,ii] == 0 & dem[i-1,ii] != 0) { # downside boundary of catchment on the left hand side
#       id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii-1)))
#     } else if (dem[i,ii] == 0 & prev0 == 0 & dem[i-1,ii] == 0 & ii > 1) { # downside boundary of catchment on the right hand side
#       if (dem[i-1,ii-1] != 0) {
#         id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii-1)))
#       }
#     }
#   }
#   if (dem[i,ii] != 0) { # if reached right boundary and cell has value, then node on the right is BC
#     id_NOD <- c(id_NOD , which(xyz_p$V3 == (i-1) & xyz_p$V2 == (ii)))
#   }
# }
#
# i <- myrow # bottom row
# for (ii in 1:mycol) {
#   if (dem[i,ii] != 0 & prev0 == 1) { #first non 0s cell at the boundary
#     id_NOD <- c(id_NOD , which(xyz_p$V3 == (i) & xyz_p$V2 == (ii-1)))
#     prev0 <- 0
#   } else if (dem[i,ii] != 0 & prev0 == 0) { # contiguous cell at the boundary...
#     id_NOD <- c(id_NOD , which(xyz_p$V3 == (i) & xyz_p$V2 == (ii-1)))
#   } else if (dem[i,ii] == 0 & prev0 == 0) { # just terminated series of contiguous non0s cells at the boundary
#     id_NOD <- c(id_NOD , which(xyz_p$V3 == (i) & xyz_p$V2 == (ii-1)))
#     prev0 <- 1
#   }
# }
# if (dem[i,ii] != 0) { # if reached right boundary and cell has value, then node on the right is BC
#   id_NOD <- c(id_NOD , which(xyz_p$V3 == (i) & xyz_p$V2 == (ii)))
# }
#
# ###
# j <- as.data.frame(which(xyz_p$V1 %in% id_NOD))
# names(j) <- "V1"
# j <- left_join(j , xyz_p, by=c("V1"))
# j <- j %>%
#   dplyr::arrange(V4)
# dem_border <- as.data.frame(matrix(0,nrow=nrow(dem)+1,ncol=ncol(dem)+1)) # nodes
# for (i in 1:nrow(j)) {
#   dem_border[j$V2[i]+1,j$V3[i]+1] <- j$V4[i]
# }
# dem_border <- as.data.frame(t(as.matrix(dem_border)))
# # dem_border <- dem_border[,ncol(dem_border):1]
# dem_border <- dem_border[nrow(dem_border):1,]
# write.table("ncols 74",
#             paste0("nansfdirbc_map.asc", sep=""),
#             append = F, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table("nrows 72",
#             paste0("nansfdirbc_map.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table("xllcorner 999082.7992",
#             paste0("nansfdirbc_map.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# # write.table("yllcorner 4503510.2946",
# write.table("yllcorner 4503660.2946",
#             paste0("nansfdirbc_map.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table("cellsize 80",
#             paste0("nansfdirbc_map.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table("NODATA_value 0",
#             paste0("nansfdirbc_map.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table(dem_border,
#             paste0("nansfdirbc_map.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
#
# #
# dem_exclude <- c(5.69037,6.931481,8.06733,8.388097,10.15286,10.48912,10,87621,11,15954,11,36004,11.58799,11.75221,11.6749,11.89193,12.18493,12.08735,12.30438,12.67359,12.93587,13.02818,13.33207,13.7816,14.19393,14.31215,14.58745,14.67807,14.92528,15.73407,15.97731,16.67273,16.90575,17.50111,17.60617,18.14391,18.04798,18.69766,18.48979,19.22138,20.0326,20.34329,20.59133,20.84243,
#   10.01167,10.01577,10.01994,10.02207,10.02617,10.03132,10.03657,10.04064,10.04734,10.05912,10.0665,10.07388,10.0781,10.08293,10.08636,10.10135,11.22916,11.79855,11.93907,12.32239,13.02211,13.83824,14.18992,14.85158,15.10406,15.53893,15.64574,15.27686,15.15725,15.56895,15.69912,16.39353,17.2125,17.69686,17.94017,17.70726,17.89038,17.71766,17.72459,17.76324,17.73846,18.22098,17.93295,18.1879,17.94161,17.94652,18.46706,17.95937,18.24192,17.96977,18.592,18.67794,19.26408,19.90966)
#
# for (i in 1:length(dem_exclude)) {
#   jj <- which(dem_border == dem_exclude[i], arr.ind = T)
#   if (nrow(jj)>=1) {
#     for (ii in 1:nrow(jj)) {
#       dem_border[jj[ii,1],jj[ii,2]] <- 0
#     }
#   }
# }
#
# write.table("ncols 74",
#             paste0("nansfdirbc_map_gradient.asc", sep=""),
#             append = F, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table("nrows 72",
#             paste0("nansfdirbc_map_gradient.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table("xllcorner 999082.7992",
#             paste0("nansfdirbc_map_gradient.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# # write.table("yllcorner 4503510.2946",
# write.table("yllcorner 4503660.2946",
#             paste0("nansfdirbc_map_gradient.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table("cellsize 80",
#             paste0("nansfdirbc_map_gradient.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table("NODATA_value 0",
#             paste0("nansfdirbc_map_gradient.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
# write.table(dem_border,
#             paste0("nansfdirbc_map_gradient.asc", sep=""),
#             append = T, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
#
# jj <- which(j$V4 %in% dem_exclude)
# # jex <- j
# # jex <- jex[-jj,]
# # jex <- jex %>%
# #   arrange(V4)
# id_NOD <- sort(j$V1[jj])

###############################################################################
# which layer has depth that corresponds to GW depth?
# it depends on dz given and GWd
xyz_1 <- xyz[seq(1,nrow(xyz),by=nrow(xyz_p)),]
xyz_1$depth <- xyz_1$V4 - xyz_1$V4[1]

N_at_GW <- head(which(xyz_1$depth <= -2),1) #layer where the soil depth is < 2 m
print(N_at_GW)
dz <- 0.5 # m below 2 m as it is discretized now the z

id_NOD <- as.numeric(node_dirichlet$V1)
mypsi <- 0
psi_NOD <- NULL
psi_pos <- NULL
# psi_pos_surf <- NULL # for checking in QGIS
# psi_pos_surf <- id_NOD
for (i in N_at_GW:Nlayer) {
  psi_NOD <- c(psi_NOD , rep(mypsi, length(id_NOD)))
  psi_pos <- c(psi_pos , id_NOD + NNOD*(i-1) )
  mypsi <- mypsi + dz
}



# ###############################################################################
# ### Weather data warmup
# nameMETEOin <- "input/Shiny_Rain_BC_warmup.csv"
# myRain <- read.csv(nameMETEOin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)
#
# names(myRain)[which(names(myRain) == "Date_dum")] <- "Date"
# names(myRain)[which(names(myRain) == "rainsum")] <- "Rain_mm"
#
# if (!is.na( as.numeric(as.POSIXlt(as.character(myRain$Date[1]), format = "%Y-%m-%d %H:%M")) )) {
#   myRain$Date_pos <- as.numeric(as.POSIXlt(as.character(myRain$Date), format = "%Y-%m-%d %H:%M", tz="CET"))
#   myRain$Date <- as.character(format( as.POSIXct(myRain$Date_pos, origin="1970-01-01", tz="CET"), "%d.%m.%Y %H:%M"))
# } else {
#   myRain$Date_pos <- as.numeric(as.POSIXlt(as.character(myRain$Date), format = "%d.%m.%Y %H:%M", tz="CET"))
# }
#
# myRain <- subset(myRain , select=c("Date","Date_pos","Rain_mm"))
#
# j <- which(is.na(myRain$Date))
# if (length(j)>0) {
#   myRain <- myRain[-j,]
#   print("removing change of solar hours")
#   print("removing NA in Date_pos due to CET and not UTC, j=2907 31.03.2024 02:09")
# }
#
# myAtmbc <- myRain



### Weather data
nameMETEOin <- "input/Shiny_Rain_BC_warmup.csv"
myRain_warmup <- read.csv(nameMETEOin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

nameMETEOin <- "input/Shiny_Rain_BC.csv"
myRain <- read.csv(nameMETEOin, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE)

myRain <- rbind(myRain_warmup,myRain)
rm(myRain_warmup)

names(myRain)[which(names(myRain) == "Date_dum")] <- "Date"
names(myRain)[which(names(myRain) == "rainsum")] <- "Rain_mm"

j1 <- which(myRain$Date == date_st)
j2 <- which(myRain$Date == date_en)
myRain <- myRain[j1:j2,]
rownames(myRain) <- 1:nrow(myRain)

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

# myAtmbc <- rbind(myAtmbc , myRain)
myAtmbc <- myRain

###############################################################################
# warm up
addndays <- 0 * 86400 # in seconds
nrepwarmup <- 1 # how many times to repeat the warm up?
stepndays <- addndays * nrepwarmup / dt_weather

psi_pos <- as.data.frame(t(psi_pos))
psi_NOD <- as.data.frame(t(psi_NOD))
# warm up
if (stepndays > 0) {
  for (iii in 1:nrepwarmup) {
    pb = txtProgressBar(min = 1, max = stepndays/nrepwarmup, initial = 1)
    for (ii in 1:(stepndays/nrepwarmup)) {
      setTxtProgressBar(pb,ii)

      if (ii == 1) {
        write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays*nrepwarmup * (iii-1),
                    paste0("input/nansfdirbc_DLC", sep=""),
                    append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      } else {
        write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays*nrepwarmup * (iii-1),
                    paste0("input/nansfdirbc_DLC", sep=""),
                    append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
      }
      write.table(paste0("0 ", length(psi_pos), sep=""),
                  paste0("input/nansfdirbc_DLC", sep=""),
                  append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

      write.table(psi_pos,
                  paste0("input/nansfdirbc_DLC", sep=""),
                  append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

      write.table(psi_NOD,
                  paste0("input/nansfdirbc_DLC", sep=""),
                  append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    }
  }
  # close(pb)
}

if (constantDIRI == 0) {
  ###############################################################################
  pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1)
  for (ii in 1:nrow(myAtmbc)) {
    setTxtProgressBar(pb,ii)

    write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays*nrepwarmup,
                paste0("input/nansfdirbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

    write.table(paste0("0 ", length(psi_pos), sep=""),
                paste0("input/nansfdirbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

    write.table(psi_pos,
                paste0("input/nansfdirbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

    write.table(psi_NOD,
                paste0("input/nansfdirbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  close(pb)
  print(paste0("NPMAX is: ", length(psi_pos), sep=""))
}

if (constantDIRI == 1) {
  myAtmbc_copy <- myAtmbc
  myAtmbc <- myAtmbc[c(1,nrow(myAtmbc)),]
  ###############################################################################
  pb = txtProgressBar(min = 1, max = nrow(myAtmbc), initial = 1)
  for (ii in 1:nrow(myAtmbc)) {
    setTxtProgressBar(pb,ii)

    if (ii == 1) {
      write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays*nrepwarmup,
                  paste0("input/nansfdirbc_DLC", sep=""),
                  append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    } else {
      write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos) + addndays*nrepwarmup,
                  paste0("input/nansfdirbc_DLC", sep=""),
                  append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
    }

    write.table(paste0("0 ", length(psi_pos), sep=""),
                paste0("input/nansfdirbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

    write.table(psi_pos,
                paste0("input/nansfdirbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)

    write.table(psi_NOD,
                paste0("input/nansfdirbc_DLC", sep=""),
                append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  }
  close(pb)
  print(paste0("NPMAX is: ", length(psi_pos), sep=""))
  myAtmbc <- myAtmbc_copy
}
# to check is 2 m
xyz[21997,]
xyz[1,]
