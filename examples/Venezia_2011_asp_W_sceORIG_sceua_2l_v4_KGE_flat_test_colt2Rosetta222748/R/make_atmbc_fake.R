# rm(list=ls()); .rs.restartR()
setwd("C:/CATHY/cathy1_0/runs/Venezia_2009_ic")

library(dplyr)
library(ggplot2)
# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3

myrow <- 10
mycol <- 13
dx <- 0.1
dy <- 0.1

# substituting NA from rain with 0 mm
# working at hour at the moment for speed

dt_weather <- 5*60 # 5 minutes in seconds
dt_weather <- 60*60 # going to 1 hour

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


### Weather data
# save a mock up example from the real Weather_EB. data to be edited to have irrigation only
# write.table(myRain, paste0("R/Weather_EB.csv", sep=""),
#             append = FALSE, sep = ";", dec = ".", row.names = FALSE, col.names = TRUE)
tmpname <- c("idrow","id","hour","hourcum","ET0","Irr")
dfrow <- 40 * 24 # dt = 1h for 40 days
myAtmbc <- as.data.frame(matrix(NA,nrow=dfrow,ncol=length(tmpname)))
names(myAtmbc) <- tmpname
myAtmbc$idrow <- 1:dfrow
myAtmbc$id <- rep(1:40, each=24)
myAtmbc$hour <- rep(0:23, 40)
myAtmbc$hourcum <- 1:nrow(myAtmbc)

# ET0. 2 mm/d/m2
myk <- c(rep(0,7), seq(0,1,length.out=6), seq(1,0,length.out=6), rep(0,5))
myk <- myk/sum(myk)*2 # times mm/d
myk <- rep(myk,40)
myAtmbc$ET0 <- myk

# no cultivar because cut
# sensor on april 1
# cuts on april 6 and on april 24
myk <- c(rep(1,7), seq(0,1,length.out=7), rep(1,10), seq(0,1,length.out=7), rep(1,9))
myk <- rep(myk,each=24)
myAtmbc$ET0 <- myAtmbc$ET0 * myk
plot(myAtmbc$ET0, type="l")

# Irr
myAtmbc$Irr <- 0
#
# j <- which(myAtmbc$id %in% c(round(seq(1,40,length.out=10))) & myAtmbc$hour == 10)
# myAtmbc$Irr[j] <- sum(myAtmbc$ET0 / length(j))
#
# j <- which(myAtmbc$hourcum %in% c(42, 52, 130, 380, 660, 680, 700))
j <- which(myAtmbc$hourcum %in% c(42, 52, 160, 380, 660, 680, 700))
# myAtmbc$Irr[j] <- sum(myAtmbc$ET0 / length(j))
# myAtmbc$Irr[j] <- c(10,10,6,4,4,4,8)
myAtmbc$Irr[j] <- c(10,10,3,4,3,3,4)
#

myAtmbc$ET0_m3s <- myAtmbc$ET0 / 1e3 / dt_weather # from mm/dt to m/s
myAtmbc$Irr_m3s <- myAtmbc$Irr / 1e3 / dt_weather # from mm/dt to m/s

myAtmbc$Date_pos <- seq(0,86400*39.999, by=3600)
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
         "0" = { # outside domain. imagine rain is the same
           y <- rain
         },
         "1" = { # interrow
           #TODO
           y <- 0
         },
         "2" = { # crop
           #TODO
           y <- - et0 + irr
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


write.table("0 1",
            paste0("R/atmbc_DLC", sep=""),
            append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
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
  
  write.table(myAtmbc$Date_pos[ii]-min(myAtmbc$Date_pos),
              paste0("R/atmbc_DLC", sep=""),
              append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(myNode_atmbc,
              paste0("R/atmbc_DLC", sep=""),
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
