rm(list=ls());
# .rs.restartR()
setwd("C:/CATHY/cathy1_0/runs/Venezia_2011_ic_ET_soil_asp_layers_OneLine_5cm_W_sceORIG_short_Z")

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

# library(dplyr)
# library(ggplot2)
# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3

myrow <- 10
mycol <- 13
dx <- 0.1
dy <- 0.1

dt_weather <- 5*60 # 5 minutes in seconds
dt_weather <- 60*60 # going to 1 hour
dt_weather <- 5*60 # 5 minutes in seconds

nameout <- paste0("input/ic_piston_DLC", sep="") # postprocessed to match DTM active cells

namelanduse <- paste0("input/root_map", sep="") # postprocessed to match DTM active cells
namexyz <- paste0("output/xyz", sep="")

r_landuse <- read.table(namelanduse, header=FALSE, sep=" ", dec = ".", skip=6)
if (is.na(r_landuse[1,1])) {r_landuse <- r_landuse[,-1]}
if (nrow(r_landuse) != myrow | (ncol(r_landuse) != mycol)) {stop("row or col do not match")}

xyz_header <- read.table(namexyz,
                         head = FALSE,
                         nrows = 1)
NNOD <- xyz_header[1,1]
N <- xyz_header[1,2]
Nlayer <- 3 # 1 for atmbc, 2... for subsurface. plastic says that Nlayer=2 has Kz=0

xyz <- read.table(namexyz, header=FALSE, sep="", dec = ".", skip=1)
xyz_p <- xyz[(NNOD*(Nlayer-1)+1) : (NNOD*Nlayer), ]
if (nrow(xyz_p) != NNOD) {stop("nrow do not match")}
xyz_p$V2 <- (xyz_p$V2 + 0) /dx
xyz_p$V3 <- (xyz_p$V3 + 0) /dy



# 4 0 INDP IPOND
# 1.0   WTPOSITION
# 1 non uniform ic (one value read in for each node)

# myfake <- t(round(runif(N),2))

nstr <- N/NNOD

# depth <- 5.0
# zratio <- 
# cumsum(zratio) * depth
# wtdepth <- 1.0

myic <- c(-1.0, -0.95, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0)
myic <- c(-1.5, -1.50, -1.5, -1.5, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0)
myic <- c(-10, -10, -10, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0, -3.0)

myfake <- rep(myic , each=NNOD)
myfake <- t(myfake)

write.table("1 0 INDP IPOND",
            nameout,
            append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(myfake,
            nameout,
            append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)



