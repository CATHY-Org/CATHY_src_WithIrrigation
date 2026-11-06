
rm(list=ls());
# .rs.restartR()
setwd("C:/CATHY/cathy1_0/runs/Calibration/Venezia_2011_asp_W_sceORIG_sceua_2l_v4_KGE_flat_test_colt2Rosetta222748")

# make sure to have ouput/xyz. To have it, run ./cathy_ft with IPRT1 in input/parm=3

myrow <- 10
mycol <- 13
dx <- 0.1
dy <- 0.1

nameout <- paste0("input/ic", sep="") # postprocessed to match DTM active cells

namexyz <- paste0("output/xyz", sep="")

xyz_header <- read.table(namexyz,
                         head = FALSE,
                         nrows = 1)
NNOD <- xyz_header[1,1]
N <- xyz_header[1,2]


# 4 0 INDP IPOND
# 1.0   WTPOSITION
# 1 non uniform ic (one value read in for each node)

# myfake <- t(round(runif(N),2))

nstr <- N/NNOD
# xyz$V4[seq(1,N,by=NNOD)]
# 0.78  0.73  0.68  0.58  0.48  0.38  0.28  0.18  0.08 -0.02 -0.12 -0.22 -0.32 -0.42 -0.52 -0.62 -0.72 -0.82 -0.92 -1.02 -1.22

# myic <- c(.09,.09,.09    ,.14,.26,.27,.30)
# myic <- c(myic , rep(myic[length(myic)],nstr-length(myic)))

###################################################################################
# select top layer values and then repeat to fill the soil profile
# myic <- c(-1.0E+04,-1.0E+04,-1.0E+04    ,-1.0E+02,-1.0E+02,-1.0E+00,-1.0E+01) * 1
# myic <- c(myic , rep(myic[length(myic)],nstr-length(myic)))
# myfake <- NULL
# for (i in 1:nstr) {
#   myfake <- c(myfake , rep(myic[i], NNOD))
# }
# myfake <- t(myfake)

###################################################################################
# define layer, then repeat across mesh

myic <- seq(-11,-11+0.025*10,length.out=11)
myic <- c(myic, seq(-6,-6+0.025*12,length.out=13))
myic_l <- myic[length(myic)]
myic <- c(myic, seq(myic_l,myic_l+0.1*3,length.out=4))
myic_l <- myic[length(myic)]
myic <- c(myic, seq(myic_l,myic_l+0.5*8,length.out=9))

ic_0 <- -3 # pressure at top
ic_d <- 0.5 # delta pressure between layers
myic <- seq(ic_0,ic_0+0.025*10,length.out=11) # 0.025 is dz
myic_l <- myic[length(myic)]
myic <- c(myic, seq(myic_l+ic_d,myic_l+ic_d+0.025*12,length.out=13))
myic_l <- myic[length(myic)]
myic <- c(myic, seq(myic_l,myic_l+0.1*3,length.out=4))
myic_l <- myic[length(myic)]
myic <- c(myic, seq(myic_l,myic_l+0.5*8,length.out=9))


myfake <- rep(myic , each=NNOD)
myfake <- t(myfake)

write.table("1 0 INDP IPOND",
            nameout,
            append = FALSE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(myfake,
            nameout,
            append = TRUE, sep = " ", dec = ".", row.names = FALSE, col.names = FALSE, quote = FALSE)



