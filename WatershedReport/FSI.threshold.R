# Read the FSI Thresholds
FSI.threshold.csv<- textConnection(
"Species.Code, FSI.num.cat, FSI.cat, lower, upper
ARGR, 1, VHR,   0,   35
ARGR, 2,  HR,  35,  90
ARGR, 3,  MR,  90, 120
ARGR, 4,  LR, 120, 170
ARGR, 5, VLR, 170, 3000
BURB, 1, VHR,   0,   35
BURB, 2,  HR,  35,  90
BURB, 3,  MR,  90, 120
BURB, 4,  LR, 120, 170
BURB, 5, VLR, 170, 3000
CTTR, 1, VHR,   0,   35
CTTR, 2,  HR,  35,  90
CTTR, 3,  MR,  90, 120
CTTR, 4,  LR, 120, 170
CTTR, 5, VLR, 170, 3000
BLTR, 1, VHR,   0,   35
BLTR, 2,  HR,  35,  90
BLTR, 3,  MR,  90, 120
BLTR, 4,  LR, 120, 170
BLTR, 5, VLR, 170, 3000
BKTR, 1, VHR,   0,   35
BKTR, 2,  HR,  35,  90
BKTR, 3,  MR,  90, 120
BKTR, 4,  LR, 120, 170
BKTR, 5, VLR, 170, 3000
BLBK, 1, VHR,   0,   35
BLBK, 2,  HR,  35,  90
BLBK, 3,  MR,  90, 120
BLBK, 4,  LR, 120, 170
BLBK, 5, VLR, 170, 3000
LKCH, 1, VHR,   0,   35
LKCH, 2,  HR,  35,  90
LKCH, 3,  MR,  90, 120
LKCH, 4,  LR, 120, 170
LKCH, 5, VLR, 170, 3000
RNTR, 1, VHR,   0,  35
RNTR, 2,  HR,  35,  90
RNTR, 3,  MR,  90, 120
RNTR, 4,  LR, 120, 170
RNTR, 5, VLR, 170, 3000")

FSI.threshold <- read.csv(FSI.threshold.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)
FSI.cat.unique <- unique(FSI.threshold[,c("FSI.num.cat","FSI.cat")])
FSI.threshold$FSI.cat <- factor( FSI.threshold$FSI.cat, levels= FSI.cat.unique$FSI.cat[ order(FSI.cat.unique$FSI.num.cat)], order=TRUE)
