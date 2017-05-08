

# Read the species maturity fork lengths
# Included all species that may be encountered in FWMIS, 
# but only included FL maturity thresholds for game species likely to encounter during 
# watershed surveys in the East Slopes. 
# All other species were given a default of 100mm as a placeholder 
# in case they need to be included in the future
SpeciesMaturityFL.csv<- textConnection(
"Species.Code   ,   MatureFL..mm.
AFJW   ,   100
ARCH   ,   100
ARGR   ,   283
AGMN   ,   283
ARLM   ,   100
BSSN   ,   100
BRMN   ,   100
BRST   ,   100
BKTR   ,   150
BNTR   ,   250
BLTR   ,   150
BLBK   ,   150
BURB   ,   100
CCHL   ,   100
CHSL   ,   100
CTTR   ,   153
CRTR   ,   153
DPSC   ,   100
DLVR   ,   100
EMSH   ,   100
FTMN   ,   100
FNDC   ,   100
FLCH   ,   100
GLTR   ,   100
GOLD   ,   280
GOFS   ,   100
IWDR   ,   100
KOKA   ,   100
LKCH   ,   100
LKST   ,   100
LKTR   ,   567
LKWH   ,   100
LRSC   ,   100
LGPR   ,   100
LNDC   ,   100
LNSC   ,   100
LOTS   ,   100
MOON   ,   100
MNSC   ,   100
MNWH   ,   230
NNST   ,   100
NOCY   ,   100
NRPK   ,   100
NRSQ   ,   100
NRDC   ,   100
PMCH   ,   100
PRDC   ,   100
PRSC   ,   100
PGWH   ,   100
QUIL   ,   100
RNTR   ,   142
RDSH   ,   100
RVSH   ,   100
RNWH   ,   100
SLML   ,   100
SAUG   ,   100
SHRD   ,   100
SHCS   ,   100
SLRD   ,   100
SLSC   ,   100
SMBS   ,   100
SPLA   ,   100
SPSC   ,   100
SPSH   ,   100
SMSC   ,   100
STON   ,   100
THST   ,   100
TRPR   ,   100
CISC   ,   100
TLWH   ,   100
WALL   ,   100
WEMO   ,   100
WSMN   ,   100
WHSC   ,   100
YLPR   ,   100
PRCR   ,   100
ARTR   ,   142
WSCT   ,   153
CRCA   ,   100")

SpeciesMaturityFL <- read.csv(SpeciesMaturityFL.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)
