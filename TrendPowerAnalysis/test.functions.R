# Testing the function

source("functions.R")

# check that workbook exists
fish2016 <- read.FWMIS.workbook(
        workbookName="xxxx",
        watershedName='Nordegg',
        sheetName='Electofishing',
        target.species=target.species)

# check that really is a workbook
fish2016 <- read.FWMIS.workbook(
        workbookName="functions.R",
        watershedName='Nordegg',
        sheetName='Electofishing',
        target.species=target.species)


# bad worksheet name
fish2016 <- read.FWMIS.workbook(
        workbookName="FWMIS Loadform BLTR 2016 Nordegg and Tribs.xls",
        watershedName='Nordegg',
        sheetName='xxxx',
        target.species=target.species)

# bad equipment codes
fish2016 <- read.FWMIS.workbook(
        workbookName="FWMIS Loadform BLTR 2016 Nordegg and Tribs.xls",
        watershedName='Nordegg',
        sheetName='Electrofishing',
        select.equipment='xxx')

# bad select data 
fish2016 <- read.FWMIS.workbook(
        workbookName="FWMIS Loadform BLTR 2016 Nordegg and Tribs.xls",
        watershedName='Nordegg',
        sheetName='Electrofishing',
        select.data='xxx')


