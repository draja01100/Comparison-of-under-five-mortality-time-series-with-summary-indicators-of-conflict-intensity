rm(list=ls())

.packages = c("readxl","data.table", "tidyr", "reshape2", "ggplot2", "stringr", "stringi", "stringdist", "plyr", "RColorBrewer", "randomcoloR")

.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

lapply(.packages, require, character.only=TRUE)

getwd()
setwd("V://DemoData//Data Catalog//igme-PITF")

read_all_sheets <- function(filename, rowsToSkip = 0) {
  capture.output(sheets <- excel_sheets(filename),
                 file = "NUL")
  capture.output(  
    x <- lapply(sheets, function(y) read_excel(filename, sheet = y,
                                               skip = rowsToSkip)),
    file = "NULL")
  names(x) <- sheets
  return(x)
}

IGME.Series<- data.table(read.csv("data_U5MR_20150716_updated.csv", header = TRUE, sep = ",", quote="\"", dec=".", fill = TRUE, check.names = TRUE, blank.lines.skip = TRUE, strip.white = TRUE))
IGME.Series <- IGME.Series[, !c("Original.Series.Name","Country.ISO","Start.date.of.Survey", "End.date.of.Survey", "Average.date.of.Survey", "Indicator","Sex", "Series.Category",
                  "Data.Collection.Method", "Model.Life.Table", "Age.Group.of.Women", "Series.Quantity", "Series.Quantity","Year.From","Year.To","Interval",
                  "Standard.Error.of.Estimates","Adjustment","Data.Source","Exclusion.External.Info","Exclusion.Outlier","Exclusion.Old.Data","Exclusion.Old.Data",
                  "Exclusion.Total.U5MR","Notes","Set.As.Minimum","Minimum.Level.of.Completeness","Maximum.Level.of.Completeness","Has.Bias","Fix.Series.Level.Bias",
                  "Date.Of.Data.Added","Contributor","Indirect.method.based.on.TSFB","Method.based.on.Age.Group.of.Women","Temp.Mmadjustment",                 
                  "To.be.adjusted")]  
# unique(IGME.Series$Visible)
IGME.Series <- IGME.Series[Visible=="1",]
names(IGME.Series)
setnames(IGME.Series, c("Country.Name","Country.Code","Series.Name","Series.Year","Series.Type","Reference.Date","Estimates","Inclusion","Visible" ), c("CountryName","ISOCode","Series.Name","Series.Year","Series.Type","Year","MortalityRate","Inclusion","Visible" ))
IGME.Series$Series.Name<- as.character(IGME.Series$Series.Name)
IGME.Series$Series.Year<- as.character(IGME.Series$Series.Year)

#xx<-IGME.Series[Series.Name %like% "Vital"]
IGME.Series[Series.Year=='0', Series.Year:=""]
IGME.Series$Series.Name2 = NA[nrow(IGME.Series)]
nrow<-nrow(IGME.Series)

#If no date in Series.Name then create new name with name+date 

for (i in 1:nrow) {
  # i=384
  if (grepl("\\d{4}", IGME.Series$Series.Name[i])==FALSE & is.na(IGME.Series$Series.Year[i])==FALSE ) {
    
    IGME.Series$Series.Name2[i] <- paste(IGME.Series$Series.Name[i], IGME.Series$Series.Year[i], sep=" ")
    
  }
  
  else {
    
    IGME.Series$Series.Name2[i] <- ""
  }

}

#If series.name2 column not empty then impute value into series.name

for (i in 1:nrow) {
  #i=120
  if (IGME.Series$Series.Name2[i] != "") {
    
    IGME.Series$Series.Name[i] <- IGME.Series$Series.Name2[i]
    
  } else {
    IGME.Series$Series.Name[i] <- IGME.Series$Series.Name[i]
    
  }
  
}

IGME.Series$Series.Name2 <- NULL
IGME.Series$Series.Year<-NULL
IGME.Series<-unique(IGME.Series)

#divide data into groups 
#indirect, direct, vital registration/life table

#unique(IGME.Series$Series.Type)
#str(IGME.Series$Series.Type)
IGME.Series$Series.Type <- as.character(IGME.Series$Series.Type)
IGME.Series[Series.Type %like% "Direct", Series.Type:="Direct"]
IGME.Series[Series.Type %in% c("VR", "Life Table"), Series.Type:="VR/LifeTable"]
setnames(IGME.Series,"Country.Name","CountryName")

IGME.Series$CountryName<-IGME.Series[,trimws(CountryName, which = c("both", "left", "right"))]
IGME.Series[CountryName=="Bosnia & Herzegovina", CountryName:="Bosnia"] 
IGME.Series[CountryName=="Bosnia", ISOCode :="BOS"]
IGME.Series[CountryName=="Lao PR", CountryName :="Laos"]
IGME.Series[CountryName %like% "Russia", CountryName :="Russia"]
IGME.Series[CountryName %like% "Timor Leste", CountryName :="East Timor"]
IGME.Series[CountryName %like% "East Timor", ISOCode :="ETM"]
IGME.Series[CountryName %like% "Gambia", CountryName :="Gambia"]
IGME.Series[CountryName %like% "Gambia", ISOCode :="GAM"]
IGME.Series[CountryName %like% "Trinidad", CountryName := "Trinidad and Tobago"]
IGME.Series[CountryName %like% "Guatemala", ISOCode := "GUA"]
IGME.Series[CountryName %like% "Indonesia", ISOCode := "INS"]
IGME.Series[CountryName %like% "Lebanon", ISOCode := "LEB"]
IGME.Series[CountryName %like% "Libya", ISOCode := "LIB"]
IGME.Series[CountryName %like% "Moldova", ISOCode := "MLD"]
IGME.Series[CountryName %like% "Morocco", ISOCode := "MOR"]
IGME.Series[CountryName %like% "Mozambique", ISOCode := "MOZ"]
IGME.Series[CountryName %like% "Nepal", ISOCode := "NEP"]
IGME.Series[CountryName %like% "Nigeria", ISOCode := "NIG"]
IGME.Series[CountryName %like% "Oman", ISOCode := "OMA"]
IGME.Series[CountryName %like% "Philippines", ISOCode := "PHI"]
IGME.Series[CountryName %like% "Romania", ISOCode := "RUM"]
IGME.Series[CountryName %like% "South Africa", ISOCode := "SAF"]
IGME.Series[CountryName %like% "Sri Lanka", ISOCode := "SRI"]
IGME.Series[CountryName %like% "Sudan pre secession", CountryName := "Sudan"]
IGME.Series[CountryName == "Sudan", ISOCode := "SUD"]
IGME.Series[CountryName == "South Sudan", ISOCode := "SSU"]
IGME.Series[CountryName %like% "Tajikistan", ISOCode := "TAJ"]
IGME.Series[CountryName %like% "Thailand", ISOCode := "THI"]
IGME.Series[CountryName %like% "United Kingdom", ISOCode := "UKG"]
IGME.Series[CountryName %like% "Zambia", ISOCode := "ZAM"]
IGME.Series[CountryName %like% "Zimbabwe", ISOCode := "ZIM"]
IGME.Series[CountryName %like% "Myanmar", ISOCode := "MYA"]
IGME.Series[CountryName %like% "Sierra Leone", ISOCode := "SIE"]
IGME.Series[CountryName %like% "Lao PDR", CountryName := "Laos"]
IGME.Series[CountryName %like% "Cote d Ivoire", CountryName := "Ivory Coast"]
IGME.Series<-unique(IGME.Series)

## check if ISO-countryname match between IGME and comparison sources (like PITF) 
# series.test<-IGME.Series[,1:2]
# series.test<-unique(series.test)
# igmesmoothedtest<-IGME.smoothed[,1:2]
# igmesmoothedtest<-unique(igmesmoothedtest)
# colnames(igmesmoothedtest)[1]<-"ISOsmoothedIGME"
# mergedtest<-merge(igmesmoothedtest,series.test, all.x=TRUE, all.y = TRUE, by="CountryName")
# check<- mergedtest[ISOsmoothedIGME != ISOCode,]
# counttest<-count(mergedtest, vars="CountryName")
# counttest<-count(mergedtest, vars="ISOCode")

LUTable<-IGME.Series[,c(1,2)]
LUTable<-unique(LUTable)
LUTable <- as.data.table(LUTable)

df_list <- split(LUTable, as.factor(LUTable$CountryName))
df_list[145] #145 rwanda for country code/149 for iso
IGME.Series.all[ISOCode:=as.character(ISOCode)]

#import "IGME.smoothed" with smoothed estimates of mortality
IGME.smoothed <- read_excel("RatesDeaths_AllIndicators.xlsx", sheet=1, col_names=TRUE, col_types = NULL, na ="", skip=6)
IGME.smoothed <- IGME.smoothed[,1:201]
IGME.smoothed <- melt(IGME.smoothed)
setnames(IGME.smoothed, c("ISO Code","Uncertainty bounds*","variable","value"), c("ISOCode","UncertaintyBounds","Indicator.Yr","MortalityRate"))
IGME.smoothed$Indicator.Yr <- as.character(IGME.smoothed$Indicator.Yr)
IGME.smoothed$Year <- substr(IGME.smoothed$Indicator.Yr,nchar(IGME.smoothed$Indicator.Yr)-3,nchar(IGME.smoothed))
IGME.smoothed$Indicator <- substr(IGME.smoothed$Indicator.Yr,1,nchar(IGME.smoothed$Indicator.Yr)-5)
IGME.smoothed$Year <- as.numeric(IGME.smoothed$Year)
str(IGME.smoothed)
unique(IGME.smoothed$Year)
unique(IGME.smoothed$MortalityRate)
max((IGME.smoothed$Year))
min(IGME.smoothed$Year)
max(IGME.smoothed$MortalityRate,na.rm=TRUE)
IGME.smoothed$Indicator.Yr <- NULL
IGME.smoothed <- spread(IGME.smoothed,UncertaintyBounds, MortalityRate)
IGME.smoothed <- IGME.smoothed[c(1,2,4,3,5,6,7)]
IGME.smoothed <- as.data.table(IGME.smoothed)

IGME.smoothed$CountryName<-IGME.smoothed[,trimws(CountryName, which = c("both", "left", "right"))]
IGME.smoothed[CountryName=="Bosnia & Herzegovina", CountryName:="Bosnia"] 
IGME.smoothed[CountryName=="Bosnia", ISOCode :="BOS"]
IGME.smoothed[CountryName=="Lao PR", CountryName :="Laos"]
IGME.smoothed[CountryName %like% "Russia", CountryName :="Russia"]
IGME.smoothed[CountryName %like% "Timor Leste", CountryName :="East Timor"]
IGME.smoothed[CountryName %like% "East Timor", ISOCode :="ETM"]
IGME.smoothed[CountryName %like% "Gambia", CountryName :="Gambia"]
IGME.smoothed[CountryName %like% "Gambia", ISOCode :="GAM"]
IGME.smoothed[CountryName %like% "Trinidad", CountryName := "Trinidad and Tobago"]
IGME.smoothed[CountryName %like% "Guatemala", ISOCode := "GUA"]
IGME.smoothed[CountryName %like% "Indonesia", ISOCode := "INS"]
IGME.smoothed[CountryName %like% "Lebanon", ISOCode := "LEB"]
IGME.smoothed[CountryName %like% "Libya", ISOCode := "LIB"]
IGME.smoothed[CountryName %like% "Moldova", ISOCode := "MLD"]
IGME.smoothed[CountryName %like% "Morocco", ISOCode := "MOR"]
IGME.smoothed[CountryName %like% "Mozambique", ISOCode := "MOZ"]
IGME.smoothed[CountryName %like% "Nepal", ISOCode := "NEP"]
IGME.smoothed[CountryName %like% "Nigeria", ISOCode := "NIG"]
IGME.smoothed[CountryName %like% "Oman", ISOCode := "OMA"]
IGME.smoothed[CountryName %like% "Philippines", ISOCode := "PHI"]
IGME.smoothed[CountryName %like% "Romania", ISOCode := "RUM"]
IGME.smoothed[CountryName %like% "South Africa", ISOCode := "SAF"]
IGME.smoothed[CountryName %like% "Sri Lanka", ISOCode := "SRI"]
IGME.smoothed[CountryName == "Sudan", ISOCode := "SUD"]
IGME.smoothed[CountryName == "South Sudan", ISOCode := "SSU"]
IGME.smoothed[CountryName %like% "Tajikistan", ISOCode := "TAJ"]
IGME.smoothed[CountryName %like% "Thailand", ISOCode := "THI"]
IGME.smoothed[CountryName %like% "United Kingdom", ISOCode := "UKG"]
IGME.smoothed[CountryName %like% "Zambia", ISOCode := "ZAM"]
IGME.smoothed[CountryName %like% "Zimbabwe", ISOCode := "ZIM"]
IGME.smoothed[CountryName %like% "Myanmar", ISOCode := "MYA"]
IGME.smoothed[CountryName %like% "Sierra Leone", ISOCode := "SIE"]
IGME.smoothed[CountryName %like% "Lao PDR", CountryName := "Laos"]
IGME.smoothed[CountryName %like% "Cote d Ivoire", CountryName := "Ivory Coast"]
unique(IGME.smoothed$CountryName)
IGME.smoothed <- IGME.smoothed[Indicator == "U5MR",]
IGME.smoothed<-IGME.smoothed[is.na(IGME.smoothed$Median)==FALSE,]


##GENOPOLITICIDE
PITF_Geno <- read_all_sheets("PITF GenoPoliticide 2015.xls")
PITF_Geno <- as.data.table(PITF_Geno)
colnames(PITF_Geno)<- c("CountryName","ISOCode", "CCODE", "Year", "MOBEGIN","YRBEGIN","MOEND","YREND","PTYPE","DEATHMAG","DESC","DESC2")
PITF_Geno<-PITF_Geno[, !c("MOBEGIN", "YRBEGIN", "MOEND","YREND","PTYPE","DESC","DESC2")]
PITF_Geno$CountryName<-PITF_Geno[,trimws(CountryName, which = c("both", "left", "right"))]
unique(PITF_Geno$CountryName)
unique(PITF_Geno$ISOCode)

#no south sudan
#north sudan/sudan already OK
#no korea rep/korea dpr data

PITF_Geno[ISOCode=='ANG',ISOCode := 'AGO']
PITF_Geno[CountryName=="Myanmar (Burma)", CountryName := "Myanmar"]
PITF_Geno[CountryName %like% "Yemen", CountryName := "Yemen"]
PITF_Geno[CountryName %like% "Yemen", ISOCode := "YEM"]
PITF_Geno[CountryName %like% "Pakistan", ISOCode := "PAK"]
PITF_Geno[CountryName %like% "Burundi", ISOCode := "BDI"]
PITF_Geno[CountryName %like% "Cambodia", ISOCode := "KHM"]
PITF_Geno[CountryName %like% "Central African Republic", ISOCode := "CAF"]
PITF_Geno[CountryName %like% "Chad", ISOCode := "TCD"]
PITF_Geno[CountryName %like% "El Salvador", ISOCode := "SLV"]
PITF_Geno[CountryName %like% "Equatorial Guinea", ISOCode := "GNQ"]
PITF_Geno[CountryName %like% "Georgia", ISOCode := "GEO"]
PITF_Geno[CountryName %like% "Algeria", ISOCode := "DZA"]
PITF_Geno[CountryName %like% "Ivory Coast", ISOCode := "CIV"]
PITF_Geno[CountryName %like% "Congo-Kinshasa", CountryName := "Congo DR"]
PITF_Geno[CountryName == "Congo DR", ISOCode := "COD"]
#no congo brazaville
PITF_Geno[CountryName %like% "Congo-Brazaville", CountryName := "Congo"]
PITF_Geno[CountryName == "Congo", ISOCode := "COG"]
PITF_Geno[CountryName == "Vietnam South", CountryName := "Vietnam"]
PITF_Geno[CountryName == "Vietnam", ISOCode := "VNM"]
PITF_Geno<-PITF_Geno[CountryName != "Yugoslavia"]
PITF_Geno[,TYPE := "GEN"]
PITF_Geno$CCODE<-NULL

avgdeathmag<-PITF_Geno
avgdeathmag<-avgdeathmag[,.(DEATHMAG2 = round(mean(DEATHMAG),2)), .(by=Year, CountryName)]
colnames(avgdeathmag)[1]<-"Year"
PITF_Geno<-as.data.table(merge(avgdeathmag, PITF_Geno, by=c("CountryName","Year"), all = TRUE))
PITF_Geno[,DEATHMAG:= DEATHMAG2]
PITF_Geno[,DEATHMAG2:=NULL]
PITF_Geno<-unique(PITF_Geno)

## Check if ISO-countryname match between IGME and comparison sources
# test<-PITF_Geno[,1:2]
# test<-unique(test)
# igmetest<-IGMEMR[,1:2]
# igmetest<-unique(igmetest)
# colnames(igmetest)[1]<-"ISOIGME"
# mergedtest<-merge(igmetest,test, all.x=TRUE, all.y = TRUE, by="CountryName")
# check<- mergedtest[ISOIGME != ISOCode,]
# counttest<-count(mergedtest, vars="CountryName")
# counttest<-count(mergedtest, vars="ISOCode")

##ETHNIC WARS
PITF_EthnicWar<-read_all_sheets("PITF Ethnic War 2015.xls")
#No Korea drp/rep data
PITF_EthnicWar <- as.data.table(PITF_EthnicWar)
#Remove AVEMAG5 at end of vector with future copies of PITF_EthnicWar:
colnames(PITF_EthnicWar)<- c("CountryName","ISOCode", "CCODE", "Year", "MOBEGIN","YRBEGIN","MOEND","YREND","PTYPE","MAGFIGHT","MAGFATAL","MAGAREA","AVEMAG", "DESC","DESC2","AVEMAG5")

#Rescale to 5
PITF_EthnicWar[MAGFATAL==1, MAGFATAL:=.5]
PITF_EthnicWar[MAGFATAL==2, MAGFATAL:=1.25]
PITF_EthnicWar[MAGFATAL==3, MAGFATAL:=2.0]
PITF_EthnicWar[MAGFATAL==4, MAGFATAL:=2.5]
PITF_EthnicWar[MAGFATAL=='9',MAGFATAL:=""]  

PITF_EthnicWar<-PITF_EthnicWar[, !c("AVEMAG","AVEMAG5","MOBEGIN", "YRBEGIN", "MOEND","YREND","MAGFIGHT","PTYPE","DESC","DESC2")]
unique(PITF_EthnicWar$CountryName)
PITF_EthnicWar$CountryName<-PITF_EthnicWar[,trimws(CountryName, which = c("both", "left", "right"))]
#no vietnam data
#no korea data 
#all sudan data  OK except "Sudan-North"
PITF_EthnicWar[CountryName == "Sudan-North", CountryName := "Sudan"]
PITF_EthnicWar[CountryName == "Sudan", ISOCode := "SUD"]
PITF_EthnicWar[ISOCode=="ANG",ISOCode:="AGO"]
PITF_EthnicWar[CountryName=="Myanmar (Burma)", CountryName := "Myanmar"]
PITF_EthnicWar[CountryName %like% "Yemen", CountryName := "Yemen"]
PITF_EthnicWar[CountryName %like% "Yemen", ISOCode := "YEM"]
PITF_EthnicWar[CountryName %like% "Ethiopia", ISOCode := "ETH"]
PITF_EthnicWar[CountryName %like% "Pakistan", ISOCode := "PAK"]
PITF_EthnicWar[CountryName %like% "Bangladesh", ISOCode := "BGD"]
PITF_EthnicWar[CountryName %like% "Burundi", ISOCode := "BDI"]
PITF_EthnicWar[CountryName %like% "Cambodia", ISOCode := "KHM"]
PITF_EthnicWar[CountryName %like% "Central African Republic", ISOCode := "CAF"]
PITF_EthnicWar[CountryName %like% "Chad", ISOCode := "TCD"]
PITF_EthnicWar[CountryName %like% "Croatia", ISOCode := "HRV"]
PITF_EthnicWar[CountryName %like% "El Salvador", ISOCode := "SLV"]
PITF_EthnicWar[CountryName %like% "Georgia", ISOCode := "GEO"]
PITF_EthnicWar[CountryName %like% "Algeria", ISOCode := "DZA"]
PITF_EthnicWar[CountryName %like% "Ivory Coast", ISOCode := "CIV"]
PITF_EthnicWar[CountryName %like% "Congo-Kinshasa", CountryName := "Congo"]
PITF_EthnicWar[CountryName %like% "Congo", ISOCode := "COG"]
#no congo-brazaville/congo dr found in file
PITF_EthnicWar[CountryName %like% "Congo-Brazaville", CountryName := "Congo DR"]
PITF_EthnicWar[CountryName %like% "Congo DR", ISOCode := "COD"]
PITF_EthnicWar[CountryName %like% "Russia", CountryName := "Russia"]
PITF_EthnicWar<-PITF_EthnicWar[CountryName != "Yugoslavia"]
PITF_EthnicWar[MAGFATAL == "9", MAGFATAL := ""]
PITF_EthnicWar[,TYPE := "ETH"]
PITF_EthnicWar$CCODE<-NULL

avgmagfat <-PITF_EthnicWar
avgmagfat <-avgmagfat[,.(MAGFAT2 = round(mean(MAGFATAL),2), MAGAREA2 = round(mean(MAGAREA),2)), .(by=Year, CountryName)]
## weight fatality by area of the country affected
avgmagfat[, magfatarea := 2.5 * (MAGFAT2 * MAGAREA2) / (2.5*4)]
colnames(avgmagfat)[1]<-"Year"
PITF_EthnicWar<-as.data.table(merge(avgmagfat, PITF_EthnicWar, by=c("CountryName","Year"), all = TRUE))
PITF_EthnicWar[,MAGFATAL:= magfatarea]
PITF_EthnicWar[,MAGFAT2:=NULL]
PITF_EthnicWar[,MAGAREA2:=NULL]
PITF_EthnicWar[,magfatarea:=NULL]
PITF_EthnicWar[, MAGAREA:=NULL]
PITF_EthnicWar<-unique(PITF_EthnicWar)


## check if ISO-countryname match between IGME and comparison sources
# test<-PITF_EthnicWar[,1:2]
# test<-unique(test)
# igmetest<-IGMEMR[,1:2]
# igmetest<-unique(igmetest)
# colnames(igmetest)[1]<-"ISOIGME"
# mergedtest<-merge(igmetest,test, all.x=TRUE, all.y = TRUE, by="CountryName")
# check<- mergedtest[ISOIGME != ISOCode,]
# counttest<-count(mergedtest, vars="CountryName")
# counttest<-count(mergedtest, vars="ISOCode")


##REVOLUTIONARY WARS
## PITFRevWar <- read_all_sheets("PITF Revolutionary War 2015.xls")
PITFRevWar <- read_excel("PITF Revolutionary War 2015.xls", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
PITFRevWar<-as.data.table(PITFRevWar)
PITFRevWar<-PITFRevWar[!apply(PITFRevWar == "", 1, all),] #remove empty records
PITFRevWar$AVEMAG5 <- NULL

#Remove AVEMAG5 at end of vector when dealing with future copies of PITFRevWar
colnames(PITFRevWar)<- c("CountryName","ISOCode", "CCODE", "Year", "MOBEGIN","YRBEGIN","MOEND","YREND","PTYPE","MAGFIGHT","MAGFATAL","MAGAREA","AVEMAG", "DESC","DESC2") #,"AVEMAG5" #removed avemag5 at the end
PITFRevWar[MAGFATAL==1, MAGFATAL:=.5]
PITFRevWar[MAGFATAL==2, MAGFATAL:=1.25]
PITFRevWar[MAGFATAL==3, MAGFATAL:=2.0]
PITFRevWar[MAGFATAL==4, MAGFATAL:=2.5]
PITFRevWar[MAGFATAL == 9, MAGFATAL := NA]
#PITF_EthnicWar[MAGFATAL==4, MAGFATAL:=3]

#Remove AVEMAG5 in future copies:
PITFRevWar<-PITFRevWar[, !c("MOBEGIN", "YRBEGIN", "MOEND","YREND", "PTYPE","MAGFIGHT","AVEMAG","PTYPE","DESC","DESC2")]
PITFRevWar$CountryName<-PITFRevWar[,trimws(CountryName, which = c("both", "left", "right"))]
unique(PITFRevWar$CountryName)
unique(PITFRevWar$ISOCode)
PITFRevWar[ISOCode=="ANG",ISOCode:="AGO"]
PITFRevWar[ISOCode=="ANG",ISOCode:="AGO"]
PITFRevWar[CountryName=="Myanmar (Burma)", CountryName := "Myanmar"]
#no sudan data
#no korea data 
PITFRevWar[CountryName == "Sudan", ISOCode := "SUD"]
PITFRevWar[CountryName %like% "Yemen", CountryName := "Yemen"]
PITFRevWar[CountryName %like% "Yemen", ISOCode := "YEM"]
PITFRevWar[CountryName %like% "Pakistan", ISOCode := "PAK"]
PITFRevWar[CountryName %like% "Algeria", ISOCode := "DZA"]
PITFRevWar[CountryName %like% "Cambodia", ISOCode := "KHM"]
PITFRevWar[CountryName %like% "El Salvador", ISOCode := "SLV"]
PITFRevWar[CountryName %like% "Georgia", ISOCode := "GEO"]
PITFRevWar[CountryName %like% "Guinea", ISOCode := "GIN"]
PITFRevWar[CountryName %like% "Chad", ISOCode := "TCA"]
PITFRevWar[CountryName %like% "Guinea-Bissau", ISOCode := "GNB"]
PITFRevWar[CountryName %like% "Chad", ISOCode := "TCD"]
PITFRevWar[CountryName %like% "Mozambique", ISOCode := "MOZ"]
PITFRevWar[CountryName %like% "Ivory Coast", ISOCode := "CIV"]
PITFRevWar[CountryName %like% "Congo-Brazzaville", CountryName := "Congo"]
PITFRevWar[CountryName %like% "Congo", ISOCode := "COG"]
PITFRevWar[CountryName %like% "Congo-Kinshasa", CountryName := "Congo DR"]
PITFRevWar[CountryName %like% "Congo DR", ISOCode := "COD"]
PITFRevWar[CountryName %like% "Vietnam South", CountryName := "Vietnam"]
PITFRevWar[CountryName %like% "Vietnam", ISOCode := "VNM"]
PITFRevWar<-PITFRevWar[CountryName != "Yugoslavia"]


avgmagfat<-PITFRevWar
avgmagfat <-avgmagfat[,.(MAGFAT2 = round(mean(MAGFATAL),2), MAGAREA2 = round(mean(MAGAREA),2)), .(by=Year, CountryName)]
## weight fatality by area of the country affected
avgmagfat[, magfatarea := 2.5 * (MAGFAT2 * MAGAREA2) / (2.5*4)]

colnames(avgmagfat)[1]<-"Year"
PITFRevWar<-as.data.table(merge(avgmagfat, PITFRevWar, by=c("CountryName","Year"), all = TRUE))
PITFRevWar[,MAGFATAL:= magfatarea]
PITFRevWar[,MAGFAT2:=NULL]
PITFRevWar[,MAGAREA2:=NULL]
PITFRevWar[,magfatarea:=NULL]

PITFRevWar[, MAGAREA:=NULL]
PITFRevWar<-unique(PITFRevWar)

## check if ISO-countryname match between IGME and comparison sources
# test<-PITFRevWar[,1:2]
# test<-unique(test)
# igmetest<-IGMEMR[,1:2]
# igmetest<-unique(igmetest)
# colnames(igmetest)[1]<-"ISOIGME"
# mergedtest<-merge(igmetest,test, all.x=TRUE, all.y = TRUE, by="CountryName")
# check<- mergedtest[ISOIGME != ISOCode,]
# counttest<-count(mergedtest, vars="CountryName")
# counttest<-count(mergedtest, vars="ISOCode")

PITFRevWar[,TYPE := "REV"]
PITFRevWar$CCODE<-NULL

## exploring country names
# lut2 <- merge(PITF_Geno, PITF_EthnicWar, by=c("CountryName","ISOCode","Year","TYPE"),all.x=TRUE,all.y = TRUE)
# lut2 <- merge(lut2, PITFRevWar, by=c("CountryName","ISOCode","Year","TYPE","MAGFATAL"),all.x=TRUE,all.y = TRUE)
# lut2 <- merge(lut2, PITFRevWar, by=c("CountryName","ISOCode","Year","TYPE","MAGFATAL"),all.x=TRUE,all.y = TRUE)
# lut2 <- merge(lut2, prio_acd,  by=c("CountryName","ISOCode","Year","TYPE","MAGFATAL"))
# lut2 <- merge(lut2, battledeaths,  by=c("CountryName","ISOCode","Year","TYPE","MAGFATAL"))
# lut2<-unique(lut2[,c(1,2)])
# #count(lut2,vars="CountryName")
# lut3<-merge(lut2,LUTable, by=c("CountryName", "ISOCode"), all=TRUE)
# freqname<-count(lut3,vars="CountryName")
# freqiso<-count(lut3,vars="ISOCode")

##PRIO ACD
prio_acd<-read_excel("ucdp-prio-acd-4-2016.xlsx", sheet=1,col_names = TRUE, col_types=NULL)
str(prio_acd)
prio_acd<-as.data.table(prio_acd)
prio_acd<-prio_acd[, !c("TypeOfConflict","ConflictId","SideA","SideA2nd","SideB","SideBID","SideB2nd","Incompatibility","TerritoryName","CumulativeIntensity","StartDate","StartPrec","StartDate2", "StartPrec2","EpEnd","EpEndDate","EpEndPrec","GWNoA","GWNoA2nd","GWNoB","GWNoB2nd","GWNoLoc","Region","Version")]
prio_acd<-prio_acd[, Location := gsub("\\s*\\([^\\)]+\\)","",as.character(Location ))] #remove parentheses and things in parentheses
colnames(prio_acd)[1]<-c("CountryName")
prio_acd$CountryName<-prio_acd[,trimws(CountryName, which = c("both", "left", "right"))] #remove leading and trailing spaces
prio_acd <- prio_acd[!(CountryName %like% "Hyderabadh"),] #remove records with Hyderabad 
prio_acd<- rbind(prio_acd,prio_acd[CountryName=="North Korea, South Korea"][,c('CountryName') := list("Korea DPR")]) #take subset of prio_acid whose countrynames is "north korea, south korea" and change names fom "north korea, south korea" to "north korea" and bind it to original prio_acd dataset
prio_acd[CountryName=="North Korea, South Korea",CountryName := "Korea Rep"]
prio_acd<-prio_acd[CountryName %in% c("South Vietnam", "South Vietnam, Vietnam"),CountryName := "Vietnam"]
prio_acd<-prio_acd[CountryName %like% "Yemen", CountryName := "Yemen"]
prio_acd<-prio_acd[CountryName == "DR Congo", CountryName := "Congo DR"]
prio_acd<-prio_acd[CountryName %like% "Bosnia", CountryName := "Bosnia"]
prio_acd[CountryName %like% "Rumania", CountryName := "Romania"]
prio_acd<-prio_acd[,TYPE := "ACD"]

##COMPUTATION
#avg of "sudan" with "sudan, south sudan" values
sudlist<-prio_acd[CountryName %in% c("Sudan", "South Sudan, Sudan"),]
sudavg<-sudlist[,.(IntensityLevel2 = round(mean(IntensityLevel),2)), .(by=Year)]
colnames(sudavg)[1]<-"Year"
sudlist<-sudlist[!(CountryName == "South Sudan, Sudan")]
sudlist$IntensityLevel<-NULL
sudlist<-merge(sudlist, sudavg, by="Year", all=TRUE)
#subset of sudan, south sudan, sudan, and south sudan values in prio acid
sudlist <- unique(sudlist)
#remove sudan, south sudan, sudan, and south sudan values in original prio_acd to merge new values for those records
prio_acd<-prio_acd[!(CountryName == "Sudan"),]
#merge ave values of IntensityLevel (IntensityLevel2) with original prio file for Sudan 
prio_acd <- merge(sudlist, prio_acd, by=c("Year","CountryName","TYPE"), all=TRUE)  
prio_acd[CountryName=="Sudan",IntensityLevel := IntensityLevel2]
prio_acd$IntensityLevel2 <- NULL

##COMPUTATION
#avg of "south sudan" with "sudan, south sudan" values
southsudlist<-prio_acd[CountryName %in% c("South Sudan", "South Sudan, Sudan"),]
southsudavg<-southsudlist[,.(IntensityLevel2 = round(mean(IntensityLevel),2)), .(by=Year)]
colnames(southsudavg)[1] <- "Year"
southsudlist <- southsudlist[!(CountryName == "South Sudan, Sudan")]
southsudlist$IntensityLevel <- NULL
southsudlist<- merge(southsudlist, southsudavg, by = "Year", all=TRUE)
southsudlist <- unique(southsudlist)
prio_acd <- prio_acd[!(CountryName=="South Sudan"),]

##COMPUTATION
#merge avg vals of Intensity Level (IntensityLevel2) with orignal prio file for South Sudan
prio_acd <- merge(southsudlist, prio_acd, by = c("Year", "CountryName","TYPE"), all=TRUE)
prio_acd[CountryName=="South Sudan", IntensityLevel := IntensityLevel2]
prio_acd$IntensityLevel2 <- NULL
prio_acd<-prio_acd[!c(grep(",",prio_acd$CountryName,ignore.case=TRUE)),]

##COMPUTATION
#avg intensity for same year and same country
priolist<-prio_acd
avgprio<-priolist[,.(IntensityLevel2 = round(mean(IntensityLevel),2)), .(by=CountryName, Year)]
colnames(avgprio)[1]<-"CountryName"
prio_acd<-merge(avgprio, prio_acd, by=c("CountryName", "Year"), all=TRUE)
prio_acd[,IntensityLevel:=IntensityLevel2]
prio_acd$IntensityLevel2<-NULL
prio_acd<-merge(LUTable, prio_acd, by=c("CountryName"),all.x = FALSE, all.y = TRUE )
prio_acd<-prio_acd[CountryName != "Yugoslavia"]
prio_acd<- unique(prio_acd)

##PRIO BATTLEDEATHS
a.battledeaths<-read_all_sheets("PRIO Battle Deaths Dataset 3.1.xls")
str(a.battledeaths)
names(a.battledeaths)
str(a.battledeaths)
a.battledeaths <- as.data.table(a.battledeaths)
a.battledeaths<-a.battledeaths[,c("bdonly.location","bdonly.year", "bdonly.bdeadbes", "bdonly.bdeadlow", "bdonly.bdeadhig",  "bdonly.int")]
colnames(a.battledeaths) <- c("CountryName", "Year", "bdeadbest", "bdeadlow", "bdeadhigh", "IntensityLevel")

b.battledeaths<-read_excel("ucdp-brd-conf-50-2016.xlsx", sheet=1, col_names=TRUE, col_types=NULL)
names(b.battledeaths)
str(b.battledeaths)
b.battledeaths <- as.data.table(b.battledeaths)
b.battledeaths<-b.battledeaths[,c("LocationInc","Year", "BdBest", "BdLow", "BdHigh")]
colnames(b.battledeaths)<-c("CountryName","Year", "bdeadbest", "bdeadlow", "bdeadhigh")

battledeaths<- merge(a.battledeaths, b.battledeaths, by=c("CountryName","Year","bdeadbest","bdeadlow","bdeadhigh"), all=TRUE)

battledeaths$CountryName<-battledeaths[,trimws(CountryName, which = c("both", "left", "right"))]
battledeaths <- battledeaths[!(CountryName %like% "Hyderabad"),] #remove records with Hyderabad 
battledeaths<- rbind(battledeaths,battledeaths[CountryName=="North Korea, South Korea"][,c('CountryName') := list("Korea DPR")])
battledeaths[CountryName=="North Korea, South Korea",CountryName := "Korea Rep"]
battledeaths<-battledeaths[CountryName %in% c("South Vietnam", "North Vietnam, South Vietnam"),CountryName := "Vietnam"]
battledeaths<-battledeaths[CountryName %in% c("South Yemen", "North Yemen", "North Yemen, South Yemen"), CountryName := "Yemen"]
battledeaths<-battledeaths[CountryName %like% "Bosnia", CountryName := "Bosnia"]
battledeaths<-battledeaths[CountryName %like% "Cote",CountryName := "Ivory Coast"]
battledeaths[CountryName %like% "Turkey", CountryName := "Turkey"]
#Congo is OK
battledeaths[CountryName %like% "Democratic Republic of Congo", CountryName := "Congo DR"]
battledeaths[CountryName %like% "DR Congo", CountryName := "Congo DR"]
battledeaths[CountryName == "Surinam", CountryName := "Suriname"]
battledeaths[CountryName %like% "Rumania", CountryName := "Romania"]
#sudan is OK - all are sudan
unique(battledeaths$CountryName)
battledeaths<-battledeaths[!c(grep(",",battledeaths$CountryName,ignore.case=TRUE)),] #remove records with  a comma
battledeaths<-battledeaths[, CountryName := gsub("\\s*\\([^\\)]+\\)","",as.character(CountryName))] ##remove what's in parenthesis + parentheses
battledeaths<-battledeaths[,TYPE := "BAT"]
battledeaths<-as.data.table(battledeaths)

##COMPUTATION
#sum of battledeaths - Low, High, Best
battledeaths[bdeadbest=="-999", bdeadbest :=""]
battledeaths[grepl("-", battledeaths$bdeadlow, ignore.case=TRUE),bdeadlow:=""]
battledeaths[grepl("-", battledeaths$bdeadhigh, ignore.case=TRUE),bdeadlow:=""]

#Low
sumlow<-battledeaths
sumlow<-sumlow[,.(sumbdeadlow = sum(bdeadlow, na.rm = TRUE)), .(by=Year, CountryName)]
colnames(sumlow)[1]<-"Year"
ayz<-merge(battledeaths, sumlow, by=c("CountryName","Year"), all = TRUE)

#High
sumhigh<-battledeaths
sumhigh<-sumhigh[,.(sumbdeadhigh = sum(bdeadhigh)), .(by=Year, CountryName)]
colnames(sumhigh)[1]<-"Year"
byz<-merge(ayz, sumhigh, by=c("CountryName","Year"), all = TRUE)

#Best
sumbest<-battledeaths
sumbest<-sumbest[,.(sumbdeadbest = sum(bdeadbest)), .(by=Year, CountryName)]
colnames(sumbest)[1]<-"Year"
cyz<-merge(byz, sumbest, by=c("CountryName","Year"), all = TRUE)

cyz$bdeadlow <- cyz$sumbdeadlow
cyz$bdeadhigh <- cyz$sumbdeadhigh 
cyz$bdeadbest <- cyz$sumbdeadbest 
cyz[,c("sumbdeadlow","sumbdeadhigh","sumbdeadbest") := NULL]

battledeaths<-unique(cyz)

##COMPUTATION
#avg bat for avg intenisty for countries with the same IntensityLevel but multiple intenisity points
avgbat<-battledeaths
avgbat<-avgbat[,.(IntensityLevel2 = round(mean(IntensityLevel),2)), .(by=Year, CountryName)]
colnames(avgbat)[1]<-"Year"
battledeaths<-as.data.table(merge(avgbat, battledeaths, by=c("CountryName","Year"), all = TRUE))
battledeaths[,IntensityLevel:= IntensityLevel2]
battledeaths[,IntensityLevel2:=NULL]
battledeaths<-unique(battledeaths)
#imputing ISO codes
battledeaths<-merge(LUTable, battledeaths, by=c("CountryName"),all.x = FALSE, all.y = TRUE )
#removing Yugoslavia
battledeaths<-battledeaths[CountryName != "Yugoslavia"]

PITF_EthnicWar$CountryName<-NULL
PITFRevWar$CountryName<-NULL
PITF_Geno$CountryName<-NULL
prio_acd$CountryName<-NULL
battledeaths$CountryName <- NULL
IGME.Series$CountryName <-NULL

prio_acd[, TYPE := NULL]
battledeaths[,TYPE := NULL]

##COMPUTATION
#Merge prio_acd and battledeaths and then take average of countries with diff int levels for same year
UCPD<-merge(battledeaths, prio_acd, by=c("ISOCode","Year","IntensityLevel"), all=TRUE)
avgint<-UCPD
avgint<-avgint[,.(IntensityLevel2 = round(mean(IntensityLevel),2)), .(by=Year, ISOCode)]
colnames(avgint)[1]<-"Year"
UCPD<-as.data.table(merge(avgint, UCPD, by=c("ISOCode","Year"), all = TRUE))
UCPD[,IntensityLevel:= IntensityLevel2]
UCPD[,IntensityLevel2:=NULL]
UCPD<-unique(UCPD)
UCPD[,bdeadbest:=bdeadbest/1000]
UCPD[,bdeadlow:=bdeadlow/1000]
UCPD[,bdeadhigh:=bdeadhigh/1000]

IGME.Series<-IGME.Series[,Year := trunc(IGME.Series$Year)]
IGME.Series<-IGME.Series[is.na(MortalityRate)==FALSE]
IGME.Series.all <- merge(IGME.Series, PITF_Geno, by=c("ISOCode","Year"),all.x=TRUE,all.y = TRUE)
IGME.Series.all <- merge(IGME.Series.all, PITF_EthnicWar, by=c("ISOCode","Year","TYPE"),all.x=TRUE,all.y = TRUE)
IGME.Series.all <- merge(IGME.Series.all, PITFRevWar, by=c("ISOCode","Year","TYPE","MAGFATAL"),all.x=TRUE,all.y = TRUE)
IGME.Series.all <- merge(IGME.Series.all, UCPD, by=c("ISOCode","Year"), all.x=TRUE, all.y = FALSE, allow.cartesian = TRUE)
IGME.Series.all <- as.data.table(IGME.Series.all)
# IGME.Series.all <- NULL

seriestype <- function(Series.Names, st, whichcol, symbol, linetype) {
  # st <- indirect.country.inc
  # whichcol<- aa
  # linetype=1
  # symbol="19"
  snrow<- nrow(Series.Names)
  for (i in 1:snrow) {
    #i=4
    which.series <- subset(st, st$Series.Name == Series.Names$V1[i])
    lines(which.series$MortalityRate~which.series$Year, col = whichcol[i], lty=linetype)
    points(which.series$MortalityRate~which.series$Year, col=whichcol[i],pch=symbol,cex=.4)
  }
}

unique(IGME.Series.all$Series.Type)
names(IGME.Series.all)
indirect <- IGME.Series.all[Series.Type=="Indirect",]
direct <- IGME.Series.all[Series.Type=="Direct",]
VR.lifetable <- IGME.Series.all[Series.Type=="VR/LifeTable",]

IGME.smooth.graph <- function(IGME.smoothed, IGME.smoothed.country, selection, ISO) {
  G.IGME.smoothed <- plot(IGME.smoothed.country$Upper~IGME.smoothed.country$Year, 
                          type='l', col = '#ABABAD64', #transparent grey instead of plain "grey"
                          xlab="",
                          #ylab="",
                          ylab="Mortality Rate",
                          xlim=c(min(IGME.smoothed.country$Year),2015),
                          ylim=c(0,max(IGME.smoothed.country$Upper, na.rm=TRUE)),#+25
                          xaxt="n",
                          main=paste(selection$CountryName[1], "U5MR",sep=" "))
  lines(IGME.smoothed.country$Lower~IGME.smoothed.country$Year,
        col = '#ABABAD64') #transparent grey instead of plain "grey"
  polygon(c(IGME.smoothed.country$Year, rev(IGME.smoothed.country$Year)), c(IGME.smoothed.country$Upper, rev(IGME.smoothed.country$Lower)),
          col = "#ABABAD64", border = NA)
  lines(IGME.smoothed.country$Median~IGME.smoothed.country$Year, col=mainsmoothed,lwd=2) #blue
  axis(2, at = seq(round_any((min(IGME.smoothed.country$Year,na.rm=TRUE)-10),10), 2015, by = 5), las=2)

  
}

symbol<-c(21,22,23) #outer and inner part of symbol have different colors; circle, square, diamond
thickness=c(1)
backgroundcol="dimgrey"
linewidth=c(1.8)

colors<-brewer.pal(7,"Dark2")
mainsmoothed<-rev(colorRampPalette(brewer.pal(9,"Blues")[9])(1))
pal<-c(mainsmoothed, colors[1], colors[2], colors[4], colors[6]) #red lighter blue lighter && switching the last two indicators
indicators <-function(IGME.Series.all, ISO, IGME.smoothed.country, pal, symbol, backgroundcol, thickness, linewidth){
  
  selectGen <- IGME.Series.all[ISOCode == ISO & TYPE=="GEN" ,]
  selectGen <- unique(selectGen[,c(1,2,10)])
  selectGen <- selectGen[Year >= min(IGME.smoothed.country$Year, na.rm=TRUE),]
  
  yaxis2 <- 0
  
  if  (all(is.na(selectGen$DEATHMAG))==FALSE)  { 
    par(new=T)
    plot(selectGen$DEATHMAG~selectGen$Year,
         pch=symbol[1], #pch=17
         type='p',
         bg= pal[2], 
         col=backgroundcol,
         xlim=c(min(IGME.smoothed.country$Year),2015),
         xaxt="n",
         ylim = c(0,5),
         cex=thickness,
         lwd=linewidth,
         axes=F,
         ylab="",
         xlab="" 
    )
    yaxis2 <- 1
    
  }
  selectEth<- IGME.Series.all[ISOCode == ISO & TYPE=="ETH" ,]
  selectEth <- unique(selectEth[,c(1,2,4)])
  selectEth <- selectEth[Year >= min(IGME.smoothed.country$Year, na.rm=TRUE),]
  
  if  (all(is.na(selectEth$MAGFATAL))==FALSE)  {
    par(new=T)
    plot(selectEth$MAGFATAL~selectEth$Year,
         type='p',
         bg=pal[3],   
         col=backgroundcol,
         pch=symbol[2],
         xlim=c(min(IGME.smoothed.country$Year),2015),
         ylim = c(0,5),
         cex=thickness,
         lwd=linewidth,
         axes=F,
         ylab="",
         xlab=""
    )
    yaxis2 <- 1
    
  }
  
  selectRev<- IGME.Series.all[ISOCode == ISO & TYPE=="REV" ,]
  selectRev <- unique(selectRev[,c(1,2,4)])
  selectRev <- selectRev[Year >= min(IGME.smoothed.country$Year, na.rm=TRUE),]
  
  if  (all(is.na(selectRev$MAGFATAL))==FALSE)  {
    par(new=T)
    plot(selectRev$MAGFATAL~selectRev$Year,
         type='p',
         bg= pal[4],
         col=backgroundcol,
         pch=symbol[2],
         xlim=c(min(IGME.smoothed.country$Year),2015),
         ylim = c(0,5),
         cex=thickness,
         lwd=linewidth,
         axes=F,
         ylab="",
         xlab=""
    )
    yaxis2 <- 1
    
  }
  #intensity level from battledeaths and acd
  selectIntLev <- IGME.Series.all[ISOCode == ISO & is.na(IntensityLevel)==FALSE ,]
  selectIntLev <- unique(selectIntLev[,c(1,2,11)])
  selectIntLev <- selectIntLev[Year >= min(IGME.smoothed.country$Year, na.rm=TRUE),]
  if  (all(is.na(selectIntLev$IntensityLevel))==FALSE)  {
    par(new=T)
    plot(selectIntLev$IntensityLevel~selectIntLev$Year,
         type='p',
         bg=pal[5],
         col=backgroundcol,
         pch=symbol[3],
         xlim=c(min(IGME.smoothed.country$Year),2015),
         xaxt="n",
         ylim = c(0,5),
         cex=thickness,
         lwd=linewidth,
         #lwd = 1.5, #1.5, #2
         axes=F,
         ylab="",
         xlab=""
    )
    yaxis2 <- 1
  }
  
  if (yaxis2==1) {
    axis(4)
    mtext(c("Indicator"),side=4, line=3)
  }
  
  legendtext=c("Mortality Rate", "Genocide-DEATHMAG", "Ethnic War-MAGFATAL", 
               "Revol. War-MAGFATAL", "Conflict Intensity")               
  temp <- legend("topright", legend = legendtext,
                 text.width=strwidth("xxxx Not Included"),
         ncol=1,
         lty = c(1,NA,NA,NA,NA),
         pch=c(NA, symbol[1], symbol[2], symbol[2], symbol[3]),
         col= c(mainsmoothed, backgroundcol, backgroundcol, backgroundcol, backgroundcol),
         pt.bg = c(NA,pal[2],pal[3],pal[4],pal[5]),
         pt.cex = c(NA, 1, 1, 1, 1),
         cex=.75,
         lwd=c(2,NA,NA,NA,NA),
         text.col="black"
         
  )
}


makeplot <- function(df_list, IGME.smoothed, IGME.Series.all, IGME.smooth.graph, 
                     seriestype, indicators, makeplot2) {
  #i=145  #145 #151 #- syria
  selection <- as.data.table(df_list[i])
  colnames(selection)[1] <- "CountryName"
  colnames(selection)[2] <- "ISOCode"
  selection$ISOCode <- as.character(selection$ISOCode)
  ISO <- selection$ISOCode[1]
  par(mfrow=c(2,1), mar=c(5, 4, 3, 4) + 0.1, oma=c(2,1,2,1))
  IGME.smoothed.country <- IGME.smoothed[ISOCode==ISO,]
  series.all.country <- IGME.Series.all[ISOCode == ISO,]
  series.all.country <- series.all.country[Year >= min(Year[is.na(series.all.country$MortalityRate)==FALSE], na.rm=TRUE),]
  series.all.country <- series.all.country[Year >= min(IGME.smoothed.country$Year),]
  if  (((all(is.na(series.all.country$DEATHMAG))==FALSE)|all(is.na(series.all.country$MAGFATAL))==FALSE|all(is.na(series.all.country$IntensityLevel))==FALSE| (series.all.country[is.na(bdeadbest)==FALSE,length(bdeadbest)>1]) == TRUE | (series.all.country[is.na(bdeadlow)==FALSE,length(bdeadlow)>1]) == TRUE |  (series.all.country[is.na(bdeadhigh)==FALSE,length(bdeadhigh)>1]) == TRUE) == TRUE) {
    input <- IGME.smooth.graph(IGME.smoothed, IGME.smoothed.country, selection, ISO)
    #i <-1
    indirect.country<- series.all.country[ISOCode==ISO & Series.Type == "Indirect" & is.na(MortalityRate)==FALSE,]
    rb1<-rev(colorRampPalette(brewer.pal(9,"Greens")[5:7])(nrow(unique(indirect.country[,5]))))
    indirect.country.inc <- indirect.country[Inclusion==1,] 
    Series.Names<- as.data.table(unique(indirect.country.inc$Series.Name))
    input <- seriestype(Series.Names, st=indirect.country.inc, whichcol = rb1[1:nrow(unique(indirect.country.inc[,5]))], symbol= 19, linetype=1) ### this one produces strange lines like +
    
    direct.country<- series.all.country[ISOCode==ISO & Series.Type == "Direct" & is.na(MortalityRate)==FALSE,]
    direct.country.inc <- direct.country[Inclusion==1,] 
    Series.Names<- as.data.table(unique(direct.country.inc$Series.Name))
    rb2<-rev(colorRampPalette(brewer.pal(9,"Blues")[5:7])(nrow(unique(direct.country[,5]))))
    input <- seriestype(Series.Names, st=direct.country.inc, whichcol = rb2[1:nrow(unique(direct.country.inc[,5]))], symbol=17, linetype=1) #### filled in triangles = this is good
    
    VR.lifetable.country<- series.all.country[ISOCode==ISO & Series.Type == "VR/LifeTable" & is.na(MortalityRate)==FALSE,]
    VR.lifetable.country.inc <- VR.lifetable.country[Inclusion==1,] 
    Series.Names<- as.data.table(unique(VR.lifetable.country.inc$Series.Name))
    rb3<-rev(colorRampPalette(brewer.pal(9,"Purples")[5:8])(nrow(unique(VR.lifetable.country[,5]))))
    input <- seriestype(Series.Names, st=VR.lifetable.country.inc, whichcol = rb3[1:nrow(unique(VR.lifetable.country.inc[,5]))], symbol=15, linetype=1)
    
    indirect.country.notinc <- indirect.country[Inclusion==0,] 
    Series.Names<- as.data.table(unique(indirect.country.notinc$Series.Name))
    input <- seriestype(Series.Names, st=indirect.country.notinc, whichcol = rb1[nrow(unique(indirect.country.inc[,5])):nrow(unique(indirect.country.notinc[,5]))], symbol= 19, linetype=2)
    
    direct.country.notinc <- direct.country[Inclusion==0,] 
    Series.Names<- as.data.table(unique(direct.country.notinc$Series.Name))
    input <- seriestype(Series.Names, st=direct.country.notinc, whichcol = rb2[nrow(unique(direct.country.inc[,5])):nrow(unique(direct.country.notinc[,5]))], symbol=17, linetype=2)
    
    VR.lifetable.country.notinc <- VR.lifetable.country[Inclusion==0,] 
    Series.Names<- as.data.table(unique(VR.lifetable.country.notinc$Series.Name))
    input <- seriestype(Series.Names, st=VR.lifetable.country.notinc, whichcol = rb3[nrow(unique(VR.lifetable.country.inc[,5])):nrow(unique(VR.lifetable.country.notinc[,5]))], symbol=15, linetype=2)
    
    axis(1, at = seq(round_any((min(IGME.smoothed.country$Year,na.rm=TRUE)-10),10), 2015, by = 5), las=2)
    mtext(c("Year"),side=1, line=4)
    indicators(IGME.Series.all, ISO, IGME.smoothed.country, pal, symbol, backgroundcol, thickness, linewidth)
    makeplot2(IGME.smoothed.country, series.all.country, ISO,
              indirect.country, seriestype, direct.country,
              VR.lifetable.country, IGME.Series.all, rb1, rb2, rb3)
    
  }
}

#Plot battle deaths - Best, Low, and High againt U5MR
makeplot2 <- function(IGME.smoothed.country, series.all.country, ISO,
                      indirect.country, seriestype, direct.country,
                      VR.lifetable.country, IGME.Series.all, rb1, rb2, rb3) {
  #i=1
  IGME.smoothed <- plot(IGME.smoothed.country$Upper~IGME.smoothed.country$Year,
                          type='l', col = '#ABABAD64', #transparent grey instead of plain "grey"
                          xlab="",
                          ylab="",
                          #ylab="Mortality Rate",
                          xlim=c(min(IGME.smoothed.country$Year),2015),
                          ylim=c(0,max(IGME.smoothed.country$Upper, na.rm=TRUE)),#+25
                          xaxt="n")
  lines(IGME.smoothed.country$Lower~IGME.smoothed.country$Year,
        col = '#ABABAD64') #transparent grey instead of plain "grey"
  polygon(c(IGME.smoothed.country$Year, rev(IGME.smoothed.country$Year)), c(IGME.smoothed.country$Upper, rev(IGME.smoothed.country$Lower)),
          col = "#ABABAD64", border = NA)
  lines(IGME.smoothed.country$Median~IGME.smoothed.country$Year, col=mainsmoothed,lwd=2) #blue
  #i <-1
  
  indirect.country<- series.all.country[ISOCode==ISO & Series.Type == "Indirect" & is.na(MortalityRate)==FALSE,]
  rb1<-rev(colorRampPalette(brewer.pal(9,"Greens")[5:7])(nrow(unique(indirect.country[,5]))))
  indirect.country.inc <- indirect.country[Inclusion==1,] 
  Series.Names<- as.data.table(unique(indirect.country.inc$Series.Name))
  input <- seriestype(Series.Names, st=indirect.country.inc, whichcol = rb1[1:nrow(unique(indirect.country.inc[,5]))], symbol= 19, linetype=1) ### this one produces strange lines like +
  
  direct.country.inc <- direct.country[Inclusion==1,] 
  Series.Names<- as.data.table(unique(direct.country.inc$Series.Name))
  rb2<-rev(colorRampPalette(brewer.pal(9,"Blues")[5:7])(nrow(unique(direct.country[,5]))))
  input <- seriestype(Series.Names, st=direct.country.inc, whichcol = rb2[1:nrow(unique(direct.country.inc[,5]))], symbol=17, linetype=1) #### filled in triangles = this is good
  
  VR.lifetable.country.inc <- VR.lifetable.country[Inclusion==1,] 
  Series.Names<- as.data.table(unique(VR.lifetable.country.inc$Series.Name))
  rb3<-rev(colorRampPalette(brewer.pal(9,"Purples")[5:8])(nrow(unique(VR.lifetable.country[,5]))))
  input <- seriestype(Series.Names, st=VR.lifetable.country.inc, whichcol = rb3[1:nrow(unique(VR.lifetable.country.inc[,5]))], symbol=15, linetype=1)
  
  indirect.country.notinc <- indirect.country[Inclusion==0,] 
  Series.Names<- as.data.table(unique(indirect.country.notinc$Series.Name))
  input <- seriestype(Series.Names, st=indirect.country.notinc, whichcol = rb1[nrow(unique(indirect.country.inc[,5])):nrow(unique(indirect.country.notinc[,5]))], symbol= 19, linetype=2)
  
  direct.country.notinc <- direct.country[Inclusion==0,] 
  Series.Names<- as.data.table(unique(direct.country.notinc$Series.Name))
  input <- seriestype(Series.Names, st=direct.country.notinc, whichcol = rb2[nrow(unique(direct.country.inc[,5])):nrow(unique(direct.country.notinc[,5]))], symbol=17, linetype=2)
  
  VR.lifetable.country.notinc <- VR.lifetable.country[Inclusion==0,] 
  Series.Names<- as.data.table(unique(VR.lifetable.country.notinc$Series.Name))
  input <- seriestype(Series.Names, st=VR.lifetable.country.notinc, whichcol = rb3[nrow(unique(VR.lifetable.country.inc[,5])):nrow(unique(VR.lifetable.country.notinc[,5]))], symbol=15, linetype=2)
  axis(2, at = seq(round_any((min(IGME.smoothed.country$Year,na.rm=TRUE)-10),10), 2015, by = 5), las=2)
  mtext(c("Mortality Rate"),side=2, line=3)
  axis(1, at = seq(round_any((min(IGME.smoothed.country$Year,na.rm=TRUE)-10),10), 2015, by = 5), las=2)
  mtext(c("Year"),side=1, line=4)
  
  selectBatDeaths <- IGME.Series.all[ISOCode==ISO,]
  selectBatDeaths <- unique(selectBatDeaths[,c(1,2,12,13,14)])
  selectBatDeaths <- selectBatDeaths[Year >= min(IGME.smoothed.country$Year, na.rm=TRUE) & is.na(selectBatDeaths$bdeadlow)==FALSE,]
  if  (nrow(selectBatDeaths)>=1)  { 
    par(new=T)
    plot(selectBatDeaths$bdeadhigh~selectBatDeaths$Year,
         type='l',
         col="#89CFF032", 
         xlim=c(min(IGME.smoothed.country$Year, na.rm = TRUE),2015),
         xaxt="n",
         ylim = c(0,max(selectBatDeaths$bdeadhigh, na.rm = TRUE)),
         axes=F,
         ylab="",
         xlab=""
    )
    lines(selectBatDeaths$bdeadlow~selectBatDeaths$Year, col = '#89CFF032') 
    polygon(c(selectBatDeaths$Year, rev(selectBatDeaths$Year)), c(selectBatDeaths$bdeadhigh, rev(selectBatDeaths$bdeadlow)),
            col = "#89CFF032", border = NA)  
    lines(selectBatDeaths$bdeadbest~selectBatDeaths$Year, col='#89cff0',lwd=2) 
    axis(4)
    mtext(c("Battle Deaths (000's)"), side=4, line=3)
    
  }
  
  legend('topright', legend=c("Mortality Rate","Battle Deaths"),
         ncol=1,
         lty=c(1,1),
         pch=c(NA,NA),   
         col=c(mainsmoothed,"#89cff0"), #blue
         cex=.75,
         pt.cex = .9,
         lwd=c(2,2),
         text.col="black"
  )
}

lenlist<-length(df_list)
pdf(file="9-5-17-u5mr-A.pdf", width=8, height=11)
for (i in 1:lenlist) {
  #for (i in 1:1) {
  #i=1
  makeplot(df_list, IGME.smoothed, IGME.Series.all, IGME.smooth.graph, 
           seriestype, indicators, makeplot2)
  
}
dev.off()