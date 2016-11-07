
### En premier, rouler le script manips_EPOQ_32bit pour obtenir EPOQ

library(sp)
library(rgdal)
library(plyr)
library(rgeos)
library(shiny)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(sp)
library(rgdal)
library(rgeos)
library(scales)
library(tidyr)
library(plyr)
library(RColorBrewer)
library(htmltools)
library(DT)
library(htmlwidgets)
library(rmarkdown)
library(OpenStreetMap)
library(pander) #la fonction p est dans shiny et pander
library(xlsx)

################################################
#               load data
################################################

### En premier, rouler le script manips_EPOQ_32bit pour obtenir EPOQ our loader le RDtata de epoq
load("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/EPOQ/EPOQ.RData")

#alcidae?

biomq <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/BIOMQ/all_BIOMQ_obs.csv", header=T, sep=";",stringsAsFactors=FALSE)
canardsdemer <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/CanardsMer/all_canardsmer_obs.csv", header=T, sep=";",stringsAsFactors=FALSE)
ecsas <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/ECSAS/all_ECSAS_obs.csv", header=T, sep=";",stringsAsFactors=FALSE)
eiderhiver <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/EiderHiver/all_eiderhiver_obs.csv", header=T, sep=";",stringsAsFactors=FALSE)
garrotshiver <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/GarrotsHiver/all_garrothiver_obs.csv", header=T, sep=";",stringsAsFactors=FALSE)
macreuse  <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/Macreuse/all_macreuse_obs.csv", header=T, sep=";",stringsAsFactors=FALSE)
oies <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/oies/all_oies_obs.csv", header=T, sep=";",stringsAsFactors=FALSE)
sauvagine <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/sauvagine/sauvagine_obs.csv", header=T, sep=";",stringsAsFactors=FALSE)
sriv <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/SRIV/all_SRIV_obs.csv", header=T, sep=";",stringsAsFactors=FALSE)

# pour l'instant le produire avant dans le fichier de manips individuelles
#epoq <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/EPOQ/all_aqua_obs3.csv", header=T, sep=";",stringsAsFactors=FALSE) # pour

# vérifier le contenu de ce fichier
SOSpop <- read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/SOSpop/SOSpop.csv", header=T, sep=";",stringsAsFactors=FALSE) # pour

sf<-read.csv("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/SauvagineFleuve/sauvagine_fleuve.csv",sep=";",header=T,stringsAsFactors=FALSE)

lim1<-read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_inputs/Limicoles/LIMICOLES_IDLM.csv",header=T,sep=",",stringsAsFactors=FALSE)
lim2<-read.table("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_inputs/Limicoles/REKN_BBPL_2013_YT.csv",header=T,sep=",",stringsAsFactors=FALSE)

razo_shp<-readOGR(dsn="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_inputs/Télémétrie",layer="RAZO_shp")
noga_shp<-readOGR(dsn="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_inputs/Télémétrie",layer="NOGA_shp")
rtlo_shp<-readOGR(dsn="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_inputs/Télémétrie",layer="RTLO_shp")

h<-readOGR(dsn="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_inputs/TerresFED",layer="TerresFED_SCF20160301",encoding="UTF-8")
h<-spTransform(h,CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

biomq_shp<-readOGR(dsn="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/BIOMQ",layer="BIOMQ14_polygones",encoding="UTF-8")
biomq_shp<-spTransform(biomq_shp,CRS(proj4string(h)))

wl<-readOGR(dsn="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_inputs/TerresHumides",layer="wl_stl_buff_3000",encoding="UTF-8")
wl<-spTransform(wl,CRS(proj4string(h)))
#buff<-readOGR(dsn="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_inputs/TerresHumides",layer="stl_buff_3000",encoding="UTF-8")
#buff<-spTransform(buff,CRS(proj4string(h))) #buffer de 3km autour du fleuve


he<-readOGR(dsn="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_inputs/HabitatsEssentiels",layer="habitats_essentiels",encoding="UTF-8")
he<-spTransform(he,CRS(proj4string(h)))

ns<-read.csv("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/Atlas/species_grouping.csv",sep=";",header=TRUE,stringsAsFactors=FALSE)

municip.shp<-readOGR(dsn="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_output/Quebec",layer="munic_s",encoding="UTF-8") 
# identify polygon with water
municip.shp@data$MUS_NM_MUN <- as.character(municip.shp@data$MUS_NM_MUN)
municip.shp@data[is.na(municip.shp@data$MUS_NM_MUN),]$MUS_NM_MUN <- "St-Laurent et associés"
municip.shp@data$MUS_NM_MUN <- as.factor(municip.shp@data$MUS_NM_MUN)

### CDPNQ data
perilp<-readOGR(dsn="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Data_inputs/CDPNQ",layer="cdpnq_TerresFED_SCF20160301_s",encoding="UTF-8",use_iconv=TRUE,stringsAsFactors=FALSE)
perilp<-spTransform(perilp,CRS(proj4string(h)))
#Mettre une majuscule au début des noms d'oiseaux
name<-perilp$SCOMNAME[perilp$CLASSE=="Aves"]
perilp$SCOMNAME[perilp$CLASSE=="Aves"]<-paste(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)), sep="")
perilp$SCOMNAME[grep("Arlequin plongeur, pop. de l'Est",perilp$SCOMNAME)]<-"Arlequin plongeur"
#enlever les mentions non pertinentes
perilp<-perilp[perilp$EORANK!="X",]

# 
################################################
#                 BIOMQ
################################################

# il faut absolument transformer tous les formats de dates de dompe, pour l'instant on ne prend que les formats 23/06 et cela peut introduire des dates erronnées et des NA partout

biomq$Base <- "BIOMQ"
biomq$ID<-paste0("BIOMQ",1:nrow(biomq))
biomq$Observer <- NA
biomq$date<-biomq$Date
biomq$Month<-formatC(as.numeric(biomq$Month),width=2,flag="0")
#biomq$date<-ifelse(nchar(biomq$date)==5,paste(biomq$Annee,biomq$date,substr(biomq$date,4,5),substr(biomq$date,1,2),sep="-"),biomq$date)
#biomq$date<-as.character(as.Date(biomq$date))
biomq$Feuillet<-NA

biomq<-biomq[,unique(c("Nom_Colonie","Nom_FR",names(biomq)))]

new.biomq <- subset(biomq, select=c("ID","Base","Nom_Colonie","Annee","Month","date","Observer",
                                    "Chiffre","CentroideX","CentroideY",
                                    "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.biomq)  <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                      "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")
################################################
#                 canardsdemer
################################################
canardsdemer$Base <- "CANARDS_MER"
canardsdemer$ID <- paste0("CANARDS_MER",1:nrow(canardsdemer))
canardsdemer$Date<-with(canardsdemer,paste(Annee,formatC(Mois,wi=2,fl=0),formatC(Jour,wi=2,fl=0),sep="-"))
canardsdemer$Feuillet<-NA

new.canardsmer <- subset(canardsdemer, select=c("ID","Base","NomLieu","Annee","Mois","Date","NomObservateur",
                                               "NombreTotal","LONGITUDE","LATITUDE",
                                               "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))
names(new.canardsmer)  <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                           "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")
################################################
#                 ecsas
################################################

# la facon d'aller chercher la date pourrait donner lieu à des décalages de dates avec les années bisextiles à vérifier

ecsas$Base <-"ECSAS"
ecsas$ID <- paste0("ECSAS",1:nrow(ecsas))
ecsas$NomLieu <- NA
val<-seq.Date(as.Date("2007-01-01"),as.Date("2007-12-31"),by=1)
ecsas$date<-with(ecsas,paste(Year,formatC(Month,wi=2,fl=0),substr(val[Day],9,10),sep="-"))
ecsas$Feuillet<-NA


new.ecsas <- subset(ecsas, select=c("ID","Base","NomLieu","Year","Month","date","ObserverID",
                                                "Count","LongStart","LatStart",
                                                "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.ecsas)  <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                      "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")
################################################
#                 eiderhiver
################################################
eiderhiver$Base <- "EIDERS_HIVER"
eiderhiver$ID <- paste0("EIDERS_HIVER",1:nrow(eiderhiver))
eiderhiver$NomLieu <- NA
eiderhiver$NomObservateur <- NA
eiderhiver$Date<-with(eiderhiver,paste(Annee,formatC(Mois,wi=2,fl=0),substr(val[Jour],9,10),sep="-"))
eiderhiver$Feuillet<-NA

new.eiderhiver <- subset(eiderhiver, select=c("ID","Base","NomLieu","Annee","Mois","Date","NomObservateur",
                                                "NB_TOTAL","LONG","LAT",
                                                "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.eiderhiver)  <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                           "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")
################################################
#               garrotshiver
################################################
garrotshiver$Base <-"GARROTS_HIVER"
garrotshiver$ID <- paste0("GARROTS_HIVER",1:nrow(garrotshiver))
garrotshiver$Month <- as.numeric(substr(garrotshiver$Date,6,7))
garrotshiver$ObserverID <-NA
garrotshiver$Feuillet<-NA
new.garrotshiver <- subset(garrotshiver, select=c("ID","Base","LocalitéRapportTechnique","Année","Month","Date","ObserverID",
                                                  "Nbre","Longitude","Latitude",
                                                  "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.garrotshiver)  <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                             "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")
################################################
#               macreuse
################################################
macreuse$Base <-"MACREUSES"
macreuse$ID <- paste0("MACREUSES",1:nrow(macreuse))
macreuse$Year <- as.numeric(substr(macreuse$Date,1,4))
macreuse$Month <- as.numeric(substr(macreuse$Date,6,7))
macreuse$NomLieu <- NA
macreuse$Feuillet<-NA


new.macreuse<- subset(macreuse, select=c("ID","Base","NomLieu","Year","Month","Date","Observateur_ID",
                                                  "Nombre","Longitude","Latitude",
                                                  "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.macreuse)  <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                         "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")
################################################
#               oies
################################################
oies$Base <-"OIES"
oies$ID <- paste0("OIES",1:nrow(oies))
oies$NomLieu <- as.factor(substr(oies$Nom,1,1))
oies$Année <- NA
oies$Mois <- as.numeric(substr(oies$Date,6,7))
oies$Feuillet<-NA
levels(oies$NomLieu) <- c("Ontario","Sud-ouest","Trois-Rivières/QC","Rive-sud/Matane","Lac St-Jean/Côte nord") 

new.oies<- subset(oies, select=c("ID","Base","NomLieu","Année","Mois","Date","Observateur",
                                         "Nombre","Longitude","Latitude",
                                         "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.oies) <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                    "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")
################################################
#              sriv
################################################
sriv$Base <- "SRIV"
sriv$ID <- paste0("SRIV",1:nrow(sriv))
sriv <- sriv[which(sriv$region=="Rives"),]
sriv <- sriv[!is.na(sriv$obslong),]
sriv$Mois <- as.numeric(substr(sriv$debut,6,7))
sriv$Date<-substr(sriv$debut,1,10)
sriv$Observateur <- NA
sriv$Feuillet<-NA

new.sriv <- subset(sriv, select=c("ID","Base","region3","an","Mois","Date","Observateur",
                                 "total","obslong","obslat",
                                 "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.sriv) <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                    "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")

################################################
#              sauvagine
################################################
sauvagine$Base <- "SAUVAGINE"
sauvagine$ID <- paste0("SAUVAGINE",1:nrow(sauvagine))
sauvagine$Observateur <- NA
sauvagine$Date<-with(sauvagine,paste(ANNEE,formatC(MOIS,wi=2,fl=0),substr(val[JOUR],9,10),sep="-"))
sauvagine$Feuillet<-NA

new.sauvagine <- subset(sauvagine, select=c("ID","Base","NOM_ZONE","ANNEE","MOIS","Date","Observateur",
                                  "NOMBRE","LONG","LAT",
                                  "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.sauvagine) <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                    "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")
################################################
#              epoq
################################################
epoq$Base <- "EPOQ"
epoq$ID <- paste0("EPOQ",1:nrow(epoq))

new.epoq <- subset(epoq, select=c("ID","Base","Lieu","Year","Month","Date","Observateurs","Nombre","Longitude","Latitude","Code","Nom_FR","Nom_EN","Nom_LA","No_Feuillet"))

names(new.epoq) <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long","Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")
################################################
#              SOSpop
################################################
SOSpop$Base <- "SOSpop"
SOSpop$ID <- paste0("SOSpop",1:nrow(SOSpop))
SOSpop$Date<-NA
SOSpop$Feuillet<-NA

new.SOSpop <- subset(SOSpop, select=c("ID","Base","NOM_SITE","ANNEE","MOIS","Date","NOM_OBS","ADULTES","lon","lat","Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.SOSpop) <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long","Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")

################################################
#              Sauvagine Fleuve
################################################


new.sf<-subset(sf, select=c("ID","Base","NomLieu","Année","Mois","Date","observateur",
                                         "Nombre","Longitude","Latitude",
                                         "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.sf) <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                    "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")

################################################
#              Limicoles
################################################
lim1$ID<-paste0(lim1$Base[1],1:nrow(lim1))
lim1$Year<-as.numeric(substr(lim1$Date,1,4))
lim1$Mois<-substr(lim1$Date,6,7)
lim1$Code<-NA
lim1$Nom_FR<-lim1$Espèce
lim1$Nom_EN<-NA
lim1$Nom_LA<-NA
lim1$Feuillet<-NA

new.lim1<-subset(lim1, select=c("ID","Base","Secteur","Year","Mois","Date","Observateurs",
                                         "Abundance","Longitude","Latitude",
                                         "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.lim1) <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                    "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")


lim2$ID<-paste0(lim2$Base[1],1:nrow(lim2))
lim2$Year<-as.numeric(substr(lim2$Date,1,4))
lim2$Mois<-substr(lim2$Date,6,7)
lim2$Code<-NA
lim2$Nom_EN<-NA
lim2$Nom_LA<-NA
lim2$Feuillet<-NA
lim2$Observateur<-NA

new.lim2<-subset(lim2, select=c("ID","Base","Nom.du.site.d.inventaire","Year","Mois","Date","Observateur",
                                         "Abundance","Longitude","Latitude",
                                         "Code","Nom_FR","Nom_EN","Nom_LA","Feuillet"))

names(new.lim2) <-c("ID","Base","Nom_lieu","Anne","Month","Date","Observer","Abundance","Long",
                    "Lat","Sp_code","Nom_FR","Nom_AN","Nom_LA","Feuillet")

###################################################
#    manip Terres FED
###################################################

h<-h[,-c(3:4)] # enlever les superficies et tout
g<-grep("Mingan",h$Nom)
length(g)
nrow(h)

# Mingan est composé de plus de 3000 petits polygones et il est mieux de les fusionner pour l'application et le rapport
mingan<-head(h[grep("Mingan",h$Nom),])
h2<-gUnaryUnion(h[which(h$Nom=="Réserve de parc national de l'archipel-de-Mingan"),])
h2<-SpatialPolygonsDataFrame(h2,data=mingan[1,],match.ID=FALSE)
h2<-spChFIDs(h2,"Mingan")
h<-rbind(h[-g,],h2)
rm(h2,g,mingan)


################################################
#               combine all  
################################################
all.pts <- rbind(new.biomq,
                 new.canardsmer,
                 new.ecsas,
                 new.eiderhiver,
                 new.garrotshiver,
                 new.macreuse,
                 new.sriv,
                 new.oies,
                 new.sauvagine,
                 new.epoq,
                 new.SOSpop,
                 new.sf,
                 new.lim1,
                 new.lim2)

################################################
#               modify columns 
################################################

all.pts$Month<-as.character(formatC(as.numeric(all.pts$Month),width=2,flag=0))
all.pts$Month<-ifelse(all.pts$Month=="NA",NA,all.pts$Month)
all.pts$Groupe_FR<-ns$Groupe_FR[match(all.pts$Nom_FR,ns$Nom_FR)]
all.pts$Groupe_EN<-ns$Groupe_EN[match(all.pts$Nom_FR,ns$Nom_FR)]
all.pts$Taxo<-ns$Taxo[match(all.pts$Nom_FR,ns$Nom_FR)]
all.pts$Nom_FR<-gsub("o","oe",all.pts$Nom_FR)
all.pts$Nom_FR<-gsub("canada","Canada",all.pts$Nom_FR)
all.pts$Nom_FR<-gsub("Bécassine des marais","Bécassine de Wilson",all.pts$Nom_FR)

################################################
#remove points with NA in the Lat and long field
################################################
all.pts <-all.pts[!is.na(all.pts$Long),]
################################################
#             project
################################################

all.pts$x <- all.pts$Long
all.pts$y <- all.pts$Lat
all.pts$Abundance<-as.numeric(all.pts$Abundance)

# keep only aquatic species that are in epoq because other database contain a lot of irrelevant species
elim<-setdiff(all.pts$Nom_FR[all.pts$Base!="EPOQ"],all.pts$Nom_FR[all.pts$Base=="EPOQ"])
elim<-elim[-grep(" sp|Goé|Canard|Gall|Macreuse|Chevalier",elim)]
all.pts<-all.pts[!all.pts$Nom_FR%in%elim,]

coordinates(all.pts) = ~  x + y

### create the main object with bird observations
d<-all.pts
proj4string(d)<-CRS(proj4string(h))
names(d)[which(names(d)=="Nom_AN")]<-"Nom_EN"

### create the set of unique EPOQ locations
epoqxy<-d[d$Base=="EPOQ",]
epoqxy<-epoqxy[!duplicated(epoqxy@data[,c("Lat","Long")]),c("Base")]

### create the set of data for species at risk
peril<-new.SOSpop
coordinates(peril) <- ~ Long+Lat
proj4string(peril)<-CRS(proj4string(h))
names(peril)[which(names(peril)=="Nom_AN")]<-"Nom_EN"

### delete observers from the data
w<-which(names(d)=="Observer")
d<-d[,-w]
w<-which(names(peril)=="Observer")
peril<-peril[,-w]

### remove unnecessary objects
rm(epoq,all.pts,new.epoq,SOSpop,ecsas)
options(scipen=1000)
round(rev(sort(sapply(ls(),function(x){object.size(get(x))})))/(1024*1024),1)
rm(list=ls()[grep("new.",ls())])
rm(list=setdiff(ls(),c("d","epoq","wl","h","rtlo_shp","noga_shp","razo_shp","peril","perilp","municip.shp","biomq_shp","biomq","he","epoqxy")))
gc();gc()


################################################
#               save 
################################################
#save.image("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Code_R/shiny/urgencesapp/www/urgencesapp.RData")
#load("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Code_R/shiny/urgencesapp/www/urgencesapp.RData")





