
#source("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Code_R/shiny/leaflet6.R")

#.libPaths("W:/UrgencesApp/library")
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
#library(htmltools)
library(DT)
#library(htmlwidgets)
library(rmarkdown)
library(OpenStreetMap)
library(pander) #la fonction p est dans shiny et pander
library(xlsx)


runApp(appDir="W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Code_R/shiny/urgencesapp",launch.browser=TRUE)

### potential new names for the different objects
# bo : bird observation
# fl : federal lands
# bc : breeding colonies
# sr : species at risk
# ch : critical habitats
# pa : protected areas

list.files("//int.ec.gc.ca/sys/InGEO/GW/EC1142ProtAreas_AiresProt/QC_SCF/Habitats/Milieux_humides/Plan_regional_conservation_milieux_humides/Quebec_entier/shp")

setwd("//int.ec.gc.ca/sys/InGEO/GW/EC1142ProtAreas_AiresProt/QC_SCF/Habitats/Milieux_humides/Plan_regional_conservation_milieux_humides/Quebec_entier/shp")
wl<-readOGR(dsn=".",layer="Miilieux_humides_PRCMH_QC_mars2009")

setwd("//int.ec.gc.ca/sys/InGEO/GW/EC1142ProtAreas_AiresProt/QC_SCF/Habitats/Milieux_humides/Plan_regional_conservation_milieux_humides/Quebec_entier/shp")
wl<-readOGR(dsn="Y:/FBolduc",layer="Miilieux_humides_PRCMH_QC_mars2009")

### get the list of necessary packages for urgencesapp

x<-list.files("W:/UrgencesApp/library")

	  









