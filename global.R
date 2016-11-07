

pathr<-"W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Code_R/shiny/urgencesapp/www"

addResourcePath("www",pathr)

#save.image("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Code_R/shiny/urgencesapp/www/data.RData")
#load(paste0(pathr,"/urgencesapp.RData")) #contient maintenant le preleaflet2
#rm(pathr,path)

langue<-read.csv(paste0(pathr,"/langue.csv"),header=TRUE,stringsAsFactors=FALSE,sep=";")

la<-"EN"
g<-function(x,l=langue,lan=la){
  l[match(x,l$id),lan]
}

d$sp<-d@data[,paste0("Nom_",la),drop=TRUE]
d$group<-d@data[,paste0("Groupe_",la),drop=TRUE]
biomq$sp<-biomq[,paste0("Nom_",la),drop=TRUE]
peril$sp<-peril@data[,paste0("Nom_",la),drop=TRUE]
he$sp<-he@data[,paste0("Nom_",la),drop=TRUE]
biomq_shp$Nom_Colonie<-biomq$Nom_Colonie[match(biomq_shp$ID_COLONIE,biomq$Id_Colonie)]

#leg_list<-list("Selected region","EPOQ buffer","Bird observations","Species at risk","Breeding colonies","Federal lands","Selected data","Protected areas","EPOQ locations","Municipalities")
leg_list<-as.list(g(paste0("leg",1:12),la="EN"))
names(leg_list)<-as.list(g(paste0("leg",1:12)))

#sec_list<-list("Template from the NWERCP","Location","Migratory birds","Breeding colonies","Seasonal presence","Species of birds at risk","Other species at risk","Federal lands","Important habitats")
sec_list<-as.list(g(paste0("si",1:10),la="EN"))
names(sec_list)<-as.list(g(paste0("si",1:10)))

cc_list<-strsplit(g(langue$id[grep("envoicc|coordonateur",langue$id)]),", ")

bdi<-g(langue$id[grep("bd_|bdi_|bdc_",langue$id)])

#NWERCP_list<-list("A) Title of Advisory and Event Type (e.g die-off of ducks in Lake Erie)","B) Version (of Advisory):","C) Incident:","D) Details of incident: / Détails de l'incident:","E) Initial Assessment of impacts and risk to wildlife:","Updates: Assessment of impacts and risk to wildlife:","Updates: Wildlife Species Involved - potential and actual","Updates: Wildlife Species involved - actual","F) Actions underway / planned: / Mesures prises/planifiées:","Contacts:","Issued by:") 
NWERCP_list<-as.list(g(paste0("temp",1:8)))
#names(NWERCP_list)<-NWERCP_list

lob<-list(1,2,3)
names(lob)<-c(g("ob1"),g("ob2"),g("ob3"))

lct<-list(1,2)
names(lct)<-c(g("ct1"),g("ct2"))

ldd<-list("csv","shp")
names(ldd)<-c(g("downdat1"),g("downdat2"))

lrf<-list(".pdf",".docx",".html")
names(lrf)<-c(g("rf1"),g("rf2"),g("rf3"))

lpi<-list(1,2,3,4)
names(lpi)<-c(g("pi1"),g("pi2"),g("pi3"),g("pi4"))

lgb<-list(1,2)
names(lgb)<-c(g("gb1"),g("gb2"))

nmonth<-g(paste0("mois",1:12))

colopal<-c("green","yellow","red","darkred")


textareaInput <- function(id,label="",value="",placeholder="",rows=10,cols=165,class="form-control"){
  tags$div(
    class="form-group shiny-input-container",
    tags$label(strong(label),'for'=id,class="boxlab"),
    tags$textarea(id=id,rows=rows,class=class,value=value,placeholder=placeholder,cols=cols,style="width:850px;")) #1044px
}