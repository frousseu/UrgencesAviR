

shinyServer(function(input, output, session) {
	
  gBufferll<-function(x,proj="+proj=utm +zone=18 +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0",...){
    if(!is.projected(x)){
      x2<-spTransform(x,CRS(proj))
    }else{
      x2<-x
    }
    g<-gBuffer(x2,...)
    g<-spTransform(g,CRS(proj4string(x)))
    g
  }
  
  coordinatesall<-function(x){
    res<-as(x,"SpatialPolygons")@polygons
    res<-sapply(res,function(i){i@Polygons})
    re<-seq_along(res)
    ans<-unlist(res,use.names=FALSE)
    names(ans)<-rep(re,sapply(res,length))
    m<-do.call("rbind",lapply(ans,function(i){i@labpt}))
    m<-as.data.frame(m,optional=FALSE)
    m$lab<-as.numeric(row.names(m))
    row.names(m)<-NULL
    names(m)<-c("x","y","lab")
    m
  }
  
  coopar<-function(){
    usr<-par("usr")
    mai<-par("mai")
    omi<-par("omi")
    fin<-par("fin")
    pin<-par("pin")
    din<-par("din")
    w<-usr[2]-usr[1]
    h<-usr[4]-usr[3]
    fw<-w/pin[1]
    fh<-h/pin[2]
    p<-usr
    i<-p+c(-mai[2]*fw,mai[4]*fw,-mai[1]*fh,mai[3]*fh)
    o<-i+c(-omi[2]*fw,omi[4]*fw,-omi[1]*fh,omi[3]*fh)
    list(p=usr,i=i,o=o)
  }
  
	p2pol<-function(i){
		p<-i[c(1:nrow(i),1),]
		p<-p %>% Polygon() %>% list() %>% Polygons(ID=1) %>% list() %>% SpatialPolygons(proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80"))
		p
	}
	
	epoq_calendar<-function(y){
	  feuillet<-ddply(y,.(Month),function(j){
      nb_feuillet<-length(unique(j$Feuillet))
      nb_feuillet
    })
    species<-ddply(y,.(sp,Month),function(j){
      nrow(j)
    })
    month<-formatC(1:12,width=2,flag=0)
    species$p<-species$V1/feuillet$V1[match(species$Month,feuillet$Month)]
    cal<-spread(species[,c("sp","p","Month")],Month,p,fill=0)
    nbfeuillet<-feuillet$V1
    names(nbfeuillet)<-feuillet$Month
    mm<-setdiff(month,names(cal))
    if(length(mm)>0){
      fill<-as.data.frame(sapply(mm,function(x){as.numeric(rep(NA,nrow(cal)))}))
      cal<-cbind(cal,fill)
      cal<-cal[,c("sp",sort(names(cal)[-1]))]
      temp<-rep(0,length(mm))
      names(temp)<-mm
      nbfeuillet<-c(nbfeuillet,temp)
      nbfeuillet<-nbfeuillet[order(names(nbfeuillet))]
    }
    mentions<-table(y$sp)
    cal<-list(cal=cal,mentions=mentions,nbfeuillet=nbfeuillet,data=y)
    cal
  }
	
	clear_map<-function(){
	    leafletProxy("map",deferUntilFlush=FALSE) %>% 
        clearGroup("Federal lands") %>% 
        clearGroup("Selected area") %>% 
        clearGroup("EPOQ buffer") %>% 
        clearGroup("Birds at risk") %>% 
	      clearGroup("Species at risk") %>% 
        clearGroup("Bird observations") %>% 
	      clearGroup("region_line") %>% 
	      clearGroup("Wetlands") %>% 
	      clearGroup("Selected data") %>% 
	      clearGroup("Breeding colonies") %>% 
	      clearGroup("Critical habitats") %>% 
	      #clearGroup("target") %>% 
	      removeMarker(layerId="single_marker") #%>% 
	      #clearMarkers()
	}
	
	clear_selection<-function(){    
	    df$df<-NULL
      hdf$hdf<-NULL
      region$region<-NULL
      dfperil$dfperil<-NULL
      dfperil$dfperilp<-NULL
      bc$bc<-NULL
      bc$bcobs<-NULL
      buffer$buffer<-NULL
      ap$ap<-NULL
      ch$ch<-NULL
      ll$x<-NULL
      ll$y<-NULL
	}
	
	optionsDT<-list(
    pageLength = 5,
    lengthMenu = c(5, 10, 15, 20, 50, 100)
  )
	
	region<-reactiveValues(region=NULL)
	ll<-reactiveValues(x=NULL,y=NULL) #click sur la map
	df<-reactiveValues(df=NULL)       #observations d'oiseaux
	hdf<-reactiveValues(hdf=NULL)     #terres fédérales
	ap<-reactiveValues(ap=NULL)       #aires protégées
	bc<-reactiveValues(bc=NULL,bcobs=NULL)       #breeding colonies deata and breeding colonies locations
	cal<-reactiveValues(cal=NULL)     #calendrier EPOQ
	dfperil<-reactiveValues(dfperil=NULL,dfperilp=NULL)#espèces en péril
	ch<-reactiveValues(ch=NULL)#habitats essentiels (critical habitats)
	buffer<-reactiveValues(buffer=NULL)#buffer EPOQ
	no_feuillet<-reactiveValues(nb="X")#nb de feuillets sur lequel le calendrier est basé
	plot_data<-reactiveValues(x=NULL,xx=NULL,tab=NULL,temp=NULL)
	inle<-reactiveValues(inle=NULL) #valeur précédente de input$legend
	opa<-reactiveValues(opa=NULL) #dit lorsque l'opacity change pour ne pas activer le observeEvent plus bas
  
	
	output$map <- renderLeaflet({
	  withProgress({
	  leaflet() %>%
			addProviderTiles("Stamen.TonerLite",options = providerTileOptions(noWrap = TRUE)) %>%
	    addTiles(group="Base") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
	    addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>% 
	    addProviderTiles("Esri.WorldTopoMap", group = "Esri World Topo Map") %>%
	    addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
	    #addPolygons(data=buff,col="brown") %>%
			setView(lng=-65,lat=49,zoom=6) %>% 
	    addLayersControl(
        baseGroups=c("Base","Toner Lite","CartoDB.Positron","Esri World Topo Map","Esri World Imagery"),
        options=layersControlOptions(collapsed=TRUE),
	      position="bottomright"
      )
	  },message=g("waitm1"))
	})
	
 
		
	output$map_noga <- renderLeaflet({
	  withProgress({
	  pal<-colorNumeric(palette=colopal,domain=noga_shp$p)
	  leaflet() %>%
			addProviderTiles("Stamen.TonerLite",options = providerTileOptions(noWrap = TRUE)) %>%
	    addTiles(group="Base") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
	    addProviderTiles("Esri.WorldTopoMap", group = "Esri World Topo Map") %>%
	    addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
	    addPolygons(data=noga_shp,stroke=FALSE,fillColor=pal(noga_shp$p),popup=paste0(round(noga_shp$p*100,1),"%"),weight=0,fillOpacity=0.5,group="noga") %>% 
	    addLayersControl(
        baseGroups=c("Base","Toner Lite","Esri World Topo Map","Esri World Imagery"),
        options=layersControlOptions(collapsed=TRUE),
	      position="bottomleft"
      ) %>% 
	    addLegend("bottomright",pal=pal,values = noga_shp$p,title=g("tdper"),labFormat=labelFormat(suffix="%",transform=function(x){100*x}),opacity=0.5,layerId="noga")
	  },message=g("waitm1"))
	})
	
	
	output$map_razo <- renderLeaflet({
	  withProgress({
	  pal<-colorNumeric(palette=colopal,domain=razo_shp$p)
	  leaflet() %>%
			addProviderTiles("Stamen.TonerLite",options = providerTileOptions(noWrap = TRUE)) %>%
	    addTiles(group="Base") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
	    addProviderTiles("Esri.WorldTopoMap", group = "Esri World Topo Map") %>%
	    addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
	    addPolygons(data=razo_shp,stroke=FALSE,fillColor=pal(razo_shp$p),popup=paste0(round(razo_shp$p*100,1),"%"),weight=0,fillOpacity=0.5,group="razo") %>% 
	    addLayersControl(
        baseGroups=c("Base","Toner Lite","Esri World Topo Map","Esri World Imagery"),
        options=layersControlOptions(collapsed=TRUE),
	      position="bottomleft"
      ) %>% 
	    addLegend("bottomright",pal=pal,values = razo_shp$p,title=g("tdper"),labFormat=labelFormat(suffix="%",transform=function(x){100*x}),opacity=0.5,layerId="razo")
	  },message=g("waitm1"))
	})
	
	
	output$map_rtlo <- renderLeaflet({
	  withProgress({
	  pal<-colorNumeric(palette=colopal,domain=rtlo_shp$p)
	  leaflet() %>%
			addProviderTiles("Stamen.TonerLite",options = providerTileOptions(noWrap = TRUE)) %>%
	    addTiles(group="Base") %>%
      addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
	    addProviderTiles("Esri.WorldTopoMap", group = "Esri World Topo Map") %>%
	    addProviderTiles("Esri.WorldImagery", group = "Esri World Imagery") %>%
	    addPolygons(data=rtlo_shp,stroke=FALSE,fillColor=pal(rtlo_shp$p),popup=paste0(round(rtlo_shp$p*100,1),"%"),weight=0,fillOpacity=0.5,group="rtlo") %>% 
	    addLayersControl(
        baseGroups=c("Base","Toner Lite","Esri World Topo Map","Esri World Imagery"),
        options=layersControlOptions(collapsed=TRUE),
	      position="bottomleft"
      ) %>% 
	    addLegend("bottomright",pal=pal,values = rtlo_shp$p,title=g("tdper"),labFormat=labelFormat(suffix="%",transform=function(x){100*x}),opacity=0.5,layerId="rtlo")
	  },message=g("waitm1"))
	})
  
	
	plot_barplot1<-function(){
	  if(!is.null(df$df)){
	    set.seed(1)
	    x<-df$df@data
	    x<-x[!is.na(x$Abundance),]
      x<-x[x$Abundance>0,]
      x$logAbundance<-log2(x$Abundance)
      if(input$radio_group==1){
        x$sp<-factor(x$sp,levels=unique(x$sp))
      }else{  
        x$sp<-factor(x$group,levels=unique(x$group))
      }
      x<-x[rev(order(x$Abundance)),]
      x$id<-1:nrow(x)
      y<-input$value_obs
      type<-c("first","threshold")[as.numeric(input$graph_type)]
      sel<-function(x,type=type){
        switch(type,
          first = x[1:min(nrow(x),y),],
          threshold = x[x$Abundance>=y,])
      }
      xx<-sel(x,type=type)
      xx<-droplevels(xx)
      xx$col<-rep(brewer.pal(12,"Paired"),50)[as.numeric(xx$sp)]
      xx$pch<-rep(15:18,each=12)[as.numeric(xx$sp)]
      tab<-rev(sort(table(xx$sp)))
      x$xval<-match(x$sp,names(tab))
      temp<-droplevels(x[x$sp%in%xx$sp,])
      plot_data$x<-x
      plot_data$xx<-xx
      plot_data$tab<-tab
      plot_data$temp<-temp
      par(new=FALSE)
      par(mar=c(11,6,3,5),mfrow=c(1,1))
      b<-barplot(tab,col=xx$col[match(names(tab),xx$sp)],border="white",space=c(0.015,0),ylim=c(0,1.1*max(tab)),names.arg=gsub(" ","\n",names(tab )),yaxt="n",xaxt="n",ylab="")
      axis(2,las=2)
      axis(1,at=b[,1],labels=names(tab),las=2,lwd=0,cex.axis=1.1,padj=0.25,hadj=1.025)
      points(b[,1],rep(0,length(b))-(0.03*max(tab)),col=xx$col[match(names(tab),xx$sp)],pch=xx$pch[match(names(tab),xx$sp)],cex=1.5,xpd=TRUE)
      mtext(paste(g("p1axe11"),min(xx$Abundance),g("p1axe12")),side=2,line=4,cex=1.25)
      par(new=TRUE)
      barplot(rep(max(x$logAbundance),length(tab)),col=rgb(0,0,0,0),border=rgb(0,0,0,0),space=c(0.015,0),names.arg="",xlab="",ylab="",yaxt="n",xaxt="n",ylim=c(0,1.1*max(x$logAbundance)))
      
      if(input$show_obs | input$show_perc){
        div<-pretty(x$logAbundance,10)
        axis(4,las=2,at=div,labels=round(2^(div),4),col=gray(0.6),col.axis=gray(0.6))
        mtext(g("p1axe21"),side=4,line=4,cex=1.25)
        for(i in seq_along(div)){abline(div[i],0,lty=3,col=gray(0.6))}
        abline(min(xx$logAbundance),0,lty=1,col=gray(0.6))
      }
      
      if(input$show_obs){
        points(jitter(b[,1][temp$xval],1),temp$logAbundance,col=ifelse(temp$id%in%xx$id,"red",gray(0.6,0.3)),cex=1,xpd=TRUE)
        legend("topright",pch=c(1,1,NA),lwd=c(NA,NA,1),col=c("red",gray(0.6,0.3),gray(0.6)),legend=c(g("p1leg1"),g("p1leg2"),paste(min(xx$Abundance),g("p1leg3"))),ncol=3,bty="n",inset=c(0,-0.05),xpd=TRUE)  
      }
      if(input$show_perc){
        temp2<-ddply(temp,.(sp),function(j){log2(quantile(j$Abundance,c(0.9,0.75,0.5)))})
        temp2<-temp2[order(match(temp2$sp,names(tab))),]
        #points(b[,1],temp2[,2],col="blue",cex=1.25,xpd=TRUE,pch=16)
        #points(b[,1],temp2[,3],col="blue",cex=1.25,xpd=TRUE,pch=16)
        #points(b[,1],temp2[,4],col="blue",cex=1.25,xpd=TRUE,pch=16)
        text(b[,1],temp2[,2],names(temp2)[2],font=2,xpd=TRUE,pch=16)
        text(b[,1],temp2[,3],names(temp2)[3],font=2,xpd=TRUE,pch=16)
        text(b[,1],temp2[,4],names(temp2)[4],font=2,xpd=TRUE,pch=16)
      }
	  }
	}
	
	plot_barplot2<-function(max=NULL,mar=c(5,6,3,5)){
	  if(!is.null(df$df)){
	    #y<-input$value_obs
      x<-plot_data$x
	    xx<-plot_data$xx
	    tab<-plot_data$tab
	    temp<-plot_data$temp
	    if(!is.null(max)){
	      xx<-xx[1:min(max,nrow(xx)),]
	    }
	    
	    par(mar=mar,mfrow=c(1,1))
      plot(1:nrow(xx),xx$Abundance,cex=1.25,yaxt="n",xaxt="n",col=xx$col,pch=xx$pch,yaxt="n",ylab="",xlab="",ylim=c(0,1.1*max(xx$Abundance)),bty="n")
      div<-pretty(xx$Abundance,10)
      axis(2,at=div,labels=pretty(xx$Abundance,10),las=2,col.axis=gray(0.6),pos=c(0,0),tick=FALSE)
      #axis(1,at=pretty(1:nrow(xx),10),labels=pretty(1:nrow(xx),10),col.axis="white",col.ticks=gray(0.6),pos=c(0,0))
      for(i in seq_along(div)){abline(div[i],0,lty=3,col=gray(0.6))}
      abline(min(xx$Abundance),0,lty=1,col=gray(0.6))
      points(1:nrow(xx),xx$Abundance,cex=1.5,col=xx$col,pch=xx$pch)
      
      text(1:nrow(xx),xx$Abundance+(max(xx$Abundance)*0.03),labels=paste(xx$Anne,xx$Base,sep=" "),srt=90,adj=c(0,0.25),cex=0.75,xpd=TRUE)
      mtext(g("p2axe1"),side=1,line=3,cex=1.25)
      mtext(g("p2axe2"),side=2,line=5,cex=1.25)
      xxl<-unique(xx[,c("pch","col","sp")])
      legend("topright",pch=xxl$pch,col=xxl$col,legend=xxl$sp,ncol=ifelse(nrow(xxl)>15,2,1),box.lwd=0,inset=c(0,0),xpd=TRUE,pt.cex=1.5,bg="white",box.col="white")
	  }
	}
	
	plot_calendar<-function(nbsp=NULL,box=FALSE){
	  cal2<-cal$cal$cal
	  withProgress(min=1,max=nrow(cal2),{
		setProgress(message=g("waitm5"))
	  n<-nrow(cal2)
	  if(input$radio1==2){
		  cal2[,-1]<-t(apply(cal2[,-1],1,function(i){i/max(i,na.rm=TRUE)}))
	  }
	  if(is.null(df$df)){
	    temp<-NULL
	  }else{
	    temp<-df$df@data
	  }
	  if(input$radio2==1){
		  cal2<-cal2[order(cal2[,1]),]
		}else{
		  if(input$radio2==2){
		    cal2<-cal2[order(d$Taxo[match(cal2[,1],d$sp)]),]
		  }else{
		    if(is.null(temp)){
		      temp2<-ddply(cal$cal$data,.(sp),function(x){x[which.max(x$Abundance),]}) #si aucune donnée, trie selon les données dans le buffer
		    }else{  
		      temp2<-ddply(temp,.(sp),function(x){x[which.max(x$Abundance),]}) 
		    }
		    cal2<-cal2[order(match(cal2[,1],temp2$sp[rev(order(temp2$Abundance))])),]
		  }
		}
	  n<-199
	  if(!is.null(nbsp)){
	    cal2<-cal2[1:min(as.numeric(nbsp),nrow(cal2)),]
	    n<-nbsp
	  }
	  inc<-if(is.null(temp)){rep(FALSE,length(cal2[,1]))}else{cal2[,1]%in%temp$sp}
	  m<-matrix(1:(n+1),ncol=1)
		layout(m)
		par(mar=c(0,25,1.2,10),oma=c(3,4,6,2))
		for(i in 1:nrow(cal2)){
		  bar<-unlist(cal2[i,-1])
		  if(i==1){
		    b<-barplot(bar,space=0,col="white",border=NA,names.arg="",axes=FALSE,ylim=c(0,1.05))
		    val<-((b[2,1]-b[1,1])/2)
			  xs<-b[,1]+val
		    text(b[,1],0.5,cal$cal$nbfeuillet[match(names(cal2)[-1],names(cal$cal$nbfeuillet))],xpd=TRUE,cex=1.5,col="black",adj=c(0.5,0.5))
		    text(-1.5*b[1,1],0.5,g("caxe1"),adj=c(1,0.5),cex=1.5,xpd=TRUE,col="black",font=2)
		    text(b[nrow(b),1]+(2*val),0.5,g("caxe2"),adj=c(0.0,0.5),cex=1.5,xpd=TRUE,col="black",font=2)
		    legend(x=coopar()$o[1],y=coopar()$o[4],text.col=c("black","grey75"),legend=c(g("cleg1"),g("cleg2")),xpd=NA,border=NA,cex=1.5,bty="n")
		    l<-legend(x=coopar()$o[2],y=coopar()$o[4],fill=c("darkgreen","springgreen3","grey90"),legend=c(g("cleg3"),g("cleg4"),g("cleg5")),xpd=NA,border=NA,cex=1.5,bty="n",plot=FALSE)
		    legend(x=coopar()$o[2]-(l$rect$w*1.1),y=coopar()$o[4],fill=c("darkgreen","springgreen3","grey90"),legend=c(g("cleg3"),g("cleg4"),g("cleg5")),xpd=NA,border=NA,cex=1.5,bty="n")
		  }
		  b<-barplot(bar,space=0,col="springgreen3",border=NA,names.arg="",axes=FALSE,ylim=c(0,1.05))
		  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="white",border="white")
			colbar<-if(any(input$month=="All months")){
			  "darkgreen"
			}else{
        ifelse(names(cal2)[-1]%in%input$month,"darkgreen","springgreen3")
			}
		  b<-barplot(bar,space=0,col=colbar,border=NA,names.arg="",axes=FALSE,ylim=c(0,1.05),add=TRUE)
			if(any(is.na(bar))){ # si il n'y a pas de données pour ce mois trace une barre grise par dessus tout
			  b<-barplot(ifelse(is.na(bar),1.00,NA),space=0,col="grey90",border=NA,names.arg="",axes=FALSE,ylim=c(0,1.05),add=TRUE)
			}
		  if((i%%5)==1){
		    text(b[,1],1.36,nmonth,xpd=TRUE,cex=1.3,col="grey75",adj=c(0.5,0.5))
		  }
			text(-1.5*b[1,1],0.5,cal2[i,1],adj=c(1,0.5),cex=2,xpd=TRUE,col=ifelse(inc[i],"black","grey75"))
			#rect(par("usr")[1],0,b[1,1]-val,1.05,col=alpha("lightsteelblue1",0.3),border="lightsteelblue1")
			lines(c(-1000,b[nrow(b),1]+(val)),c(0,0),col="grey90",xpd=TRUE,lwd=1)
			#rect(b[nrow(b),1]+val,0,par("usr")[2],1.05,col=alpha("lightsteelblue1",0.3),border=alpha("lightsteelblue1",0.3))
			text(b[nrow(b),1]+(3*val),0.6,cal$cal$mentions[match(cal2[i,1],names(cal$cal$mentions))],col="black",xpd=TRUE,cex=1.5,adj=c(0,0.5))
			for(i in 1:(length(xs)-1)){
			  lines(rep(xs[i],2),c(0,1.00),lwd=0.5,col="grey90")
			}
			setProgress(value=i)
		}
		if(box){
		  box("outer",col="grey90",lwd=3)
		  #box("inner",col="grey90",lwd=1)
		  #box("figure",col="grey90",lwd=1)
		  #box("plot",col="grey90",lwd=1)
		}
	  })
	}

	output$barplot1<-renderPlot({
    plot_barplot1()
	})
 
	     
  output$barplot2<-renderPlot({
    plot_barplot2()
  }) 
  
  
  output$graph3<-renderPlot({
	  if(!is.null(df)){
	    x<-cal$cal$data
      x<-x[x$sp==input$select_sp,]
      par(bg="lightsteelblue1")
      plot(as.numeric(x$Month),log2(x$Abundance),xaxt="n",yaxt="n",type="n",xlab="",ylab="",bty="n",xlim=c(0.5,12.5))
      div<-pretty(log2(x$Abundance),10)
      lab<-round(2^(div),10)
      keep<-ifelse((lab-floor(lab))>0.0000000001,FALSE,TRUE)
      lab<-lab[keep]
      div<-div[keep]
      axis(2,las=2,at=div,labels=lab,col=gray(0.6),pos=c(0.5,0),tick=FALSE)
      axis(1,at=1:12,label=nmonth,tick=FALSE)
      mtext(g("p3axe"),side=2,line=2,cex=1.25)
      rect(xleft=0.5,ybottom=par("usr")[3],xright=12.5,ytop=par("usr")[4],col="white",border=NA)
      for(i in seq_along(1:12)){lines(rep(((1:12)-0.5)[i],2),c(-1000,par("usr")[4]),lwd=2,col="lightsteelblue1",xpd=TRUE)}
      #for(i in seq_along(1:12)){if((i%%2)==0){rect(xleft=i-0.5,ybottom=0,xright=i+0.5,ytop=par("usr")[4],col="lightsteelblue1",border=NA)}}
      for(i in seq_along(div)){lines(c(0.5,12.5),rep(div[i],2),lty=3,col=gray(0.6,0.3))}
      points(jitter(as.numeric(x$Month),1.5),log2(x$Abundance),col=gray(0.4,0.3))
      legend("topright",col=gray(0.4,0.3),pch=1,legend=g("p3leg"),xpd=NA,border=NA,inset=c(0.1,-0.1),bty="n")
	  }
  }) 
	
  output$plot_legend<-renderPlot({
    par(mar=c(0.5,0,0.8,0),oma=c(0,0,0,0),bg="white")
    colos<-c("tomato","darkgreen","black","blue","black","black","orange","red","brown","black","black","black")
    colob<-c("tomato","darkgreen","blue","blue","orange","orange","orange","red","brown","red","black","black")
    col<-alpha(colos,0.5)
    bg<-alpha(colob,0.3)
    stroke<-c(FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)
    pch<-c(15,15,21,22,21,22,22,22,22,21,16,22)
    cex<-c(2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,2.5,3,2,2.5)
    lwd<-c(0,0,1.5,1.5,1.5,1.5,1.5,1.5,1.5,1.5,0,1.5)
    plot(rep(1,12),12:1,pch=pch,col=ifelse(stroke,col,bg),lwd=lwd,bg=bg,cex=cex,axes=FALSE,bty="n")
  }) 
	
  #output$nb_calendar <- renderText({
  #  paste("This calendar is based on        ",no_feuillet$nb,"EPOQ files")
	#}) 
	
	output$table1 <- renderDataTable({
		if(!is.null(df$df)){
		  input$resetSelectionBO
		  temp<-df$df@data
		  temp<-temp[rev(order(temp$Abundance)),]
		  datatable(temp,filter=list(position='top',clear=TRUE,plain=TRUE),options=optionsDT,rownames=TRUE)
		}
	})
	
	output$table2 <- renderDataTable({
		if(!is.null(hdf$hdf)){
		  datatable(hdf$hdf@data,filter=list(position='top',clear=TRUE,plain=TRUE),options=optionsDT,rownames=TRUE)
		}
	})
	
	output$table3 <- renderDataTable({
		if(!is.null(dfperil$dfperil)){
		  input$resetSelectionSAR
		  datatable(dfperil$dfperil@data,filter=list(position='top',clear=TRUE,plain=TRUE),options=optionsDT,rownames=TRUE)
		}
	})
	
	output$table4 <- renderDataTable({
		if(!is.null(ap$ap)){
		  datatable(ap$ap@data,filter=list(position='top',clear=TRUE,plain=TRUE),options=optionsDT,rownames=TRUE)
		}
	})
	
	output$table5 <- renderDataTable({ #à modifier pour que les bonnes données soient intégrées
		if(!is.null(bc$bcobs)){
		  temp<-bc$bcobs
		  temp<-temp[rev(order(temp$Chiffre)),]
		  datatable(temp,filter=list(position='top',clear=TRUE,plain=TRUE),options=optionsDT,rownames=TRUE)
		}
	})
	
	output$table6 <- renderDataTable({
		if(!is.null(ch$ch)){
		  datatable(ch$ch@data,filter=list(position='top',clear=TRUE,plain=TRUE),options=optionsDT,rownames=TRUE)
		}
	})
	
	output$table7 <- renderDataTable({
		if(!is.null(dfperil$dfperilp)){
		  datatable(dfperil$dfperilp@data,filter=list(position='top',clear=TRUE,plain=TRUE),options=optionsDT,rownames=TRUE)
		}
	})
	
	output$calendar <- renderPlot({
	  plot_calendar()
	},height=7000)
	
	
	observe({
		if(input$select_mode){
	    if(length(ll$x)>1){
			  leafletProxy("map") %>% 
				  removeMarker(layerId="single_marker") %>% 
				  addPolylines(lng=c(ll$x),lat=c(ll$y),stroke=TRUE,weight=3,color="tomato",group="region_line") #%>% 
		  }
		  if(length(ll$x)==1){
			  leafletProxy("map") %>% 
				  addMarkers(lng=ll$x,lat=ll$y,layerId="single_marker") %>% 
				  addPolylines(lng=c(ll$x),lat=c(ll$y),stroke=TRUE,weight=3,color="tomato",group="region_line")
		  }
		}
	})
	
	#observe({
	#  updateTextInput(session,"lat",value=tail(ll$y,1))
	#  updateTextInput(session,"lon",value=tail(ll$x,1))
	#})
	
	#output$text_modal<-renderText({
	#  paste(c("LON","LAT"),round(as.numeric(c(input$lon,input$lat)),5))
	#})
	
	observeEvent(input$map_click, {
	  if(input$select_mode){
	    tempx <- c(ll$x,input$map_click$lng)
		  tempy <- c(ll$y,input$map_click$lat)
		  ll$x <- tempx
		  ll$y <- tempy
	  }
	})

	observeEvent(input$map_shape_click, {
		if(input$select_mode){
	    tempx <- c(ll$x,input$map_shape_click$lng)
		  tempy <- c(ll$y,input$map_shape_click$lat)
		  ll$x <- tempx
		  ll$y <- tempy
		}
	  #browser()
	})
	
	observeEvent(input$map_marker_click, {
		if(input$select_mode){
	    tempx <- c(ll$x,input$map_marker_click$lng)
		  tempy <- c(ll$y,input$map_marker_click$lat)
		  ll$x <- tempx
		  ll$y <- tempy
		}
	})
	
	observeEvent(input$select_region,{
	    if(is.null(region$region) && length(ll$x)<3){
	      leafletProxy("map") %>% 
				  removeMarker(layerId="single_marker")
		    return()
	    }else{
		    if(length(ll$x)>=3){ 
		      clear_map()
		      m<-p2pol(cbind(ll$x,ll$y))
			    region$region<-m 
		    }else{
	        clear_map()
		    }
	    }
	    withProgress({
	    ## plot region and buffer
	    leafletProxy("map",deferUntilFlush=FALSE) %>% 
			  clearGroup("region_line") %>% 
			  removeMarker(layerId="single_marker") %>% 
				addPolygons(data=region$region,stroke=FALSE,color="tomato",group="Selected area")
	    ll$x<-NULL
		  ll$y<-NULL
			buffer$buffer<-gBufferll(region$region,width=as.numeric(max(0,input$buff_size*1000)))
			### first subset for epoq calendar
			o<-over(d,buffer$buffer)
			x<-d[!is.na(o),]
			y<-x[x$Base=="EPOQ",]
			o<-over(x,region$region)
			x<-x[!is.na(o),]
			if(nrow(y)==0){  # si aucune donnée EPOQ est disponible, on prend toutes les données EPOQ, à changer avec les latitudes
			  cal$cal<-epoq_calendar(d@data[d$Base=="EPOQ",])
			  no_feuillet$nb<-"all"
			}else{
			  cal$cal<-epoq_calendar(y@data) 
			  no_feuillet$nb<-length(unique(y$Feuillet))
			}
			updateSelectInput(session,"select_sp",choices=unique(cal$cal$cal[,1]))
			### subset data according to boxes
			if(all(input$month!="All months")){
        x<-x[which(x$Month%in%input$month),]	
			}
			if(all(input$database!=g("bdall")) && nrow(x)!=0){
        x<-x[which(x$Base%in%input$database),]	
			}
      if(nrow(x)!=0){
			  x<-x[which(x$Anne>=input$min_year & x$Abundance>=input$min_nb),]			  
      }
      ###
			df$df<-if(nrow(x)==0){NULL}else{x}
			
			### colonies
			o<-over(biomq_shp,region$region)
			if(all(is.na(o))){
			  bc$bc<-NULL #pour corriger le bug dans sp où subset vide de spatial créer une erreur
			  bc$bcobs<-NULL
			}else{
			  temp<-biomq_shp[!is.na(o),]
				bc$bc<-temp
				keep<-biomq$Id_Colonie%in%temp$ID_COLONIE
				bc$bcobs<-biomq[keep,]
			}

			### federal lands
			o<-over(h,region$region)
			if(all(is.na(o))){
			  hdf$hdf<-NULL #pour corriger le bug dans sp où subset vide de spatial créer une erreur
			}else{
			  hh<-h[!is.na(o),]
				hdf$hdf<-hh
			}
			
			### species at risk records
			o<-over(peril,region$region)
			if(all(is.na(o))){
			  dfperil$dfperil<-NULL #pour corriger le bug dans sp où subset vide de spatial créer une erreur
			}else{
			  pp<-peril[!is.na(o),]
				dfperil$dfperil<-pp
			}
			if(!is.null(dfperil$dfperil)){
			  choix<-c("",unique(dfperil$dfperil$sp))
			  names(choix)[1]<-g("sarph")
			  names(choix)[2:length(choix)]<-""
			  updateSelectInput(session,"select_sar",choices=choix)
			}else{
			  updateSelectInput(session,"select_sar",choices="")
			}
			
			### species at risk polygons
			o<-over(perilp,region$region)
			if(all(is.na(o))){
			  dfperil$dfperilp<-NULL #pour corriger le bug dans sp où subset vide de spatial créer une erreur
			}else{
			  pp<-perilp[!is.na(o),]
				dfperil$dfperilp<-pp
			}

			o<-over(wl,region$region)
			if(all(is.na(o))){
			  ap$ap<-NULL #pour corriger le bug dans sp où subset vide de spatial créer une erreur
			}else{
		    pp<-wl[!is.na(o),]
				ap$ap<-pp
			}
			o<-over(he,region$region)
			if(all(is.na(o))){
			  ch$ch<-NULL #pour corriger le bug dans sp où subset vide de spatial créer une erreur
			}else{
		    pp<-he[!is.na(o),]
				ch$ch<-pp
			}
			#cat("df",class(df$df),nrow(df$df),"\n")
			#cat("hdf",class(hdf$hdf),nrow(hdf$hdf),"\n")
			#cat("dfperil",class(dfperil$dfperil),nrow(dfperil$dfperil),"\n")
			#cat("ap",class(ap$ap),nrow(ap$ap),"\n")
			#cat("bc",class(bc$bc),nrow(bc$bc),"\n")
			updateCheckboxInput(session,"select_mode",value=FALSE)
			leafletProxy("map",deferUntilFlush=FALSE) %>% 
				addPolygons(data=buffer$buffer,stroke=FALSE,color="darkgreen",group="EPOQ buffer")
		  },message=g("waitm6"))
	})
	
	observeEvent(input$show_data,{
		if(is.null(region$region)){
			return()
		}else{
			withProgress({
			leafletProxy("map",deferUntilFlush=FALSE) %>% 
		    clearGroup("Selected area") %>% 
		    clearGroup("EPOQ buffer") %>% 
		    addPolygons(data=buffer$buffer,stroke=FALSE,color="darkgreen",group="EPOQ buffer") %>% 
				addPolygons(data=region$region,stroke=FALSE,color="tomato",group="Selected area")# %>% 
			  #fitBounds(bbox(region$region)[1,1], bbox(region$region)[2,1], bbox(region$region)[1,2], bbox(region$region)[2,2])
			if(!is.null(df$df)){  
		    popup<-as.character(apply(df$df@data,1,function(i){paste(paste(names(df$df@data),i,sep=": "),collapse="<br/>")}))
		    #limit<-df$df$Base!="EPOQ",TRUE,ifelse(,,) #limit the numbers of locations displayed
		    leafletProxy("map",deferUntilFlush=FALSE) %>% 
		      addCircleMarkers(data=df$df,radius=6,fillColor="blue",popup=popup,group="Bird observations",clusterOptions=if(!input$use_clusters){NULL}else{markerClusterOptions()},stroke=TRUE,color="black",weight=1)
		  }
			if(!is.null(hdf$hdf)){
			  popup<-as.character(apply(hdf$hdf@data,1,function(i){paste(paste(names(hdf$hdf@data),i,sep=": "),collapse="<br/>")}))
			  leafletProxy("map",deferUntilFlush=FALSE) %>% 
					addPolygons(data=hdf$hdf,stroke=TRUE,col="red",popup=popup,weight=1,group="Federal lands")	
			}
			if(!is.null(dfperil$dfperil)){
				popup<-as.character(apply(dfperil$dfperil@data,1,function(i){paste(paste(names(dfperil$dfperil@data),i,sep=": "),collapse="<br/>")}))
			  leafletProxy("map",deferUntilFlush=FALSE) %>% 
					addCircleMarkers(data=dfperil$dfperil,radius=6,fillColor="orange",popup=popup,group="Birds at risk",stroke=TRUE,color="black",weight=1)	
			}
			
			if(!is.null(dfperil$dfperilp)){
			  popup<-as.character(apply(dfperil$dfperilp@data,1,function(i){paste(paste(names(dfperil$dfperilp@data),i,sep=": "),collapse="<br/>")}))
			  leafletProxy("map",deferUntilFlush=FALSE) %>% 
					addPolygons(data=dfperil$dfperilp,stroke=TRUE,fillColor="orange",color="black",popup=popup,weight=1,group="Species at risk",layerId=dfperil$dfperilp$EO_ID)	
			}  
			  
			if(!is.null(ch$ch)){
			  popup<-as.character(apply(ch$ch@data,1,function(i){paste(paste(names(ch$ch@data),i,sep=": "),collapse="<br/>")}))
			  leafletProxy("map",deferUntilFlush=FALSE) %>% 
					addPolygons(data=ch$ch,stroke=TRUE,fillColor="orange",color="red",popup=popup,weight=1,group="Critical habitats")	
			}
		  if(!is.null(ap$ap)){
				popup<-as.character(apply(ap$ap@data,1,function(i){paste(paste(names(ap$ap@data),i,sep=": "),collapse="<br/>")}))
			  leafletProxy("map",deferUntilFlush=FALSE) %>% 
					addPolygons(data=ap$ap,stroke=TRUE,col="brown",popup=popup,weight=1,group="Wetlands")
			  if(!(c("Wetlands")%in%input$legend)){
			    leafletProxy("map",deferUntilFlush=FALSE) %>% 
					  hideGroup("Wetlands") 
			  }
		  }
		  if(!is.null(bc$bc)){
		    leafletProxy("map",deferUntilFlush=FALSE) %>% 
				  addPolygons(data=bc$bc,stroke=TRUE,col="blue",popup=paste(bc$bc$Nom_Colonie," / colonie",bc$bc$ID_COLONIE),weight=1,group="Breeding colonies")	
		  }
			},message=g("waitm7"))
		}
	})
	
	observeEvent(input$clear_selection,{
	    clear_selection()
      clear_map()
      updateCheckboxInput(session,"select_mode",value=TRUE)
      leafletProxy("map_noga") %>% 
	      clearGroup("region")
	    leafletProxy("map_razo") %>% 
	      clearGroup("region")
	    leafletProxy("map_rtlo") %>% 
	      clearGroup("region")
      
	})
	
	observeEvent(input$clear_map,{
      clear_map()
	})
	
	
	observeEvent(input$legend,{  #problème ici avec la manipulation de la légende et des couches Municipalities et EPOQ locations
	    inle_p<-inle$inle #pour ne pas effacer et retracer les deux couches si elles sont déjà là
	    inle$inle<-input$legend
	    leafletProxy("map",deferUntilFlush=FALSE) %>% 
	      hideGroup(group=setdiff(unlist(leg_list,use.names=FALSE),input$legend)) %>% 
	      showGroup(group=input$legend)
	    if(any(input$legend=="Municipalities") && !any("Municipalities"==inle_p)){
	      withProgress({
	      leafletProxy("map",deferUntilFlush=FALSE) %>% 
	        clearGroup("Municipalities") %>% 
	        addPolygons(data=municip.shp,stroke=TRUE,popup=municip.shp$MUS_NM_MUN,col="black",weight=1,group="Municipalities",fillOpacity=0.1,opacity=0.35)
	      },message=g("waitm2"))
	    }
	    if(any(input$legend=="EPOQ locations") && !any("EPOQ locations"==inle_p)){
	      withProgress({
	      leafletProxy("map",deferUntilFlush=FALSE) %>% 
	        clearGroup("EPOQ locations") %>% 
	        addCircleMarkers(data=epoqxy,radius=2,popup=epoqxy$Base,group="EPOQ locations",col="black",weight=5,opacity=0.4,fillOpacity=0.2)
	      },message=g("waitm3"))
	    }
	},ignoreNULL=FALSE)
	
	
	#observeEvent(input$create_report,{
	#  browser()  
	#  saveWidget(widget = leafletmap$leafletmap, file = "W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Code_R/shiny/map2.html")
	#})
	
	observeEvent(input$table1_rows_selected,{
	  selec<-input$table1_rows_selected
	  if(is.null(selec)){
	    leafletProxy("map") %>% 
	      clearGroup("Selected data")
	  }else{
	    popup<-as.character(apply(df$df@data[selec,],1,function(i){paste(paste(names(df$df@data),i,sep=": "),collapse="<br/>")}))
	    leafletProxy("map") %>% 
	      clearGroup("Selected data") %>% 
  	    addCircleMarkers(data=df$df[selec,],radius=8,popup=popup,fillColor="red",group="Selected data",clusterOptions=if(!input$use_clusters){NULL}else{markerClusterOptions()},stroke=TRUE,color="black",weight=1)
	  }
	},ignoreNULL=FALSE)
	
	
	observeEvent(input$table3_rows_selected,{
	  selec<-input$table3_rows_selected
	  if(is.null(selec)){
	    leafletProxy("map") %>% 
	      clearGroup("Selected data")
	  }else{
	    popup<-as.character(apply(dfperil$dfperil@data[selec,],1,function(i){paste(paste(names(dfperil$dfperil@data),i,sep=": "),collapse="<br/>")}))
	    leafletProxy("map") %>% 
	      clearGroup("Selected data") %>% 
  	    addCircleMarkers(data=dfperil$dfperil[selec,],radius=8,popup=popup,fillColor="red",group="Selected data",stroke=TRUE,color="black",weight=1)
	  }
	},ignoreNULL=FALSE)
	
	
	
	observeEvent(input$show_region,{
	  if(!input$show_region){
	    leafletProxy("map_noga") %>% 
	      clearGroup("region")
	    leafletProxy("map_razo") %>% 
	      clearGroup("region")
	    leafletProxy("map_rtlo") %>% 
	      clearGroup("region")
	  }
	  if(input$show_region && !is.null(region$region)){
	    leafletProxy("map_noga") %>% 
	      addPolygons(data=region$region,stroke=FALSE,color="red",group="region",fillOpacity=0.15)
	    leafletProxy("map_razo") %>% 
	      addPolygons(data=region$region,stroke=FALSE,color="red",group="region",fillOpacity=0.15)
	    leafletProxy("map_rtlo") %>% 
	      addPolygons(data=region$region,stroke=FALSE,color="red",group="region",fillOpacity=0.15)
	  }
	})
	
	
	observeEvent(region$region,{
	  if(input$show_region && !is.null(region$region)){
	    leafletProxy("map_noga") %>% 
	      clearGroup("region") %>% 
	      addPolygons(data=region$region,stroke=FALSE,color="red",group="region",fillOpacity=0.15)
	    leafletProxy("map_razo") %>% 
	      clearGroup("region") %>% 
	      addPolygons(data=region$region,stroke=FALSE,color="red",group="region",fillOpacity=0.15)
	    leafletProxy("map_rtlo") %>% 
	      clearGroup("region") %>% 
	      addPolygons(data=region$region,stroke=FALSE,color="red",group="region",fillOpacity=0.15)
	  }
	})
	
	
  observeEvent(input$opacity,{
	  if(is.null(opa$opa)){
	    opa$opa<-input$opacity
	    return()
	  }
    withProgress({
      if(input$tabs=="noga"){
        pal<-colorNumeric(palette=colopal,domain=noga_shp$p)
        leafletProxy("map_noga",deferUntilFlush=FALSE) %>% 
          clearGroup("noga") %>% 
	        addPolygons(data=noga_shp,stroke=FALSE,fillColor=pal(noga_shp$p),popup=paste0(round(noga_shp$p*100,1),"%"),weight=0,fillOpacity=input$opacity,group="noga") %>% 
	        addLegend("bottomright",pal=pal,values = noga_shp$p,title="% of birds using the area",labFormat=labelFormat(suffix="%",transform=function(x){100*x}),opacity=input$opacity,layerId="noga")
      }
      
      if(input$tabs=="razo"){
        pal<-colorNumeric(palette=colopal,domain=razo_shp$p)
        leafletProxy("map_razo",deferUntilFlush=FALSE) %>% 
          clearGroup("razo") %>% 
	        addPolygons(data=razo_shp,stroke=FALSE,fillColor=pal(razo_shp$p),popup=paste0(round(razo_shp$p*100,1),"%"),weight=0,fillOpacity=input$opacity,group="razo") %>% 
	        addLegend("bottomright",pal=pal,values = razo_shp$p,title="% of birds using the area",labFormat=labelFormat(suffix="%",transform=function(x){100*x}),opacity=input$opacity,layerId="razo")
      }
      
      if(input$tabs=="rtlo"){
        pal<-colorNumeric(palette=colopal,domain=rtlo_shp$p)
        leafletProxy("map_rtlo",deferUntilFlush=FALSE) %>% 
          clearGroup("rtlo") %>% 
	        addPolygons(data=rtlo_shp,stroke=FALSE,fillColor=pal(rtlo_shp$p),popup=paste0(round(rtlo_shp$p*100,1),"%"),weight=0,fillOpacity=input$opacity,group="rtlo") %>% 
	        addLegend("bottomright",pal=pal,values = rtlo_shp$p,title="% of birds using the area",labFormat=labelFormat(suffix="%",transform=function(x){100*x}),opacity=input$opacity,layerId="rtlo")
      }
      
    },message=g("waitm1"))
    
	},ignoreNULL=TRUE)
	
	
	
	
	observeEvent(input$create_report,{
	  if(is.null(region$region)){
	    return()
  	}
	  if(input$chosen_dir==""){
	  withProgress({
	    path<-choose.dir()
	    if(is.na(path)){path<-tempdir()}
	    updateTextInput(session,"chosen_dir",value=path)
	  },message=g("waitm8"))
	  }else{
	    path<-input$chosen_dir
	  }#input$chosen_dir
	withProgress({
	  if(input$produce_png){
	    png(paste0(path,"/barplot1.png"),width=6,height=4,units="in",res=600,pointsize=6)  
	      plot_barplot1()
	    dev.off()
	    png(paste0(path,"/barplot2.png"),width=6,height=4,units="in",res=600,pointsize=6)  
	      plot_barplot2()
	    dev.off()
	    png(paste0(path,"/calendar.png"),width=6,height=input$nb_calendars/3,units="in",res=600,pointsize=6)  
	      plot_calendar(nbsp=input$nb_calendars)
	    dev.off()
	  }
	  
	  tab_nb<-1
	  fig_nb<-1
    
	  res<-c("---") %>% 
      c("output: html_document") %>% 
	    c("toc: true") %>% 
	    c("toc_depth: 2") %>% 
      c("theme: united") %>% 
	    c("always_allow_html: yes") %>% 
      c("---") %>% 
	    c("  ") %>%
	    c(paste0("![](",pathr,"/","envcanlogo2.jpg)")) %>% 
	    c("  ")# %>%
	    #c(paste0("#",toupper(g("titrerap")),"  "))
	    
	  ### add custom text before the template
	  #val<-input[["s0"]]
	  #if(!is.null(val) && val!=""){
	  #   res<-res %>% c("  ") %>% c(unlist(strsplit(input$s0,"\n"))) %>% c("  ")
	  #}
	  
    if(any(input$section_include=="Significant Wildlife Event Notification")){
      res<-res %>% 
        c("  ") %>%
        c(paste0("##",toupper(g("crtab1")))) %>% 
        c("  ")
      
      res<-res %>% 
        c("  ") %>%
	      c(paste(g("genrap1"),Sys.time(),"  ")) %>%
	      c(paste(g("genrap2"),input$reporter,"  ")) %>%
	      c("  ")
      
	    val<-input[[paste0("s",match("Significant Wildlife Event Notification",unlist(sec_list,use.names=FALSE)))]]
	    if(!is.null(val) && val!=""){
	      res<-res %>% c("  ") %>% c(val) %>% c("  ")
	    }
	    
	    for(i in seq_along(NWERCP_list)){
	      if(input[[paste0("a",i)]]==""){
	        next
	      }else{
	        res<-res %>% 
	          c("  ") %>%
	          c(paste0("**",NWERCP_list[[i]],"**")) %>%
	          c("  ") %>% 
	          c(input[[paste0("a",i)]]) %>% 
	          c("  ")
	      }
	    }
    }

	  res<-res %>% c(paste0("**",g("cclist"),"**")) %>%
	    c("  ") %>%
	    c(paste(sapply(cc_list,function(i){i[1]}),collapse=", ")) %>% 
	    c("  ") %>%
	    c(paste0("i;",paste(sapply(cc_list,function(i){i[2]}),collapse=";"))) %>% 
	    c("  ") %>%
	    #c(unlist(strsplit(input$a0,"\n"))) %>% ###custom text at the beginning
	    c("  ")
	  
	  
	  #determines figure dimension
	  lon2utmzone<-function(lon){(floor((lon + 180)/6)%%60)+1}
	  #browser()
	  b<-bbox(spTransform(region$region,CRS(paste0("+proj=utm +zone=",lon2utmzone(coordinates(region$region)[1,1]),", +datum=NAD83 +ellps=GRS80")))) #determines figure proportions
	  W<-(b[1,2]-b[1,1])*0.3
	  H<-(b[2,2]-b[2,1])*0.3
	  fw<-6.5
	  if(W>H){
	    fh<-6.5*(H/W)
	  }else{
	    fh<-5
	  }
	  
	  if(any(input$section_include=="Location")){
	    res<-res %>% c("\\newpage")
	    res<-res %>% c(paste0("##",toupper(g("si2")),"  "))
	    
	    ### query parameters and summary
	    bdsel<-if(any(input$database==g("bdall"))){sort(unique(d$Base))}else{input$database}
	    res<-res %>% c(paste0("  **",g("pa_re1"),"**","  "),"  ")
	    res<-res %>% c(paste0("  ",g("pa_re2"),": ",paste(bdsel,collapse=", ")),"  ")
	    res<-res %>% c(paste0("  ",g("pa_re3"),": ",input$month),"  ")
	    res<-res %>% c(paste0("  ",g("pa_re4"),": ",input$min_year),"  ")
	    res<-res %>% c(paste0("  ",g("pa_re5"),": ",if(is.null(df$df)){0}else{nrow(df$df)}),"  ")
	    res<-res %>% c(paste0("  ",g("pa_re6"),": ",if(is.null(bc$bc)){0}else{nrow(bc$bc)}),"  ")

	    val<-input[[paste0("s",match("Location",unlist(sec_list,use.names=FALSE)))]]
	    if(!is.null(val) && val!=""){
	      res<-res %>% c("  ") %>% c(val) %>% c("  ")
	    }
	    if(any(input$plot_include==1)){ 
	      res<-res %>% c(paste0("```{r fig.width=",fw,", fig.height=",fh,", echo=FALSE}")) %>% 
	        c("b<-bbox(region$region)") %>% 
	        c("W<-(b[1,2]-b[1,1])*0.3") %>% 
	        c("H<-(b[2,2]-b[2,1])*0.3") %>% 
	        c("map.osm2<-openmap(c(b[2,2]+H,b[1,1]-W),c(b[2,1]-H,b[1,2]+W),type=\"osm\")") %>% 
	        c("map.osm<-openmap(c(b[2,2],b[1,1]),c(b[2,1],b[1,2]),type=\"osm\")") %>% 
          c("#map.osm<-openproj(map.osm,proj4string(h))") %>% 
	        c("#map.osm2<-openproj(map.osm,proj4string(h))") %>% 
	        c("par(mar=c(0,0,0,0))") %>%
          c("plot(map.osm2)") %>% 
          c("plot(spTransform(region$region,osm()),add=T,col=alpha(\"tomato\",0.15),border=NA)") %>% 
	        c("```")
	     #}else{
	     #  res<-res %>% c("```{r fig.width=6.5, fig.height=5, echo=FALSE}") %>% 
	     #    c("leaflet() %>% addTiles() %>% ") %>% 
	     #    c("addPolygons(data=region$region,stroke=FALSE,color=\"tomato\") %>% ") %>% 
	      #c("addPolygons(data=buffer$buffer,stroke=FALSE,color=\"darkgreen\") %>% ") %>% 
       #    c("clearBounds()") %>%
	     #    c("```")
	     #}
	      res<-res %>% c("  ") %>%
	        c(paste0("**",g("fig"),fig_nb,"**. ",g("fig1"))) %>% 
	        c("  ")
	      fig_nb<-fig_nb+1
	    }
	  }
	  

	  if(any(input$section_include=="Migratory bird abundance")){
	    #res<-res %>% c("\\newpage")
	    res<-res %>% c(paste0("##",toupper(g("si3")),"   "))
	    val<-input[[paste0("s",match("Migratory bird abundance",unlist(sec_list,use.names=FALSE)))]]
	    if(!is.null(val) && val!=""){
	      res<-res %>% c("  ") %>% c(val) %>% c("  ")
	    }
	    if(!is.null(df$df)){
	      if(any(input$plot_include==2)){   
	        res<-res %>% c("```{r fig.width=6.5, fig.height=5, echo=FALSE, dev.args=list(pointsize=9)}") %>% 
	          c("plot_barplot1()") %>% 
	          c("```") %>% 
	          c("  ") %>%
	          c(paste0("**",g("fig"),fig_nb,"**. ",g("fig2"),min(plot_data$xx$Abundance),g("fig21"))) %>%
	          c("  ")
	        fig_nb<-fig_nb+1
	      }
	      if(any(input$plot_include==3)){   
	        res<-res %>% c("```{r fig.width=6.5, fig.height=4, echo=FALSE, dev.args=list(pointsize=9)}") %>% 
	          c("plot_barplot2(max=50,mar=c(5,6,3,1))") %>% 
	          c("```") %>% 
	          c("  ") %>%
	          c(paste0("**",g("fig"),fig_nb,"**. ",g("fig3"))) %>%
	          c("  ")
	        fig_nb<-fig_nb+1
	      }
	      res<-res %>% c(paste0("```{r fig.width=",fw,", fig.height=",fh,", echo=FALSE, dpi=300}")) %>% 
	        #c("b<-bbox(region$region)") %>% 
	        #c("w<-(b[1,2]-b[1,1])*0.15") %>% 
	        #c("h<-(b[2,2]-b[2,1])*0.15") %>% 
	        #c("map.osm<-openmap(c(b[2,2]+h,b[1,1]-w),c(b[2,1]-h,b[1,2]+w),type=\"osm\")") %>% 
          #c("#map.osm<-openproj(map.osm,proj4string(h))") %>% 
	        c("tab<-df$df@data") %>% 
	        c(paste0("tab<-tab[rev(order(tab$Abundance)),][1:min(",input$nb_records,",nrow(tab)),]")) %>% 
	        c("row.names(tab)<-1:nrow(tab)") %>%
	        c("tab$ID_Obs<-1:nrow(tab)") %>%
	        c("tab$ID_Site<-as.numeric(factor(paste(tab$Long,tab$Lat)))") %>% #Ne peut pas prendre le nom de lieu car cartaines n'en ont pas
	        c("par(mar=c(0,0,0,0))") %>% 
	        c("plot(map.osm)") %>% 
          c("reg<-spTransform(region$region,osm())") %>% 
	        c("plot(reg,add=TRUE,col=alpha(\"tomato\",0.15),border=NA)") %>% 
	        c("x<-spTransform(df$df[df$df$ID%in%tab$ID,],osm())") %>% 
	        c("x<-x[order(match(x$ID,tab$ID)),]") %>% 
	        c("x$ID_Obs<-tab$ID_Obs") %>% 
	        c("bb<-bbox(reg)") %>% 
	        c("jit<-max((bb[1,2]-bb[1,1]),(bb[2,2]-bb[2,1]))*0.04") %>% 
	        c("xc<-jitter(coordinates(x),amount=jit)") %>% 
	        #c("plot(x,add=TRUE,col=alpha(\"red\",1),cex=2,lwd=2)") %>% #donne la localisation originale
	        c("points(xc[,1],xc[,2],col=alpha(\"blue\",0.3),pch=16,cex=2)") %>% 
	        c("text(xc[,1],xc[,2],labels=x$ID_Obs,cex=0.7)") %>%
	        c("text(coordinates(x)[,1],coordinates(x)[,2],labels=tab$ID_Site,cex=1,col=\"red\")") %>% 
	        c("```") %>% 
	        c("  ") %>%
	        c(paste0("**",g("fig"),fig_nb,"**. ",g("fig4"),input$nbrecords,g("fig41"))) %>%
	        c("  ")
	      fig_nb<-fig_nb+1
	      res<-res %>% c("  ") %>%
	        c(paste0("**",g("tab"),tab_nb,"**. ",g("tab1"),input$nb_records,g("tab11"))) %>%
	        c("```{r,results=\"asis\", echo=FALSE}") %>% 
	        c("tab<-tab[,c(\"ID_Site\",\"ID_Obs\",\"sp\",\"Abundance\",\"Date\",\"Nom_lieu\",\"Base\")]") %>% 
	        c("panderOptions(\"table.caption.prefix\",\"Tableau 1. \")") %>% 
	        c("panderOptions(\"table.continues\",\"Suite...\")") %>% 
          c("pandoc.table(tab, style=\"multiline\",split.table=Inf)") %>% 
          c("```")
	      tab_nb<-tab_nb+1 
	    }
	  }
	  
	  
	  if(any(input$section_include=="Breeding colonies")){
	    #res<-res %>% c("\\newpage")
      res<-res %>% c(paste0("##",toupper(g("si4")),"  "))
      val<-input[[paste0("s",match("Breeding colonies",unlist(sec_list,use.names=FALSE)))]]
	    if(!is.null(val) && val!=""){
	      res<-res %>% c("  ") %>% c(val) %>% c("  ")
	    }
      if(!is.null(bc$bc)){

	      res<-res %>% c(paste0("```{r fig.width=",fw,", fig.height=",fh,", echo=FALSE}")) %>% 
	        c("par(mar=c(0,0,0,0))") %>%
          c("plot(map.osm)") %>% 
          c("plot(spTransform(region$region,osm()),add=TRUE,col=alpha(\"tomato\",0.15),border=NA)") %>% 
	        c("x<-spTransform(bc$bc,osm())") %>% 
	        c("plot(x,add=TRUE,col=alpha(\"green\",0.5),border=alpha(\"blue\",0.8))") %>% 
	        c("text(coordinates(x)[,1],coordinates(x)[,2],labels=x$ID_COLONIE,cex=0.7)") %>% 
	        c("```") %>% 
	        c("  ") %>%
	        c(paste0("**",g("fig"),fig_nb,"**. ",g("fig5"))) %>%
	        c("  ")
	      fig_nb<-fig_nb+1

        
			  res<-res %>% c("  ") %>%
	        c(paste0("**",g("tab"),tab_nb,"**. ",g("tab2"))) %>%
	        c("```{r,results=\"asis\", echo=FALSE}") %>% 
          c("tab<-bc$bcobs") %>% 
			    c("tab<-ddply(tab,.(Id_Colonie,Nom_Colonie),function(x){c(max(x$Annee,na.rm=TRUE),paste(unique(x$sp),collapse=\", \"))})") %>%
	        c("names(tab)[3:4]<-c(\"Annee\",\"sp\")") %>% 
			    #c("tab<-unique(tab[,c(\"Id_Colonie\",\"Nom_Colonie\",\"sp\",\"Active\")])") %>% 
			    c("names(tab)[which(names(tab)==\"Id_Colonie\")]<-\"ID_Colonie\"") %>% 
			    c("row.names(tab)<-1:nrow(tab)") %>% 
	        c("panderOptions(\"table.caption.prefix\",\"Tableau X.\")") %>% 
	        c("panderOptions(\"table.continues\",\"Suite...\")") %>% 
	        c("pandoc.table(tab, style=\"multiline\",split.table=Inf,split.cells=c(1,1,1,50))") %>%
          c("```")
			  tab_nb<-tab_nb+1
      }
	  }
    
    
    if(any(input$section_include=="Seasonal calendar")){
      #res<-res %>% c("\\newpage")
	    res<-res %>% c(paste0("##",toupper(g("si5")),"  "))
	    val<-input[[paste0("s",match("Seasonal presence",unlist(sec_list,use.names=FALSE)))]]
	    if(!is.null(val) && val!=""){
	      res<-res %>% c("  ") %>% c(val) %>% c("  ")
	    }
	    if(!is.null(cal$cal) && any(input$plot_include==4)){  
	      res<-res %>% c(paste0("```{r fig.width=6.5, fig.height=",min(max(input$nb_calendars/3,2),9.5),", echo=FALSE, dev.args=list(pointsize=7)}")) %>%
	        c(paste0("plot_calendar(nbsp=",input$nb_calendars,",box=TRUE)")) %>% 
	        c("```") %>% 
	        c("  ") %>%
	        c(paste0("**",g("fig"),fig_nb,"**. ",g("fig6"),input$nb_calendars,g("fig61"))) %>%
	        c("  ")
	      fig_nb<-fig_nb+1
	    }
    }

    if(any(input$section_include=="Bird species at risk")){
	    #res<-res %>% c("\\newpage")
      res<-res %>% c(paste0("##",toupper(g("si6")),"  "))
      al<-input[[paste0("s",match("Species of birds at risk",unlist(sec_list,use.names=FALSE)))]]
	    if(!is.null(val) && val!=""){
	      res<-res %>% c("  ") %>% c(val) %>% c("  ")
	    }
	    if(!is.null(dfperil$dfperil)){   
	      lsp<-input$select_sar
	      if(all(lsp=="")){
	        lsp<-unique(dfperil$dfperil$sp)
	      }
	      res<-res %>% c(paste0("```{r fig.width=",fw,", fig.height=",fh,", echo=FALSE}")) %>% 
	        c("tab<-dfperil$dfperil@data") %>% 
	        c("tab<-tab[tab$sp%in%lsp,]") %>% 
	        c("o<-order(tab$Nom_lieu)") %>% 
	        c("tab<-tab[o,]") %>%
	        c("row.names(tab)<-1:nrow(tab)") %>% 
	        c("tab$ID_Obs<-1:nrow(tab)") %>%
	        c("tab$ID_Site<-as.numeric(factor(tab$Nom_lieu))") %>%
	        c("par(mar=c(0,0,0,0))") %>%
          c("plot(map.osm)") %>% 
          c("reg<-spTransform(region$region,osm())") %>% 
	        c("bb<-bbox(reg)") %>% 
	        c("jit<-max((bb[1,2]-bb[1,1]),(bb[2,2]-bb[2,1]))*0.07") %>% 
	        c("x<-spTransform(dfperil$dfperil,osm())") %>% 
	        c("x<-x[x$sp%in%tab$sp,]") %>%
	        c("x<-x[o,]") %>%
	        c("xc<-jitter(coordinates(x),amount=jit)") %>%
	        c("plot(reg,add=TRUE,col=alpha(\"tomato\",0.15),border=NA)") %>% 
	        c(paste0("if(",input$details_sar,"){points(xc[,1],xc[,2],col=alpha(\"orange\",0.5),pch=16,cex=2)}")) %>% 
	        c(paste0("if(",input$details_sar,"){text(xc[,1],xc[,2],labels=tab$ID_Obs,cex=0.7)}")) %>% 
	        c("un<-!duplicated(coordinates(x))") %>%
	        c("text(coordinates(x)[,1][un],coordinates(x)[,2][un],labels=tab$ID_Site[un],cex=0.7,col=\"black\")") %>% 
	        c("```") %>% 
	        c("  ") %>%
	        c(paste0("**",g("fig"),fig_nb,"**. ",g("fig7"),nrow(dfperil$dfperil[dfperil$dfperil$sp%in%lsp,]),g("fig71"))) %>%
	        c("  ")
	      fig_nb<-fig_nb+1
	    
	    ### tableau résumé des mentions et sites et années
	      res<-res %>% c("  ") %>%
	        c(paste0("**",g("tab"),tab_nb,"**. ",g("tab3"))) %>%
	        c("```{r,results=\"asis\", echo=FALSE}") %>% 
	        c("tab2<-ddply(tab,.(sp),function(i){
	          c(length(unique(i$Nom_lieu)),nrow(i),range(i$Anne,na.rm=TRUE),paste(sort(unique(i$ID_Site)),collapse=\", \"))
	        })") %>% 
	        c("names(tab2)[(ncol(tab2)-4):(ncol(tab2))]<-c(\"Nb de sites\",\"Nb de mentions\",\"Mention + ancienne\",\"Mention + récente\",\"Sites\")") %>% 
	        c("row.names(tab2)<-1:nrow(tab2)") %>% 
	        c("panderOptions(\"table.caption.prefix\",\"Tableau X. \")") %>% 
	        c("panderOptions(\"table.continues\",\"Suite...\")") %>% 
          c("pandoc.table(tab2, style=\"multiline\",split.table=Inf,split.cells=c(30,1,1,1,1,40))") %>% 
          c("```")
	      tab_nb<-tab_nb+1
	    
	    
	      if(input$details_sar){
	      ### tableau détaillé des mentions
	        res<-res %>% c("  ") %>%
	          c(paste0("**",g("tab"),tab_nb,"**. ",g("tab4"),nrow(dfperil$dfperil),g("tab41"))) %>%
	          c("```{r,results=\"asis\", echo=FALSE}") %>% 
	          #c(paste0("tab<-tab[rev(order(tab$Abundance)),][1:min(",input$nb_records,",nrow(tab)),]")) %>% 
	          c("tab2<-tab[,c(\"ID_Site\",\"ID_Obs\",\"sp\",\"Abundance\",\"Anne\",\"Nom_lieu\",\"Base\")]") %>% 
	          c("panderOptions(\"table.caption.prefix\",\"Tableau X. \")") %>% 
	          c("panderOptions(\"table.continues\",\"Suite...\")") %>% 
            c("pandoc.table(tab2, style=\"multiline\",split.table=Inf)") %>% 
            c("```")
	        tab_nb<-tab_nb+1
	    
	      ### tableau des mentions les plus récentes pour chaque site
	        #res<-res %>% c("  ") %>%
	          #c(paste0("####Tableau X. Détails des ",length(unique(dfperil$dfperil$Nom_lieu))," sites contenant des espèces en péril et dernières années d'observations.")) %>%
	          #c("```{r,results=\"asis\", echo=FALSE}") %>% 
	          #c("tab2<-ddply(tab,.(sp,ID_Site,Nom_lieu),function(i){max(i$Anne,na.rm=TRUE)})") %>% 
	          #c("tab2<-spread(tab2,\"sp\",\"V1\",fill=\"-\")") %>% 
	          #c("row.names(tab2)<-1:nrow(tab2)") %>% 
	          #c("panderOptions(\"table.caption.prefix\",\"Tableau X. \")") %>% 
	          #c("panderOptions(\"table.continues\",\"Suite...\")") %>% 
            #c("pandoc.table(tab2, style=\"multiline\",split.cells=12)") %>% 
            #c("```")
	      }
	    }
    }
	  
	  if(any(input$section_include=="Other species at risk")){
	    #res<-res %>% c("\\newpage")
      res<-res %>% c(paste0("##",toupper(g("si7")),"  "))
      al<-input[[paste0("s",match("Other species at risk",unlist(sec_list,use.names=FALSE)))]]
	    if(!is.null(val) && val!=""){
	      res<-res %>% c("  ") %>% c(val) %>% c("  ")
	    }
      
      #browser()
      
	    if(!is.null(dfperil$dfperilp)){
	      res<-res %>% c(paste0("```{r fig.width=",fw,", fig.height=",fh,", echo=FALSE}")) %>% 
	        c("tab<-dfperil$dfperilp@data") %>% 
	        c("tab$ID_Carte<-as.numeric(factor(tab$SCOMNAME))") %>%
	        c("par(mar=c(0,0,0,0))") %>%
          c("plot(map.osm)") %>% 
          c("plot(spTransform(region$region,osm()),add=TRUE,col=alpha(\"tomato\",0.15),border=NA)") %>% 
	        c("x<-spTransform(dfperil$dfperilp,osm())") %>% 
	        c("plot(x,add=TRUE,col=alpha(\"orange\",0.3),border=alpha(\"red\",0.7))") %>% 
	        c("text(coordinates(x)[,1],coordinates(x)[,2],labels=tab$ID_Carte,cex=0.75)") %>% 
	        c("```") %>% 
	        c("  ") %>%
	        c(paste0("**",g("fig"),fig_nb,"**. ",g("fig11"))) %>%
	        c("  ")
	      fig_nb<-fig_nb+1
	      res<-res %>% c("  ") %>%
	        c(paste0("**",g("tab"),tab_nb,"**. ",g("tab7"))) %>%
	        c("```{r,results=\"asis\", echo=FALSE}") %>% 
	        c("tab<-unique(tab[,c(\"ID_Carte\",\"SCOMNAME\",\"CLASSE\")])") %>% 
	        c("names(tab)<-c(\"ID_Carte\",\"sp\",\"Classe\")") %>% 
	        c("tab<-tab[order(tab$Classe,tab$sp),]") %>% 
	        c("row.names(tab)<-1:nrow(tab)") %>% 
	        c("panderOptions(\"table.caption.prefix\",\"Tableau 2.\")") %>% 
	        c("panderOptions(\"table.continues\",\"Suite...\")") %>% 
	        c("pandoc.table(tab, style=\"multiline\",split.table=Inf)") %>%
          c("```")
	      tab_nb<-tab_nb+1
	    }
      
	  }
	  
	  
	  if(any(input$section_include=="Critical habitats")){
	    #res<-res %>% c("\\newpage")
	    res<-res %>% c(paste0("##",toupper(g("si8")),"  "))
	    val<-input[[paste0("s",match("Critical habitats",unlist(sec_list,use.names=FALSE)))]]
	    if(!is.null(val) && val!=""){
	      res<-res %>% c("  ") %>% c(val) %>% c("  ")
	    }
	    if(!is.null(ch$ch)){
	      res<-res %>% c(paste0("```{r fig.width=",fw,", fig.height=",fh,", echo=FALSE}")) %>% 
	        c("tab<-ch$ch@data") %>% 
	        c("tab$ID_Carte<-as.numeric(factor(tab$sp))") %>%
	        c("par(mar=c(0,0,0,0))") %>%
          c("plot(map.osm)") %>% 
          c("plot(spTransform(region$region,osm()),add=TRUE,col=alpha(\"tomato\",0.15),border=NA)") %>% 
	        c("x<-spTransform(ch$ch,osm())") %>% 
	        c("plot(x,add=TRUE,col=alpha(\"red\",0.3),border=alpha(\"red\",0.7))") %>% 
	        c("text(coordinates(x)[,1],coordinates(x)[,2],labels=tab$ID_Carte,cex=0.75)") %>% 
	        c("```") %>% 
	        c("  ") %>%
	        c(paste0("**",g("fig"),fig_nb,"**. ",g("fig10"))) %>%
	        c("  ")
	      fig_nb<-fig_nb+1
	      res<-res %>% c("  ") %>%
	        c(paste0("**",g("tab"),tab_nb,"**. ",g("tab6"))) %>%
	        c("```{r,results=\"asis\", echo=FALSE}") %>% 
	        c("tab<-unique(tab[,c(\"ID_Carte\",\"sp\",\"etat\")])") %>% 
	        c("row.names(tab)<-1:nrow(tab)") %>% 
	        c("panderOptions(\"table.caption.prefix\",\"Tableau 2.\")") %>% 
	        c("panderOptions(\"table.continues\",\"Suite...\")") %>% 
          #c("pandoc.table(tab, style=\"multiline\",split.table=Inf)") %>% 
	        c("pandoc.table(tab, style=\"multiline\",split.table=Inf)") %>%
          c("```")
	      tab_nb<-tab_nb+1
	    }
	  }
	  

	  if(any(input$section_include=="Federal lands")){
	    #res<-res %>% c("\\newpage")
	    res<-res %>% c(paste0("##",toupper(g("si9")),"  "))
	    val<-input[[paste0("s",match("Federal lands",unlist(sec_list,use.names=FALSE)))]]
	    if(!is.null(val) && val!=""){
	      res<-res %>% c("  ") %>% c(val) %>% c("  ")
	    }
	    if(!is.null(hdf$hdf)){
	      res<-res %>% c(paste0("```{r fig.width=",fw,", fig.height=",fh,", echo=FALSE}")) %>% 
	        c("tab<-hdf$hdf@data") %>% 
	        c("row.names(tab)<-1:nrow(tab)") %>% 
	        c("tab$ID_Site<-1:nrow(tab)") %>%
	        c("par(mar=c(0,0,0,0))") %>%
          c("plot(map.osm)") %>% 
          c("plot(spTransform(region$region,osm()),add=TRUE,col=alpha(\"tomato\",0.15),border=NA)") %>% 
	        c("x<-spTransform(hdf$hdf,osm())") %>% 
	        c("plot(x,add=TRUE,col=alpha(\"red\",0.3),border=alpha(\"red\",0.7))") %>% 
	        c("xx<-coordinatesall(x)") %>% 
	        c("text(xx$x,xx$y,labels=tab$ID_Site[xx$lab],cex=0.75)") %>% 
	        c("```") %>% 
	        c("  ") %>%
	        c(paste0("**",g("fig"),fig_nb,"**. ",g("fig8"))) %>%
	        c("  ")
	      fig_nb<-fig_nb+1
	      res<-res %>% c("  ") %>%
	        c(paste0("**",g("tab"),tab_nb,"**. ",g("tab5"))) %>%
	        c("```{r,results=\"asis\", echo=FALSE}") %>% 
	        c("tab<-tab[,c(\"ID_Site\",\"Nom\",\"Descrip\",\"Num_RBIF\",\"Source\",\"Annee\")]") %>% 
	        c("panderOptions(\"table.caption.prefix\",\"Tableau 2.\")") %>% 
	        c("panderOptions(\"table.continues\",\"Suite...\")") %>% 
	        c("pandoc.table(tab, style=\"multiline\",split.table=Inf)") %>%
          c("```")
	      tab_nb<-tab_nb+1
	      }
	  }
	  
	  if(any(input$section_include=="Wetlands")){
	    #res<-res %>% c("\\newpage")
	    res<-res %>% c(paste0("##",toupper(g("si10")),"  "))
	    val<-input[[paste0("s",match("Wetlands",unlist(sec_list,use.names=FALSE)))]]
	    if(!is.null(val) && val!=""){
	      res<-res %>% c("  ") %>% c(val) %>% c("  ")
	    }
	    if(!is.null(ap$ap)){
	      res<-res %>% c(paste0("```{r fig.width=",fw,", fig.height=",fh,", echo=FALSE}")) %>% 
	        c("tab<-ap$ap@data") %>% 
	        c("row.names(tab)<-1:nrow(tab)") %>% 
	        c("tab$ID_Site<-1:nrow(tab)") %>%
	        c("par(mar=c(0,0,0,0))") %>%
          c("plot(map.osm)") %>% 
          c("plot(spTransform(region$region,osm()),add=TRUE,col=alpha(\"tomato\",0.15),border=NA)") %>% 
	        c("x<-spTransform(ap$ap,osm())") %>% 
	        c("x$wl<-as.factor(as.character(x$MH_TYPE))") %>%
	        c("bp<-c(\"brown\",brewer.pal(12,\"Paired\")[-c(1,3)])") %>%
	        c("x$col<-bp[as.numeric(x$wl)]") %>%
	        c("plot(x,add=TRUE,col=alpha(x$col,0.7),border=alpha(x$col,0.9),lwd=1)") %>% 
	        c("legend(\"topleft\",cex=0.7,pt.cex=1.7,pch=22,col=alpha(unique(x$col),0.9),pt.bg=alpha(unique(x$col),0.7),legend=unique(x$MH_TYPE),bty=\"o\",bg=alpha(\"white\",0.7),box.lwd=NA,pt.lwd=1,inset=c(0,0))") %>% 
	        #c("xx<-coordinatesall(x)") %>% 
	        #c("text(xx$x,xx$y,labels=tab$ID_Site[xx$lab],cex=0.75)") %>% 
	        c("```") %>% 
	        c("  ") %>%
	        c(paste0("**",g("fig"),fig_nb,"**. ",g("fig9"))) %>%
	        c("  ")
	      fig_nb<-fig_nb+1
	    }
	  }
	  
	  ### add custom ext at the end
	  #val<-input[["s00"]]
	  #if(!is.null(val) && val!=""){
	  #   res<-res %>% c("  ") %>% c(unlist(strsplit(input$s00,"\n"))) %>% c("  ")
	  #}
	  

	  filename<-paste0("urgences_rapport_",gsub(":","",gsub(" ","_",Sys.time())))
	  write(res,paste0(path,"/",filename,".Rmd"))
	  out <- render(paste0(path,"/",filename,".Rmd"),
	      output_format=switch(
          input$report_format,
          .pdf=pdf_document(),.html=html_document(),.docx=word_document(fig_width=6.5,fig_caption=TRUE))
	  )
	  file.remove(file.path(path, paste0(filename,".Rmd")))
	  shell.exec(file.path(path, paste0(filename,input$report_format)))
	},message=g("waitm4"))
	},ignoreNULL=TRUE)
	
	
	observeEvent(input$choose_dir,{
    updateTextInput(session,"chosen_dir",value=choose.dir())
  })
	
	output$downloadBO <- downloadHandler(
	  #filename=paste0("BirdObservations",ifelse(input$csvshpBO=="csv",".csv",".zip")),
	  filename=function(){if(input$csvshpBO=="csv"){paste0(g("fileBO"),".csv")}else{paste0(g("fileBO"),".zip")}},
	  content=function(file){
	    if(input$csvshpBO=="csv"){
        write.csv(df$df@data,file,row.names=FALSE)
      }else{
        fname<-g("fileBO")
        if(file.exists(paste0(fname, ".zip"))){
          file.remove(paste0(fname,".zip"))
        }
        writeOGR(df$df,dsn=tempdir(),layer=fname,driver="ESRI Shapefile",overwrite_layer=TRUE)
        lf<-list.files(tempdir(),pattern=fname,full.names=TRUE)
        lf<-lf[grep(".dbf|.shp|.prj|.shx",lf)]
        zip(paste0(tempdir(),"\\",fname,".zip"),files=lf,zip="C:/Rtools/bin/zip.exe")
        file.copy(paste0(tempdir(),"\\",fname,".zip"),file)
        
      }
	  }
  )
	
		output$downloadSAR <- downloadHandler(
	  filename=function(){if(input$csvshpSAR=="csv"){paste0(g("fileSAR"),".csv")}else{paste0(g("fileSAR"),".zip")}},
	  content=function(file){
	    if(input$csvshpSAR=="csv"){
        write.csv(dfperil$dfperil@data,file,row.names=FALSE)
      }else{
        fname<-g("fileSAR")
        if(file.exists(paste0(fname, ".zip"))){
          file.remove(paste0(fname,".zip"))
        }
        writeOGR(dfperil$dfperil,dsn=tempdir(),layer=fname,driver="ESRI Shapefile",overwrite_layer=TRUE)
        lf<-list.files(tempdir(),pattern=fname,full.names=TRUE)
        lf<-lf[grep(".dbf|.shp|.prj|.shx",lf)]
        zip(paste0(tempdir(),"\\",fname,".zip"),files=lf,zip="C:/Rtools/bin/zip.exe")
        file.copy(paste0(tempdir(),"\\",fname,".zip"),file)
        
      }
	  }
  )
	
		observeEvent(input$value_obs,{
		  #column(3,radioButtons("graph_type",label="Type of graph",choices=list("Use first N observations"=1,"Min. no. of individuals (N)"=2),selected=1)),
		  l<-list(1,2)
		  names(l)<-c(paste(g("tg1"),input$value_obs,g("tg11")),paste(g("tg2"),input$value_obs,g("tg22")))
		  updateRadioButtons(session,inputId="graph_type",choices=l,selected=input$graph_type)
		})
		
		observe({
		  ll<-as.numeric(c(input$lon,input$lat))
		  leafletProxy("map") %>% 
		    clearGroup("target")
		  if(!any(is.na(ll)) && (ll[1]>=(-180) & ll[1]<(-30)) && (ll[2]>(30) & ll[2]<=(90))){
		    leafletProxy("map") %>% 
		      setView(lng=ll[1],lat=ll[2],zoom=9) %>% 
		      addMarkers(lng=ll[1],lat=ll[2],group="target")
		  }
		})
		
		observeEvent(input$selec_leg,{
		  if(is.null(input$legend)){  
		    updateCheckboxGroupInput(session,"legend",selected=setdiff(unlist(leg_list,use.names=FALSE),tail(unlist(leg_list,use.names=FALSE),2)))
		  }else{
		    updateCheckboxGroupInput(session,"legend",selected="")
		  }
		})
	
	  addPopover(session,"barplot1",g("barplot1i"),content=g("barplot1ii"),trigger="hover",placement="top")
	  addPopover(session,"barplot2",g("barplot2i"),content=g("barplot2ii"),trigger="hover",placement="top")
	  addPopover(session,"calendar",g("calendari"),content=g("calendarii"),trigger="hover",placement="left")
	  addPopover(session,"graph3",g("graph3i"),content=g("graph3ii"),trigger="hover",placement="left")
	  addPopover(session,"a1",g("tempi"),content=g("temp1i"),trigger="hover",placement="left")
	  addPopover(session,"a2",g("tempi"),content=g("temp2i"),trigger="hover",placement="left")
	  addPopover(session,"a3",g("tempi"),content=g("temp3i"),trigger="hover",placement="left")
	  addPopover(session,"a4",g("tempi"),content=g("temp4i"),trigger="hover",placement="left")
	  addPopover(session,"a5",g("tempi"),content=g("temp5i"),trigger="hover",placement="left")
    addPopover(session,"a6",g("tempi"),content=g("temp6i"),trigger="hover",placement="left")
    addPopover(session,"a7",g("tempi"),content=g("temp7i"),trigger="hover",placement="left")
    addPopover(session,"a8",g("tempi"),content=g("temp8i"),trigger="hover",placement="left")
	  

	  
})


