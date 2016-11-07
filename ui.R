

shinyUI(navbarPage(strong(g("app")),windowTitle=g("app"),collapsible=FALSE,position="fixed-top",
  
  tabPanel(g("content"),
    fluidPage(
      #titlePanel(strong("UrgencesAviR")),
      sidebarLayout(
        sidebarPanel(
          h1(strong(g("app"))),
          shiny::p(g("desc1"),strong(g("desc2")),strong(g("desc3")),strong(g("desc4")),strong(g("desc5")),strong(g("desc6")),g("desc7"),strong(g("desc8")),g("desc9")),
          shiny::p(),
          h2(g("hauthors")),
          shiny::p(),
          shiny::p(g("author1"),span(g("em1"),style="color:blue"),""),
          shiny::p(g("author2"),span(g("em2"),style="color:blue"),""),
          h2(g("hcontacts")),
          shiny::p(),
          shiny::p(g("contact1"),span(g("contact2"),style="color:blue"),""),
          shiny::p(),
          h2(g("hlink")),
          shiny::p(),
          shiny::p(shiny::a(g("link1"),href="http://www.ec.gc.ca/ap-pa/default.asp?lang=En&n=BB16043C-1",target="_blank")),
          shiny::p(shiny::a(g("link4"),href="http://www.tbs-sct.gc.ca/dfrp-rbif/query_question/number_numero-fra.aspx",target="_blank")),
          shiny::p(shiny::a(g("link5"),href="http://www.ec.gc.ca/default.asp?lang=Fr&n=E826924C-1",target="_blank")),
          shiny::p(shiny::a(g("link6"),href="http://www.sararegistry.gc.ca/sar/index/default_f.cfm",target="_blank")),
          shiny::p(shiny::a(g("link3"),href="https://www.servicesenligne.mddep.gouv.qc.ca/Atlas/OuvertureSession.aspx?ReturnUrl=%2fAtlas%2fNavigateurCartographique.aspx",target="_blank")),
          shiny::p(shiny::a(g("link2"),href="http://geoavirweb.ddns.net/",target="_blank"))
          
          
       
          #h2("Databases"),
          #p("EPOQ, Sauvagine, Oies, BIOMQ, Macreuses, etc.") 
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(g("intab1"),  
              h2(g("husage")),
              shiny::p(),
              shiny::p(g("usage1"),strong(g("usage2")),g("usage3"),strong(g("usage4")),g("usage5")),
              h2(g("hcontent")),
              shiny::p(),
              shiny::p("  - ",strong(g("content1")),g("content11")),
              shiny::p("  - ",strong(g("content2")),g("content22")),
              shiny::p("  - ",strong(g("content3")),g("content33")),
              shiny::p("  - ",strong(g("content4")),g("content44")),
              shiny::p("  - ",strong(g("content5")),g("content55")),
              shiny::p("  - ",strong(g("content6")),g("content66")),
              #shiny::p("  - ",strong("About"),": general information concerning the tool and its content."),
              h2(span(strong(g("hwarning")),style="color:tomato")),
              shiny::p(),
              shiny::p(g("warning1"),span(strong(g("warning11")),style="color:tomato"),g("warning111")),
              shiny::p(g("warning2"),span(strong(g("warning22")),style="color:tomato"),g("warning222"))
            ),
            
            tabPanel(g("intab2"),
              tags$iframe(style="height:700px; width:100%; padding:0; border:none; margin:0; frameborder=0;", src="www/PIU2012.pdf")
            ),
            
            tabPanel(g("intab3"),
              tags$iframe(style="height:700px; width:100%; border:none; margin:0; frameborder=0; z-index:999999;", src="www/NWERCP.pdf")
            ),
            
            tabPanel(g("intab5"),
              tags$iframe(style="height:700px; width:100%; border:none; margin:0; frameborder=0; z-index:999999;", src="www/PNIUES.pdf")
            ),
            
            tabPanel(g("intab6"),
              tags$iframe(style="height:700px; width:100%; border:none; margin:0; frameborder=0; z-index:999999;", src="www/decision_FR.png")
            ),
          
            tabPanel(g("intab4"),

              #lapply(seq_along(bdi),function(i){
              #  if(i%%2==1){
              #    shiny::p(h4(strong(bdi[i])))
              #  }else{
              #    shiny::p(bdi[i])
              #  }
              #}),
              
              lapply(seq_along(bdi),function(i){
                if(i%in%seq(1,200,by=3)){
                  shiny::p(h4(strong(bdi[i])))
                }else{
                  if(i%in%seq(2,200,by=3)){
                    shiny::p(bdi[i])
                  }else{
                    shiny::p(em("Personne contact:"),bdi[i])
                  }
                }
              })
              
            )
          )
        )
      )
    )
  ),
  
  
  tabPanel(g("content1"),
     div(class="outer",
       tags$style(type="text/css", "body {padding-top: 80px;}"), 
       tags$head(
         includeCSS("www/styles.css")
       ),
       #tags$head(tags$link(rel="stylesheet",type="text/css",href="style.css")),
	     tags$head(
		     tags$style(HTML('#create_report{background-color:tomato}'))
	     ),
       tags$head(
		     tags$style(HTML('#choose_dir{background-color:lightblue}'))
	     ),
	     tags$head(
		     tags$style(HTML('#select_region{background-color:tomato}'))
	     ),
	     tags$head(
		     tags$style(HTML('#show_data{background-color:tomato}'))
	     ),
       tags$head(
		     tags$style(HTML('#clear_selection{background-color:tomato}'))
	     ),
       tags$head(
		     tags$style(HTML('#clear_map{background-color:tomato}'))
	     ),
       tags$head(
		     tags$style(HTML('#resetSelectionBO{background-color:lightblue}'))
	     ),
       tags$head(
		     tags$style(HTML('#resetSelectionSAR{background-color:lightblue}'))
	     ),
       tags$head(
		     tags$style(HTML('#selec_leg{background-color:lightblue}'))
	     ),
       
       leafletOutput("map",height="100%",width="100%"),
       
       #bsModal("modal","title","butt_modal",size="large",
      #   textOutput("text_modal")
       #),
       
    	 absolutePanel(id = "controls1", class = "panel panel-default", fixed = TRUE,
             draggable = FALSE, top = "8%", left = "1%", right = "auto", bottom = "auto",
             width = 240, height = 600, #640
    	   #h3(g("hcontrol")),  
         shiny::p(),
	       tags$div(title="",selectInput("month",label=g("cmonth"),choices=list("All months","01","02","03","04","05","06","07","08","09","10","11","12"),selected="09",multiple=TRUE,selectize=TRUE)),
			   numericInput("min_year",label=g("cyear"),value=1980),
	       selectInput("database",label=g("cbase"),choices=as.list(c(g("bdall"),sort((unique(d$Base))))),selected=g("bdall"),multiple=TRUE,selectize=TRUE),
	       numericInput("min_nb",label=g("cmin"),value=1),  
			   numericInput("buff_size",label=g("cbuffer"),value=50),
	       #radioButtons("obs_buffer",label="Show data within the EPOQ buffer",choices=list("No"=1,"Yes"=2),selected=1,inline=TRUE),
	       checkboxInput("use_clusters",label=g("uclust"),value=FALSE),
    	   checkboxInput("select_mode",label=g("enable"),value=TRUE),
    	   textInput("lat",label=NULL,placeholder="Latitude (e.g. 49.0000)",width="100%"),
    	   textInput("lon",label=NULL,placeholder="Longitude (e.g. -65.0000)",width="100%")#,
    	   #actionButton("butt_modal",label="modal")
    	 ),
       
       absolutePanel(id = "controls2", class = "panel panel-default", fixed = TRUE,
             draggable = FALSE, top = "auto", left = "34%", right = "auto", bottom = "0%",
             width = 506, height = 46,
    	   tags$div(style="display:inline-block",actionButton("select_region",g("butt1"),width="120px")),
         tags$div(style="display:inline-block",actionButton("show_data",g("butt2"),width="120px")),
    	   tags$div(style="display:inline-block",actionButton("clear_map",g("butt4"),width="120px")),
         tags$div(style="display:inline-block",actionButton("clear_selection",g("butt3"),width="120px"))
         
       ),
       
       absolutePanel(id = "controls3", class = "panel panel-default", fixed = TRUE,
              draggable = FALSE, top = "8%", left = "auto", right = "1%", bottom = "auto",
              width = 255, height = 370, #h295
         column(2,plotOutput("plot_legend",height=313,width="200%")),
         column(10,checkboxGroupInput("legend",label=NULL,choices=leg_list,selected=setdiff(unlist(leg_list,use.names=FALSE),tail(unlist(leg_list,use.names=FALSE),2)))),
         column(12,actionButton("selec_leg",g("legbutt")))

    	 )
     )
   ),
   
  
   tabPanel(g("content2"),
     fluidPage(
	     titlePanel(h4(strong(g("hba")))),	
	     shiny::p(),
		   column(3,radioButtons("graph_type",label=g("tg"),choices=list("Use first N observations"=1,"Min. no. of individuals (N)"=2),selected=1,width="100%")),
       column(2,numericInput("value_obs",label="",value=75)),
       column(2,radioButtons("radio_group",label=g("gb"),choices=lgb,selected=2)),
       column(3,checkboxInput("show_obs",label=g("sobs"),value=FALSE),checkboxInput("show_perc",label=g("sper"),value=FALSE)),
       fluidRow(
         shiny::p()
       ),
       tabsetPanel(
			   tabPanel(g("batab1"),plotOutput("barplot1",height=500,width="90%")),
         tabPanel(g("batab2"),plotOutput("barplot2",height=500,width="90%"))
		   )
     )
   ),
  
  
  tabPanel(g("content3"),
     fluidPage(
	     titlePanel(h4(strong(g("htd")))),	
	     shiny::p(),
		   sidebarLayout(
		     sidebarPanel(
		       #titlePanel(h4(strong("Telemetry data"))),
		       shiny::p(),
		       titlePanel(h4(strong(g("htd2")))),
		       shiny::p(g("tdi")),
		       shiny::p(),
		       checkboxInput("show_region",g("tdsr"),value=FALSE),
		       shiny::p(),
		       sliderInput("opacity",label=g("tdco"),min=0,max=1,value=0.5,round=FALSE,ticks=TRUE),
           shiny::p(),
		       shiny::p(strong(g("tdtab1"))),
		       shiny::p(g("td1i")),
		       shiny::p(strong(g("tdtab2"))),
		       shiny::p(g("td2i")),
		       shiny::p(strong(g("tdtab3"))),
		       shiny::p(g("td3i")),
		       shiny::p()
		     ),
		     mainPanel(
           tabsetPanel(id="tabs",
			       tabPanel(g("tdtab2"),value="razo",
		           leafletOutput("map_razo",height="700px",width="98%")
		         ),
             tabPanel(g("tdtab1"),value="noga",
			         leafletOutput("map_noga",height="700px",width="98%")
			       ),
			       tabPanel(g("tdtab3"),value="rtlo",
			         leafletOutput("map_rtlo",height="700px",width="98%")
			       )
           )
		     )
		   )
     )
  ),
  
  
  tabPanel(g("content4"),
    fluidPage(
	    sidebarLayout(  
	      sidebarPanel(
	        titlePanel(h4(strong(g("hsc")))),
	        radioButtons("radio2",label=g("ob"),choices=lob,selected=3),
          radioButtons("radio1",label=g("ct"),choices=lct,selected=2)#,
          #radioButtons("radio3",label="Group by",choices=list("Species"=1,"Groups"=2),selected=1)
	      ),
	      mainPanel(
          tabsetPanel(
            tabPanel(g("sctab1"),shiny::p(),shiny::p(),plotOutput("calendar",width="100%")),
            tabPanel(g("sctab2"),shiny::p(),selectInput("select_sp",label=g("sctab2ss"),choices=unique(d$sp),multiple=FALSE),shiny::p(),plotOutput("graph3",width="80%"))
          )
	      )
      )
    )
  ),
  
  
  tabPanel(g("content5"),
     fluidPage(
	     titlePanel(h4(strong(g("hd")))),	
	     shiny::p(),
		   tabsetPanel(
			   tabPanel(g("dtab1"),shiny::p(),column(2,actionButton("resetSelectionBO",label=g("resetbutt"))),column(2,downloadButton("downloadBO", g("downbutt"))),column(2,radioButtons("csvshpBO",label=g("downdat"),choices=ldd,selected="csv",inline=TRUE)),shiny::p(),div(dataTableOutput("table1"),style="font-size:75%")),
		     tabPanel(g("dtab5"),shiny::p(),div(dataTableOutput("table5"),style="font-size:75%")),
		     tabPanel(g("dtab2"),shiny::p(),column(2,actionButton("resetSelectionSAR",label=g("resetbutt"))),column(2,downloadButton("downloadSAR", g("downbutt"))),column(2,radioButtons("csvshpSAR",label=g("downdat"),choices=ldd,selected="csv",inline=TRUE)),shiny::p(),div(dataTableOutput("table3"),style="font-size:75%")),
		     tabPanel(g("dtab7"),shiny::p(),div(dataTableOutput("table7"),style="font-size:75%")),
		     tabPanel(g("dtab6"),shiny::p(),div(dataTableOutput("table6"),style="font-size:75%")),
		     tabPanel(g("dtab3"),shiny::p(),div(dataTableOutput("table2"),style="font-size:75%")),
		     tabPanel(g("dtab4"),shiny::p(),div(dataTableOutput("table4"),style="font-size:75%"))
		   )
     )
  ),
  
  tabPanel(g("content6"),
     fluidPage(
     #div(class="inner",   
       #tags$head(includeHTML("W:/PIU/Projet_BanqueDeDonnees/PROJET_R/Code_R/shiny/map.html")),
     sidebarLayout(
       sidebarPanel(
         
         shiny::p(),
         #actionButton("choose_dir",label=g("foldbutt")),
         shiny::p(),
         radioButtons("report_format",label=g("rf"),choices=lrf,selected=".docx",inline=TRUE),
         shiny::p(),
         checkboxInput("produce_png",label=g("pngplots"),value=FALSE),
         shiny::p(),
         textInput("reporter",label=g("name"),value="",placeholder=g("name2")),
         shiny::p(),
         titlePanel(h4(strong(g("hcr1")))),
         titlePanel(h4("")),
         shiny::p(),
         column(6,checkboxGroupInput("section_include",label=strong(g("si")),choices=sec_list,selected=unlist(sec_list,use.names=FALSE))),
         shiny::p(),
         column(6,checkboxGroupInput("plot_include",label=strong(g("pi")),choices=lpi,selected=1:4)),
         fluidPage(),
         shiny::p(),
         titlePanel(h4(strong(g("hcr2")))),
         titlePanel(h4("")),
         shiny::p(),
         sliderInput("nb_records",label=g("norecords"),min=1,max=50,value=10,round=TRUE,ticks=FALSE),
         shiny::p(),
         sliderInput("nb_calendars",label=g("nospecies"),min=1,max=35,value=10,round=TRUE,ticks=FALSE),
         shiny::p(),
         selectInput("select_sar",label=g("sar"),choices="",multiple=TRUE,selectize=TRUE),
         shiny::p(),
         checkboxInput("details_sar",label=g("sardetails"),value=FALSE),
         shiny::p(),
         actionButton("create_report",g("crbutt")),
         textInput("chosen_dir",label="",value=tempdir(),placeholder=g("rap_loc")),
         shiny::p()
       ),
       mainPanel(
         tabsetPanel(
           tabPanel(g("crtab1"),
             shiny::p(),
             #textareaInput(id="a0",rows=3,placeholder=g("temp0")),
             shiny::p(),
             titlePanel(h4(strong(g("htemp")))),
             titlePanel(h4(strong(""))),
             shiny::p(),
             lapply(seq_along(NWERCP_list),function(i){textareaInput(paste0("a",i),NWERCP_list[[i]],"",rows=3)})
           ),
           tabPanel(g("crtab2"),
             titlePanel(h4(strong(g("hcrtab2")))),
             shiny::p(),
             #textareaInput(id="s0",rows=3,placeholder=g("rs0")),
             lapply(seq_along(sec_list),function(i){textareaInput(paste0("s",i),names(sec_list)[i],"",rows=3)})
             #textareaInput(id="s00",rows=3,placeholder=g("rs0"))
           ),
           tabPanel(g("crtab3"),
             tags$iframe(style="height:700px; width:100%; border:none; margin:0; frameborder=0; z-index:999999;", src="www/rapport_exemple.pdf")
           )
         )
       )
     )
    )
  ),
  
  #bsTooltip("select_region",g("butt1i"),placement="top"),
  #bsTooltip("clear_map",g("butt4i"),placement="top"),
  #bsTooltip("show_data",g("butt2i"),placement="top"),
  #bsTooltip("clear_selection",g("butt3i"),placement="top"),
  #bsTooltip("controls3",g("legi"),placement="left"),
  #bsTooltip("min_nb",g("uclusti"),placement="right"),  
	#bsTooltip("buff_size",g("cbufferi"),placement="right"),  
  #bsTooltip("use_clusters",g("uclusti"),placement="right"),  
  #bsTooltip("select_mode",g("enablei"),placement="right"),  
  #bsTooltip("graph_type",g("tgi"),placement="top"),
  #bsTooltip("show_obs",g("sobsi"),placement="top"),
  #bsTooltip("show_perc",g("speri"),placement="top"),
  #bsTooltip("lat",g("lati"),placement="right"),
  #bsTooltip("lon",g("loni"),placement="right"),
  #bsTooltip("selec_leg",g("legbutti"),placement="left"),
  
  
inverse=FALSE))