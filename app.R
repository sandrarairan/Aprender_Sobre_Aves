library(shiny)
library(rlist)
library(shinyWidgets)

# Para que se mustren las imagenes y videos es necesario crear una carpeta de www
fileaudio<-list.files("www/",pattern="(.wav)") # produce un vector de caracteres de los nombres de archivos [1] "Sicalis flaveola.wav"  "Spinus psaltria.wav"   "Synallaxis azarae.wav"
file_audio<-sapply(fileaudio,function(x) tools::file_path_sans_ext(x),USE.NAMES = F) # devuelve las rutas de archivo sin extensiones (y el punto inicial) [1] "Sicalis flaveola"  "Spinus psaltria"   "Synallaxis azarae"


filevideo<-list.files("www/",pattern="(.mp4)") 
file_video<-sapply(filevideo,function(x) tools::file_path_sans_ext(x),USE.NAMES = F) #

imgname<-sapply(file_audio,function(x) gsub(pattern="_.+$",replacement="",x))
imgfile<-paste0(imgname,".jpg")
specfile<-paste0(file_audio,"_especto.png")
#videofile<-paste0(imgname,".mp4")
answers<-sapply(file_audio,function(x) gsub("_XC.*","",x),USE.NAMES = F)

familia<-c("Thraupidae","Fringillidae","Furnariidae")


# Define la UI
ui <- navbarPage(title="Aves Reserva de Castilla",position="fixed-top",theme=shinythemes::shinytheme("darkly"),
                
                 tabPanel("Adivinar ¿Qué Ave es?", 
                          fluidPage(
                            #cololar syles
                            tags$head(
                              tags$style(HTML("
                       img{
                       margin: 20px;
                       border-width: 5px;
                       border-style: solid;
                       max-height: 200px;
                       max-width: 100%;
                       width: auto;
                       display: inline;
                       }
                      
                      body {padding-top: 70px;}
                      @media screen and (max-width: 768px) {
                      body { padding-top: 170px; }
                        }
                      
                       .sidebar{
                         border-color: #317eac;
                         border-width: 2px;
                         border-style: solid;
                         background: #67bf73;
                         color: white;}
                       
                       .callButton{
                         background: #174023;
                         color: white;
                       }
                       .well{
                       border-width: 3px;
                       border-style: solid; 
                       border-color: #555555;
                       
                       }
                       h3{
                       background-color: #555555;
                       color: white;
                       margin-left:0px; 
                       margin-right: 0px;
                       text-indent: 8px;
                       width: 100%;
                       padding: 0px;
                       }
                       ")
                              )
                            ),
                            sidebarPanel(class="sidebar",width=3,title="Setup",
                                        
                                         fluidRow(
                                           p(strong("Mostrar")),
                                           div(style="display:inline",
                                               checkboxInput("showpic","Imagen",F),
                                               checkboxInput("showspec","Espectograma",F),
                                               checkboxInput("showaudio","Canto",T),
                                               checkboxInput("showvideo","Video",F)
                                           ),
                                           sliderInput("picsize","Tamaño Imagen",20,1000,100,step=20,ticks=F,width="100%"),
                                           actionButton("reset","Reset",class="btn btn-secondary",icon=icon("times-circle")),
                                           br(),
                                           tags$br(),
                                           shinyWidgets::prettyToggle("reveal","Mostrar Respuestas","Ocultar Respuesta"))
                                        
                            ), #end sidebar
                            mainPanel(width=9,
                                      fluidRow( 
                                        tags$br(), 
                                        uiOutput('dynamic'),
                                        tags$br(),
                                        actionButton("call","Próximo Canto",class="btn btn-primary"),
                                        br(),br()
                                      )
                            )#End main panel
                          )),#End adivinar Tab
                 tabPanel("Aves Urbanas Manizales",
                           
                          mainPanel(class="body",width=10,
                                  
                                    lapply(1:length(file_audio),function(i) {
                                      wellPanel(fluidRow(
                                        h3(paste0(i,": ",answers[i])),
                                        img(src=imgfile[i],height="200px",width="auto"),
                                        img(src=specfile[i]),
                                        tags$audio(src=fileaudio[i],type = "audio/wav", controls = "false"),
                                        tags$video(src=filevideo[i],type = "video/mp4", autoplay =NA , controls =NA,height="200px",width="auto"),
                                        #tags$video(id="ave", type = "video/mp4",src = "Sicalisflaveola.mp4", controls = "controls")
                                        
                                      )
                                      )
                                    })
                          )
                 ),
                 tabPanel("Navbar 1",
                          
                          mainPanel(class="body",width=10,
                            tabsetPanel(
                              tabPanel("Tab 1",
                                       
                                       h4("Table"),
                                       tableOutput("table"),
                                       h4("Verbatim text output"),
                                       verbatimTextOutput("txtout"),
                                       h5("Header 5"),
                                       lapply(1:length(file_audio),function(i) {
                                         wellPanel(fluidRow(
                                           h3(paste0(i,": ",answers[i])),
                                           img(src=imgfile[i],height="200px",width="auto"),
                                           h4(familia[i]),
                                           tags$h5(familia[i]),
                                           
                                         )
                                         )
                                       })
                              ),
                              tabPanel("Tab 2", "This panel is intentionally left blank"),
                              tabPanel("Tab 3", "This panel is intentionally left blank")
                            )
                          )
                 )#fin tabpanel1 navbar
                 
                 
                 
                              
                 
)#fin UI



#------------------------------------------------
# Define server 
server <- function(input, output) {
  vals<-reactiveValues()
  vals$counter=1
  
  fileaudio<-list.files("www/",pattern="(.wav)") #find audio files; 
  file_audio<-sapply(file_audio,function(x) tools::file_path_sans_ext(x),USE.NAMES = F) 
  
  #Sample the files initially (random)
  isolate({
    vals$ordr<-sample(1:length(file_audio),length(file_audio),replace=F)
  })
  
  #Reset Button
  observeEvent(input$reset,{
    vals$counter <- 1
    vals$ordr<-sample(1:length(file_audio),length(file_audio),replace=F)
    print(paste0("counter=",vals$counter))
    print(paste0("order=",vals$ordr))
    
  })
  
  #next bird
  observeEvent(input$call,{
    vals$counter=vals$counter+1
    print("---------------------")
    key<-as.matrix(t(rbind(1:vals$counter,file_audio[vals$ordr[vals$counter]])))
    rownames(key)<-rep("",vals$counter)
    colnames(key)<-c("Sound","Answer")
    print(key)
  })
  
  
  fullQuiz<-reactive({
    out<-list()
    
    for(i in 1:vals$counter){
      #which sample of the randomized order are we on?
      whichSample<-vals$ordr[i]
      #assemble pieces of well panel
      audio<-if(input$showaudio){tags$audio(src=fileaudio[whichSample],type = 'audio/wav', controls = 'false')}else{}
      
      pic<- if(input$showpic){img(src=imgfile[whichSample],height="200px",width="auto")}else{}
      
      spec<- if(input$showspec){img(src=specfile[whichSample])}else{}
      
      video<-if(input$showvideo){tags$video(src=filevideo[whichSample],type = 'video/mp4', controls = NA,height="200px",width="auto")}else{}
      
      #Muestra la ave ó oculta la información del ave
      text<- if(input$reveal){h3(paste0(i,": ",answers[whichSample]))}else{paste0("¿ Qué Ave es ? ",i)}
      
      out_i<-wellPanel(fluidRow(audio,spec,pic,video,text))
      out[[i]]<-out_i
    }#end for loop
    
    out
  }
  )
  
  
  output$dynamic<-renderUI({
   
    tagList(fullQuiz()[1:vals$counter])
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)