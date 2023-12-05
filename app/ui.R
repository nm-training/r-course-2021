

ui<-dashboardPagePlus(
  
  
  header =dashboardHeaderPlus(title = h5("PDF finder"), enable_rightsidebar = TRUE),
  
  
  dashboardSidebar(
      
      
        
        fileInput("files_sanjay", "Input pdfs", multiple =T),
        uiOutput("up"), br()
      
  ),
      
  dashboardBody(tabsetPanel(
          
          tabPanel("Criteria", div(
            style = "height: 700px;overflow-y: scroll", fluidRow(column(4,uiOutput("dnd_crit_enter")), column(3,uiOutput("submit_btn"))), uiOutput("dnd_ui"), fluidRow(column(3,uiOutput("find")), column(2, ""), column(4, uiOutput("rem_all")))
                   ,br(), br(),fluidRow(column(4,uiOutput("sliders")), column(4, downloadButton("save_final", "Save!"))), br()
                   ,
                   tabsetPanel(
                     tabPanel("Graph", plotOutput("plt")),
                     tabPanel("Table", tableOutput("f_t"))
                   )
                   ))
          
        )
        
        
      ),
  
  
  rightSidebar(background = "dark",
               rightSidebarTabContent(id = 1, title = "Created by",icon = "info", active = T ,h5(" ©SanjayHamal"), h5("sanjay.hamal@numericmind.com"))
               
  )
      
  
)      
    
    
    
    
  
