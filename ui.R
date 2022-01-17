
ui <- dashboardPage(
  skin = "black",
  title = "Lab Data Graphic Visualization",
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(
    titleWidth = 300 
    
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(
    width = 300,
    introBox(data.step = 3, data.intro = ' ', #  intro tour
             div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
             sidebarMenu(
               introBox(data.step = 1, data.intro = 'Upload before clicking', # intro tour
                        div(id = "sidebar_button",
                            bsButton(inputId = "confirm", 
                                     label = "START", 
                                     icon = icon("play-circle"), 
                                     style = "danger")
                        )
               ),
               div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
               menuItem(
                 "Tag#",
                 tabName = "Tag#",
                 icon = icon("spinner"),
                 
                 checkboxGroupInput(
                   inputId = "tagInput",
                   label = "Tag#",
                   choices = unique(di_plot2$tag_n),
                   selected = unique(di_plot2$tag_n)
                 )
               )
               ,
               br(),
               br(),
               menuItem(
                 "Group Comparison",
                 tabName = "gc",
                 icon = icon("spinner"),
                 checkboxGroupInput(
                   inputId = "gcInput",
                   label = "",
                   choices = unique(di_plot2$sort_n),
                   selected = unique(di_plot2$sort_n)[1:2],
                   inline = TRUE
                 ) 
               ),
               br(),
               br(), 
               menuItem(
                 "UPLOAD SELECTION",
                 tabName = "upload",
                 icon = icon("upload"),
                 textInput(
                   inputId = "filename",
                   placeholder = "Name upload file",
                   label = ""
                 ),
                 div(
                   fileInput('file1', 'Choose Template File',
                             accept=c('.xlsx'))
                 )
               ),
               br(),
               br(),
               menuItem(
                 "DOWNLOAD SELECTION",
                 tabName = "download",
                 icon = icon("download"),
                 textInput(
                   inputId = "filename",
                   placeholder = "Name download file",
                   label = ""
                 ),
                 div(
                   downloadButton(
                     outputId = "downloadData",
                     label = "Save Volum/Weight Data",
                     icon = icon("download"),
                     style = "color: black; margin-left: 15px; margin-bottom: 5px;"
                   )
                 ),
                 div(
                   downloadButton(
                     outputId = "downloadMicroData",
                     label = "Save Survival Data",
                     icon = icon("download"),
                     style = "color: black; margin-left: 15px; margin-bottom: 5px;"
                   )
                 )
               ),
               br()
               
             )
    )),
  
  
  # BODY --------------------------------------------------------------------
  
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet", 
        type = "text/css", 
        href = "radar_style.css")
    ),
    
    useShinyjs(),
    introjsUI(),
    
    # MAIN BODY ---------------------------------------------------------------
    
    fluidRow(
      column(
        width = 12,
        introBox(
          bsButton("v", 
                   label = "TUMOR VOLUME",  
                   style = "success"),
          bsButton("w", 
                   label = "WEIGHT", 
                   style = "success"),
          bsButton("gc", 
                   label = "GROUP COMPARISON",  
                   style = "success"),
          bsButton("s", 
                   label = "SURVIVAL",   
                   style = "success"),
          data.step = 2, data.intro = ' ')
      )
    ),
    
    fluid_design("w_panel", "box1", "box2", NULL, NULL),
    fluid_design("gc_panel", "box5", NULL, NULL,NULL),
    fluid_design("s_panel", "box_los1", "box_los2", NULL, NULL),
    
    fluidRow(
      div(
        id = "v_panel", 
        column(
          width = 12,
          introBox(data.step = 4, data.intro =' ',
                   uiOutput("box_pat")
          )
        ),
        column(
          width = 6,
          uiOutput("box_pat2")
        ),
        column(
          width = 6,
          uiOutput("box_year")
        )
      )
    )
  )
)