

# UI section 

ui <- fluidPage(tags$div( 
  #or titlePanel("Hello Shiny!")
      headerPanel("East Lothian Covid Tracker")),
     
      tabsetPanel(
        #tab 1
        tabPanel("Dashboard", div(class = "separator"),
                 fluidRow(column(7,
                 girafeOutput("map"))),
                 fluidRow(
                 plotOutput("bar"))
          
        ),
        #tab 2     
        tabPanel("About", div(class = "separator"),
                 fluidRow(
          tags$h6(textOutput("demo_text2"))),
        )),
        div(class = "separator"),
        tags$footer(class = "footer_text", paste0("Produced by Kerr McIntosh"),
                    tags$div(class = "separator"))
            
        )
 