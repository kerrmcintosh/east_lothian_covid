

# UI section 

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  tags$div( 
  #or titlePanel("Hello Shiny!")
      headerPanel("East Lothian Covid Tracker - DATE")),
     
      tabsetPanel(
        #tab 1
        tabPanel("Dashboard", div(class = "separator"),
                 fluidRow(
                   column(4, class = "col3_pad",style='padding-left:40px;', 
                          fluidRow(
                            tags$h6(textOutput("bar_title")
                                    ),
                          div(plotOutput("el_bar", height = 320))
                          ),
                            div(class ="box_head", h6(class ="white", "East Lothian Stats")),
                          div(class="box",
                            textOutput("el_cases"), 
                            textOutput("el_tests"), 
                            textOutput("el_deaths"), 
                            textOutput("el_crude"), 
                            paste("CRUDE RATES: EL & SCOT")
                                     
                          )
                          ),
                          column(8,
                                 tags$h6(textOutput("map_title")),
                                 girafeOutput("map", height = 500))
                          ),
                 fluidRow(
                   div(class = "line"), 
                   tags$h6(textOutput("la_line_title")),
                          column(2,class ="radio_buttontop",
                                        tags$div(class ="radio_button",
                                                 radioButtons("la_line_plot",
                                                             tags$h5(""),
                                                             choices = c("Cases", "Deaths", "Tests"),
                                                             selected = "Cases")
                 )),
                 column(10, 
                        plotlyOutput("la_line"))),
                 fluidRow(div(class ="shade",
                   tags$h3("National Picture"),
                   column(3,
                          h6(div(class ="shade", "Scottish Cumulative Cases and Deaths")),
                          div(plotOutput("scot_bar", height = 275)),
                          fluidRow(textOutput("scot_cases")),
                          fluidRow(textOutput("scot_tests")),
                          fluidRow(textOutput("scot_deaths")),
                          fluidRow(textOutput("scot_crude")),),
                   column(6,
                          h6("Covid Hospitalisation and ICU Numbers"),
                          plotlyOutput("hospitalisation")),
                   column(3,
                          h6("Proportion of Scottish Population who have received First Dose of Vaccination"),
                          plotOutput("vax_one", height = 275),
                          fluidRow(paste("number vaxed")),
                          fluidRow(paste("number vaxed 2 and %"))))
                 )
                 
          
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
 