

# UI section 

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  tags$div( 
  #or titlePanel("Hello Shiny!")
      tags$h3("East Lothian Covid Tracker - ", head_date)),
     
      tabsetPanel(
        #tab 1
        tabPanel("Dashboard", div(class = "separator"),
                 fluidRow(
                   column(8,
                          tags$h6(class ="padLeft", textOutput("map_title")),
                          girafeOutput("map")),
                   column(4, style='padding-right:40px;', 
                          fluidRow(                            div(class ="box_head", h6(class ="white", "East Lothian Daily Stats: ", head_date)),
                                                               div(class="box",
                                                                   htmlOutput("el_cases"), 
                                                                   htmlOutput("el_deaths"), 
                                                                   htmlOutput("el_tests"), 
                                                                   htmlOutput("el_crude"), 
                                                                   htmlOutput("el_sevenday")
                                                               ),
                            tags$h6(textOutput("bar_title")),
                          div(plotOutput("el_bar", height = 350))
                          )

                          )

                          ),
                 fluidRow(
                   div(class = "line"), 
                   tags$h6(class ="padLeft2", textOutput("la_line_title")),
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
                   tags$h3(class="vcenter", "National Picture in Scotland")),
                   fluidRow(
                   column(3, style='padding-left: 40px;',

                                                   div(class ="box_head", h6(class ="white", "Scotland Daily Stats:", head_date)),
                                                               div(class="box",
                                                                   htmlOutput("scot_cases"), 
                                                                   htmlOutput("scot_deaths"), 
                                                                   htmlOutput("scot_tests"), 
                                                                   htmlOutput("scot_crude"), 
                                                                   htmlOutput("scot_sevenday")
                                                               ),
                          h6("Scottish Cumulative Cases and Deaths"),
                          div(plotOutput("scot_bar", height = 255))),
                   column(6, 
                          h6(class ="padLeft", "Covid Hospitalisation and ICU Numbers"),
                          plotlyOutput("hospitalisation")),
                   column(3, style='padding-right:40px;', 
                          h6("Proportion of Scottish Population who have received First Dose of Vaccination"),
                          plotOutput("vax_one", height = 275),
                          div(class ="box_head", h6(class ="white", "Vaccination Stats")),
                          div(class="box",
                              htmlOutput("had_vax_one"), 
                              htmlOutput("had_vax_two"), 
                          )))
                 )
                 
          
        ),
        #tab 2     
        tabPanel("About", div(class = "separator"),
                 fluidRow(
                   tags$h6(textOutput("demo_text2")),
                   tags$p("This app is updated daily and is up to date as per Public Health Scotland Data Releases.
                                Data at Local Authority level is released after 2 days."),
                   tags$p("All data is taken from Public Health Scotland: Daily Trends, xxxxxxxx"),
                   tags$p("Vaccination and Hospital Data not available at local autgority data.")),
        )),
        div(class = "separator"),
        tags$footer(class = "footer_text", paste0("Produced by Kerr McIntosh"),
                    tags$div(class = "separator"))
            
        )
 