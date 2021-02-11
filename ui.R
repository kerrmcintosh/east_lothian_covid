

# UI section 

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  tags$div( 
  #or titlePanel("Hello Shiny!")
      tags$h3(class = "head_left", "East Lothian Covid-19 Tracker"), h6(class = "head_right", "Updated: ", Head_title_date)),
     
      tabsetPanel(
        #tab 1
        tabPanel("Dashboard", div(class = "separator"),
                 div(class = "mobile", h4(class = "mobile_warning",  "VIEW IN LANDSCAPE MODE for optimum performance")),
                 fluidRow(
                   column(8,
                          div(class ="box_head", h6(class ="white_center", textOutput("map_title"))),
                          girafeOutput("map")),
                   column(4, style='padding-right:40px;', 
                          fluidRow(                            div(class ="box_head_2line", h6(class ="white", "East Lothian Daily Stats: ", local_box_date)),
                                                               div(class="box",
                                                                   htmlOutput("el_cases"), 
                                                                   htmlOutput("el_deaths"), 
                                                                   htmlOutput("el_tests"), 
                                                                   htmlOutput("el_crude"), 
                                                                   htmlOutput("el_sevenday"),
                                                                   htmlOutput("el_vax")
                                                               ),
                                                               div(class ="box_head_2line", h6(class ="white", "East Lothian Total Cases & Deaths: ", local_box_date)),
                                                               div(class="box",
                          div(plotOutput("el_bar", height = 350)))
                          )

                          )

                          ),
                 fluidRow(
                   # div(class = "line"), 
                   div(class ="box_head_90", tags$h6(class ="white_center", textOutput("la_line_title"))),
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

                                                   div(class ="box_head_2line", h6(class ="white", "Scotland's Daily Stats:", head_date)),
                                                               div(class="box",
                                                                   htmlOutput("scot_cases"), 
                                                                   htmlOutput("scot_deaths"), 
                                                                   htmlOutput("scot_tests"), 
                                                                   htmlOutput("scot_crude"), 
                                                                   htmlOutput("scot_sevenday")
                                                               ),
                          div(class ="box_head_2line", h6(class ="white", "Total Cases & Deaths: ", head_date)),
                          div(class="box",
                          div(plotOutput("scot_bar", height = 300)))),
                   column(6, 
                          div(class ="box_head", h6(class ="white_center", "Covid Hospitalisation and ICU Numbers")),
                          # h6(class ="padLeft", "Covid Hospitalisation and ICU Numbers"),
                          plotlyOutput("hospitalisation")),
                   column(3, style='padding-right:40px;', 
                          div(class ="box_head_pie", h6(class ="white", "Proportion of Scottish Population who have received First Dose of Vaccination: ", head_date)),
                          div(class="box_pie",
                          plotOutput("vax_one", height = 275)),
                          div(class ="box_head", h6(class ="white", "Vaccination Stats: ", head_date)),
                          div(class="box",
                              htmlOutput("had_vax_one"), 
                              htmlOutput("had_vax_two"),
                              htmlOutput("over_80_vax")
                          )))
                 )
                 
          
        ),
#tab 2
        tabPanel("About", div(class = "separator"),
                 fluidRow(
                   column(8,
                   div(class = "about_us",
                   tags$h6("About This Dashboard"),
                   tags$p("This app has been designed and hard coded using R Shiny to give insight and visualisation into East Lothian data related to Covid-19."),
                   tags$p("The app is updated daily as per Public Health Scotland Data Releases. Daily figures are confirmed cases/deaths/test at the time of writing. 
                          Because of delays in reporting, daily figures will be subject to change.  Data at Local Authority level is released after 3 days. A weekly update of historical figures normally takes place every Tuesday. 
                        "),
                   tags$p("Data Totals are cumulative numbers since 28th February 2020 as per Public Health Scotland Data. Death statistics are any death identfied as Covid related.
                          Hospital Data is only available from September 2020.  Hospital Data is not available at local authority level.  Hospital data is included at national level for info purposes. Age related and local authority data
                          is released weekly and hence explains time lag of data shown"),
                   tags$p(h5("All covid data is provided by Public Health Scotland: "), uiOutput("daily_url"), " Daily Case Trends By Local Authority, Daily Case Trends By Neighbourhood, Cumulative Data",
                   uiOutput("trends_url"), " Trends in daily COVID-19 data"),
                   tags$p(h5("Population Figures"), "Locality and national population data is also taken from 'Daily Case Trends By Neighbourhood'.  This is an estimate of population from National Records of Scotland as per June 2019."),
                   tags$p(h5("Scottish Locality Spatial data is taken from:  "), uiOutput("spatial_url"), "Intermediate Zone Boundaries 2011 - 
                          The localities / Intermediate Zones were designed to meet constraints on population thresholds (2,500 - 6,000 household residents), based on the UK 2011 Census,  and nest within local authorities.
There are 1,279 Intermediate Zones (localities) covering the whole of Scotland."),

                   )),
                   column(4,
                          tags$div(img(class = "symbol", src="east_lothian.jpg"))
        )),
        div(class="large_div")
        )),
        div(class = "separator"),
        tags$footer(class = "footer_text", uiOutput("github"), uiOutput("linkedin"),
                    tags$div(class = "separator"))
            
        )


