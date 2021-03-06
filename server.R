

server <- function(input, output) {
  
  output$map_title<- renderText({
    paste0("East Lothian Locality Covid Picture: ", local_box_date)
  })
  
  output$map <- renderGirafe({

      gg <- ggplot(el_map) +
        geom_sf_interactive(aes(fill = CrudeRate7DayPositive, 
                                tooltip = c(paste0(IntZoneName, "\n",real_rate_per_OT,  " infections per 100,000 (Actual rate over previous 7 days) \n", Positive7Day, " infections in last 7 days \n(", abs(wow), " ",change,")")),  
                                data_id = IntZoneName)) +
        scale_fill_brewer(palette = "Purples") +
        theme_void() +
        geom_text(x=10, y=30, label="Scatter plot") +
        labs(title = "Hover over map for locality info" ,fill = "Infections per 100,000 \n(Crude Rate)") +
        guides(shape = guide_legend(override.aes = list(size = 1)),
               color = guide_legend(override.aes = list(size = 1))) +
        theme(legend.title = element_text(size = 7), 
              legend.text = element_text(size = 5),
              legend.position = c(.9,.9),
              plot.margin = margin(0, 0, 0, 0, "cm"),
              plot.title = element_text(size = 11, face = "italic", colour = "#696969"),
              plot.subtitle = element_text(size = 9))
      map <- girafe(ggobj = gg) 
      # x <- ggiraphOutput(height = .5, width = 1)
      map <- girafe_options(map,
                            opts_zoom(min = 0.5, max = 2),
                            opts_tooltip(css = tooltip_css),
                            opts_sizing(rescale = TRUE, width = .7),
                            opts_selection(type = "none"))
      map
  })
  
  output$bar_title<- renderText({
    paste0("Total Cases and Deaths")
  })
  
  output$el_bar <- renderPlot({
  ggplot(county_cumulative) +
    aes(y=CumulativeTotals, x = Stats, fill = Stats) +
    geom_col() +
    scale_fill_manual(values = c("#bcbddc", "#88419d")) +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, vjust=4),
          axis.text.y = element_blank(),
          legend.position = "none",
          line = element_blank(),
          panel.border = element_blank(),
          aspect.ratio = 4/5
          # plot.margin = margin(0, 0, 0, 0, "cm"),
          # plot.title = element_text(family = "Helvetica", face = "bold", size = (15), color = "#696969", margin=margin(0,0,20,0))
          ) +
          # plot.margin = unit(x = c(0.5, 0, 0, 0), units = "cm"),
    geom_text(aes(label = prettyNum(CumulativeTotals, big.mark=",",scientific=FALSE)), vjust = -0.5, size = 6) +
    scale_x_discrete(expand = c(0.1, 0), labels = c("Positive Cases", "Deaths")) +
    # labs(title = "Total Cases & Deaths") +
    coord_cartesian(clip = "off") 
  })
  
  output$la_line_title<- renderText({
    paste0("Daily ", input$la_line_plot ," Over Time / 7 Day Rolling Average: East Lothian vs Scotland")
  })

  output$la_line <- renderPlotly({  
  CovidTime <- ggplotly(
    CovidTimeLine %>% 
      filter(Stats == input$la_line_plot) %>% 
      filter(RollingStats == case_when(input$la_line_plot == "Cases" ~"RollingCases", 
                                       input$la_line_plot == "Deaths" ~ "RollingDeaths",
                                       TRUE ~"RollingTests"))%>% 
      ggplot() +
      geom_line(aes(x= Date, y = Numbers, colour = Region)) + 
      geom_line(aes(x= Date, y = RollingNumbers, group = Region), colour = "#696969") + 
      # stat_smooth(aes(x= Date, y = Numbers), inherit.aes = FALSE) +
      theme_classic() +
      scale_colour_manual(values = c("#998ec3","#e08214")) +
      scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
      theme(
        # legend.title = element_blank(),
            legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major.y = element_line( size=.1, color="#bfd3e6" )) +
      facet_grid(rows = vars(Region), scales = "free"))
      # labs(title = "Cases over Time")) 
  # CovidTime %>%  layout(legend = list(orientation = 'v', x = 0.35, y = 1.15), yaxis = y_la) 
  
  })
  
    output$demo_text<- renderText({
      paste0("TEST TEXT")
    })
    output$demo_text2<- renderText({
      paste0("TEST TEXT2")
    })
  
    output$scot_bar <- renderPlot({
      ggplot(national_total_data) +
        aes(y=CumulativeTotals, x = Stats, fill = Stats) +
        geom_col() +
        scale_fill_manual(values = c("#bcbddc", "#88419d")) +
        theme_light() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = 12, vjust=4),
              axis.text.y = element_blank(),
              legend.position = "none",
              line = element_blank(),
              panel.border = element_blank(),
              aspect.ratio = 4/5

        ) +
        geom_text(aes(label = prettyNum(CumulativeTotals, big.mark=",",scientific=FALSE)), vjust = -0.5, size = 6) +
        scale_x_discrete(expand = c(0.1, 0), labels = c("Positive Cases", "Deaths")) +
        coord_cartesian(clip = "off") 
    })
    
    output$el_cases<- renderText({
      paste0(("<b>New Cases: </b>"), el_daily$DailyPositive)
    })
    
    output$el_deaths<- renderText({
      paste0("<b>Deaths: </b>", el_daily$DailyDeaths)
    })
    
    output$el_tests<- renderText({
      paste0("<b>Tests: </b>", el_daily$TotalTests)
    })
    
    output$el_sevenday<- renderText({
      paste0("<b>Cases in last 7 days: </b>", el_7day)
    })
    
    output$el_crude<- renderText({
      paste0("<b>Crude Infection Rate: </b>", round(el_crude_today), " per 100,000 over previous 7 days")
    })
    
    output$el_vax<- renderText({
      paste0(("<b>First Dose Vaccinated: </b>"), east_lothian_vax, "% (weekly statistic as of ", weekly_vax_date, ")")
    })
# DATA WAS BEING TAKEN FROM PHS DATASET - BUT THE ACTUAL FIGURE LISTED ON PHS DAILY UPDATE IS MORE UP TO DATE (AND
# so has been manually entered instead)  -  latest daily figure from dataset is often only a 1/3 of PHS dashboard reported daily total

    # output$scot_cases<- renderText({
    #   paste0("<b>New Cases: </b>",prettyNum(scot_total$DailyPositive, big.mark=",",scientific=FALSE))
    # })
    
    output$scot_cases<- renderText({
      paste0("<b>New Cases: </b>",prettyNum(655, big.mark=",",scientific=FALSE))
    })
    
    # output$scot_deaths<- renderText({
    #   paste0("<b>Deaths: </b>", scot_total$DailyDeaths)
    # })
    
    output$scot_deaths<- renderText({
      paste0("<b>Deaths: </b>", 56)
    })
    
    # output$scot_tests<- renderText({
    #   paste0("<b>Tests: </b>", prettyNum(scot_total$TotalTests, big.mark=",",scientific=FALSE))
    # })
    # 
    
    output$scot_tests<- renderText({
      paste0("<b>Tests: </b>", prettyNum(16031, big.mark=",",scientific=FALSE))
    })
    
    output$scot_crude<- renderText({
      paste0("<b>Crude Infections Rate: </b>", round(scot_crude_today ), " per 100,000 over previous 7 days")
    })
    
    output$hospitalisation <- renderPlotly({  
      hospitalisation_data <- ggplotly(
        ggplot(hospitalisation_data) +
          geom_line(aes(x= date, y = numbers, colour = hospitalisation )) + 
          theme_classic() +
          scale_colour_manual(values = c("#e08214", "#998ec3"), labels=c("Covid All Hospital Patients", "Covid ICU Patients")) +
          scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +    
          theme(legend.title = element_blank(),
                panel.grid.major.y = element_line( size=.1, color="#bfd3e6" ),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()) )
      
      hospitalisation_data %>%  layout(legend = list(orientation = 'v', x = 0.1, y = 0.93))
      
    })
    output$vax_one <- renderPlot({
    first_vax_pie <- first_vax_pie + 
      coord_polar("y", start=0)+ 
      scale_fill_brewer(palette = "Purples", labels = c("Had First Dose", "Not Had First Dose")) + 
      theme_void() +
      theme(axis.text.x=element_blank(),
            legend.title = element_blank(),
            legend.position = "top") +
      geom_text(aes(label = paste0(split_numbers, " %")),
                position = position_stack(vjust = 0.5)) 
    first_vax_pie
    })
   
    output$had_vax_one<- renderText({
      paste0("<b>", prettyNum(vax_data$FirstDose, big.mark=",",scientific=FALSE), "</b> have had first dose")
    }) 

    output$had_vax_two<- renderText({
      paste0("<b>", prettyNum(vax_data$SecondDose, big.mark=",",scientific=FALSE), "</b>  have had second dose (", vax_data$propotion_second,"%)")
    }) 
    
    output$over_80_vax<- renderText({
      paste0("<b>", over80_popn, "%</b> of over 80s have had first dose (weekly statistic as of ", weekly_vax_date, ")")
    })
    output$github <- renderUI({
      url <- a("github.com/kerrmcintosh", href="https://www.github.com/kerrmcintosh")
      tagList(url)
    })
    
    output$linkedin <- renderUI({
      url <- a("linkedin.com/kerr-mcintosh", href="https://www.linkedin.com/in/kerr-mcintosh/")
      tagList(url)
    })
    
    output$daily_url <- renderUI({
      url <- a("Public Health Scotland Open Data", href="https://www.opendata.nhs.scot/dataset/covid-19-in-scotland")
      tagList(url)
    })
    
    output$trends_url <- renderUI({
      url <- a("Scottish Government Statistics", href="https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/")
      tagList(url)
    })
   
    output$spatial_url <- renderUI({
      url <- a("Scottish Government SpatialData.gov.scot", href="https://data.gov.uk/dataset/133d4983-c57d-4ded-bc59-390c962ea280/intermediate-zone-boundaries-2011 ")
      tagList(url)
    })
    
    
    
  }
  