

server <- function(input, output) {
  
  output$map <- renderGirafe({
    # react <- ggplot(data[which(data$model==input$plot1_selected),], aes(x=cty)) +
    #   geom_bar()
      gg <- ggplot(el_map) +
        geom_sf_interactive(aes(fill = CrudeRate7DayPositive, 
                                tooltip = c(paste0(IntZoneName, "\n",real_rate_per_OT,  " infections per 100,000 \n (Actual rate over previous 7 days) \n", Positive7Day, " infections in last 7 days \n(", abs(wow), " ",change,")")),  
                                data_id = IntZoneName)) +
        scale_fill_brewer(palette = "Purples") +
        theme_void() +
        labs(title = "East Lothian Locality Infection Rate ", subtitle = "Click map for locality info" ,fill = "Infections per 100,000 \n(Crude Rate)") +
        guides(shape = guide_legend(override.aes = list(size = 1)),
               color = guide_legend(override.aes = list(size = 1))) +
        theme(legend.title = element_text(size = 7), 
              legend.text = element_text(size = 5),
              legend.position = c(.9,.85))
      map <- girafe(ggobj = gg) 
      # x <- ggiraphOutput(height = .5, width = 1)
      map <- girafe_options(map,
                            opts_zoom(min = 0.5, max = 2),
                            opts_tooltip(css = tooltip_css),
                            opts_sizing(rescale = TRUE, width = .7))
      # if( interactive() ) map
      map
  })
  
  output$bar <- renderPlot({
  ggplot(county_cumulative) +
    aes(y=CumulativeTotals, x = Stats, fill = Stats) +
    geom_col() +
    scale_fill_manual(values = c("#bfd3e6", "#88419d")) +
    theme_light() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 12, vjust=4),
          axis.text.y = element_blank(),
          legend.position = "none",
          line = element_blank(),
          panel.border = element_blank(),
          aspect.ratio = 4/5,
          plot.margin = unit(x = c(0.5, 0, 0, 0), units = "cm"),
          plot.title = element_text(margin=margin(0,0,20,0))) + 
    geom_text(aes(label = CumulativeTotals), vjust = -0.5, size = 6) +
    scale_x_discrete(expand = c(0.1, 0), labels = c("Total Positive Cases", "Total Deaths")) +
    labs(title = "East Lothian Total Cases and Deaths") +
    coord_cartesian(clip = "off") 
  })
  
  
  
    output$demo_text<- renderText({
      paste0("TEST TEXT")
    })
    output$demo_text2<- renderText({
      paste0("TEST TEXT2")
    })
  
  
  }
  