#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(plotly)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

#GDP Data cleaning and graphing

#setwd("~/Desktop/Final_Project/Iceland")

GDP_all <- read.csv("GDP.csv", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)


names(GDP_all)[1] <- "Country"


iceland_gdp <- GDP_all %>%
  filter(Country == "Iceland")

iceland_gdp <- iceland_gdp %>% select(-`Country Code`)
iceland_gdp <- iceland_gdp %>% select(-`Indicator Name`)
iceland_gdp <- iceland_gdp %>% select(-`Indicator Code`)


iceland_data_long <- iceland_gdp %>%
  pivot_longer(-Country, names_to = "Year", values_to = "Value") %>%
  filter(Country == "Iceland") %>%
  mutate(
    Year = as.numeric(Year),
    Value = as.numeric(Value),
    Variable = "GDP"   # So it matches input$var == "GDP"
  )

#Population Density Data graphing

city_data <- data.frame(
  city = c("Reykjavík", "Akureyri", "Selfoss", "Egilsstaðir", "Ísafjörður"),
  lat = c(64.1355, 65.6839, 63.9333, 65.2667, 66.0756),
  lon = c(-21.8954, -18.1105, -21.0028, -14.3948, -23.1200),
  population = c(135000, 19000, 7000, 2500, 2600)
)

#Parcipitation Data 
precip_raw <- read_delim("parcipitation.csv")
colnames(precip_raw)

names(precip_raw) <- trimws(names(precip_raw))
precip_long <- precip_raw %>%
  pivot_longer(
    cols = -`Weather station`,
    names_to = "Month",
    values_to = "Precipitation"
  ) %>%
  rename(Station = `Weather station`)
precip_long$Month <- factor(precip_long$Month,
                            levels = c("January", "February", "March", "April", "May", "June", "July", "August")
)

#National Park Information

parks <- data.frame(
  name = c("Þingvellir", "Vatnajökull", "Snæfellsjökull"),
  lat = c(64.255, 64.5, 64.8),
  lon = c(-21.129, -17.0, -23.783),
  intro = c(
    "UNESCO World Heritage Site; historical and geological significance.",
    "Europe's largest glacier and volcanoes; spans ~14% of Iceland.",
    "Volcano + glacier; setting for 'Journey to the Center of the Earth'."
  )
)


library(shiny)
library(bslib)
library(leaflet)
library(plotly)

# ── A modern Bootstrap 5 theme
my_theme <- bs_theme(
  version    = 5,
  bootswatch = "minty",      # pick the style you like
  primary    = "#00629B",
  base_font  = font_google("Inter")
)

# ── UI -----------------------------------------------------------------
ui <- navbarPage(
  title  = div(icon("globe-europe"), "Discover Iceland"),
  theme  = my_theme,
  collapsible = TRUE,
  id = "main_nav",
  
  # 1 Introduction ------------------------------------------------------
  tabPanel(
    "Introduction",
    fluidPage(
      
      fluidRow(
        column(
          width = 6,
          tags$img(
            src = "Iceland_1.jpg",
            width = "100%",
            style = "border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.2); margin-bottom: 20px;"
          )
        ),
        column(
          width = 6,
          tags$img(
            src = "Flag.jpg",
            width = "100%",
            style = "border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.2); margin-bottom: 20px;"
          )
        )
      ),
      
      fluidRow(
        column(
          width = 5,
          div(
            class = "card shadow-sm p-4",
            style = "min-height: 420px;",
            p("Iceland, a land of fire and ice, is a Nordic island nation situated in the North Atlantic Ocean. 
             It is known for its stunning natural landscapes, including active volcanoes, vast glaciers, geysers, lava fields, and black-sand beaches. 
             With a small population of around 370,000, most residents live in or around the capital city of Reykjavík."),
            p("Iceland’s location on the Mid-Atlantic Ridge results in a geologically active environment, giving rise to geothermal energy, 
             which powers most of the country’s electricity and heating systems."),
            p("The country also boasts a rich cultural heritage rooted in Norse traditions. Its literary legacy includes medieval sagas and modern-day innovation. 
             Visitors can enjoy puffin and whale watching, explore glacier caves, or relax in world-famous hot springs."),
            p("Use the interactive map to explore Iceland’s geographic position in the North Atlantic.")
          )
        ),
        
        column(
          width = 7,
          div(
            class = "card shadow-sm p-2",
            leafletOutput("iceland_neighbors_map", height = 400)
          )
        )
      )
    )
  )
  ,
  
  
  tabPanel(
    "📊 Social Science Statistics",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("var_social", "Choose metric:",
                    choices = c("GDP", "City Population"))
      ),
      mainPanel(
        conditionalPanel(
          "input.var_social == 'GDP'",
          div(class = "card shadow-sm",
              plotlyOutput("stats_plot", height = 400),
              div(class = "card-footer small text-muted",
                  p("Interpretation:",
                    "In the 2010s and early 2020s, tourism became the dominant sector, contributing significantly to GDP through services such as hospitality, transportation, and cultural exports. The number of tourists visiting Iceland has grown exponentially, surpassing the population many times over each year. Meanwhile, Iceland continues to capitalize on its abundant geothermal and hydroelectric resources, which power its industries sustainably and attract energy-intensive operations like data centers and aluminum smelting.
                      Despite its small scale, Iceland ranks high globally in terms of GDP per capita, reflecting a high standard of living, robust social systems, and a focus on sustainable development. However, its economic reliance on tourism and global market fluctuations also makes it vulnerable to shocks, such as during the COVID-19 pandemic., ",
                    "renewable energy exports, and tech start‑ups.")))
        ),
        conditionalPanel(
          "input.var_social == 'City Population'",
          div(class = "card shadow-sm",
              leafletOutput("city_pop_map", height = 420),
              div(class = "card-footer small text-muted",
                  p("Iceland's total population is just over 370,000, making it one of the most sparsely populated countries in Europe. Yet despite its modest population, Iceland has a dynamic urban structure, where population density and services are heavily concentrated in a few key urban areas, while much of the country remains rural or uninhabited. Click to view the popultion of the city.

                    .")))
        )
      )
    )
  ),
  
 
  tabPanel(
    "🌿 Environmental Statistics",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("var_env", "Choose metric:",
                    choices = c("National Park", "2024 Precipitation"))
      ),
      mainPanel(
        conditionalPanel(
          "input.var_env == 'National Park'",
          div(class = "card shadow-sm",
              leafletOutput("national_parks_map", height = 400),
              div(class = "card-footer small text-muted",
                  p("Iceland protects roughly 14 % of its land area as national parks.
                    click to view breif intro")))
        ),
        conditionalPanel(
          "input.var_env == '2024 Precipitation'",
          div(class = "card shadow-sm",
              plotlyOutput("precip_plot", height = 420),
              div(class = "card-footer small text-muted",
                  p("Southern stations such as Kirkjubæjarklaustur receive the highest rainfall.
                    click to view the exact data")))
        )
      )
    )
  ),
  
  
  tabPanel(
    "🖼️ Gallery",
    fluidRow(
      column(6,
             tags$img(src = "Iceland_2.jpg", width = "100%",
                      class = "shadow-sm mb-3")),
      column(6,
             tags$img(src = "Iceland_3.jpg", width = "100%",
                      class = "shadow-sm mb-3"))
    )
  ),
  
  tabPanel(
    "📜 History",
    fluidPage(
      h2("A Brief History of Iceland"),
      br(),
      div(
        class = "card shadow-sm p-4",
        style = "font-size: 16px; line-height: 1.7;",
        p("Iceland's history begins with Norse settlers who arrived in the late 9th century, 
         primarily from Norway and the British Isles. The island was largely uninhabited, 
         aside from a few Irish monks who soon departed. These settlers established a unique 
         society based on farming, fishing, and communal law."),
        
        p("In 930 AD, the Alþingi (Althing), one of the world’s oldest parliaments, was founded at Þingvellir, 
         marking the beginning of Iceland’s tradition of democratic governance. Despite harsh climate and 
         volcanic disruptions, early Icelanders maintained a cohesive culture documented through epic sagas 
         and oral traditions."),
        
        p("In the 13th century, Iceland came under Norwegian and later Danish rule, which lasted for over 600 years. 
         It wasn’t until the 20th century that Iceland began asserting independence—first becoming a sovereign 
         state under the Danish crown in 1918, and fully independent as a republic in 1944."),
        
        p("Since then, Iceland has transformed into a modern, progressive society with strong environmental values, 
         a thriving cultural scene, and one of the world’s highest standards of living—all while preserving 
         its deep-rooted Norse heritage.")
      )
    )
  ),
  tabPanel("📚 Citations & Acknowledgments",
           fluidPage(
             h3("Sources and Citations"),
             
             tags$div(
               class = "mb-4",
               tags$ul(
                 tags$li(
                   strong("Images:"),
                   tags$ul(
                     tags$li("Aurora over Iceland and waterfall photo: National Geographic"),
                     tags$li("Icelandic flag image: Wikipedia Commons (Public Domain)")
                   )
                 ),
                 tags$li(
                   strong("Data Sources:"),
                   tags$ul(
                     tags$li("GDP and population statistics: World Bank and Statistics Iceland"),
                     tags$li("Precipitation data: Icelandic Meteorological Office"),
                     tags$li("City locations and population: WorldPop and open civic datasets"),
                     tags$li("National park boundaries: Protected Planet (UNEP-WCMC, WDPA)")
                   )
                 ),
                 tags$li(
                   strong("Textual Content:"),
                   tags$ul(
                     tags$li("Historical background and descriptions adapted from Wikipedia articles on Iceland")
                   )
                 )
               )
             ),
             
             h4("Acknowledgments"),
             tags$p(
               "This project was developed by Haoyi Liu."
             ),
             tags$p(
               "Sincere thanks to ",
               strong("Professor Haviland Wright"),
               " for his guidance and support throughout the semester."
             ),
             tags$p(
               "Portions of the code, design layout, and content integration were supported by ",
               strong("ChatGPT (OpenAI)"),
               " for refining the user interface and assisting in technical implementation using R Shiny."
             )
           )
  )
  
)

server <- function(input, output, session) {
  
  observeEvent(input$show_modal, {
    showModal(modalDialog(
      title = "About this site",
      p("This Shiny site combines maps and interactive charts to explore Iceland."),
      easyClose = TRUE, footer = NULL
    ))
  })
  
  # Iceland + nearby regions map
  output$iceland_neighbors_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -20, lat = 65, zoom = 4) %>%
      addMarkers(lng = -21.94, lat = 64.14, popup = "🇮🇸 Reykjavík (Iceland)") %>%
      addMarkers(lng = -22.0, lat = 72.0, popup = "🇬🇱 East Greenland") %>%
      addMarkers(lng = -8.7, lat = 62.0, popup = "🇫🇴 Faroe Islands") %>%
      addMarkers(lng = -22.6, lat = 63.4, popup = "Vestmannaeyjar (South Iceland Islands)") %>%
      addCircles(lng = -21.94, lat = 64.14, radius = 100000, color = "blue", opacity = 0.4)
  })
  
  # National parks map (shown only when selected)
  output$national_parks_map <- renderLeaflet({
    leaflet(parks) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -19.5, lat = 64.7, zoom = 6) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        label = ~name,
        popup = ~paste0("<b>", name, "</b><br>", intro),
        radius = 6,
        color = "forestgreen",
        fillOpacity = 0.7,
        labelOptions = labelOptions(
          direction = "auto",
          style = list("font-weight" = "bold"),
          textsize = "14px"
        )
      )
  })
  
  #Population PLot
  output$city_pop_map <- renderLeaflet({
    leaflet(city_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 5,
        color = "tomato",
        fillOpacity = 0.6,
        popup = ~paste0("<b>", city, "</b><br>Population: ", format(population, big.mark = ","))
      ) %>%
      addLabelOnlyMarkers(
        lng = ~lon, lat = ~lat,
        label = ~paste(city, "(", format(population, big.mark = ","), ")"),
        labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE)
      )
  })
  
  #Parcipitation Plot
  output$precip_plot <- renderPlotly({
    p <- ggplot(precip_long, aes(x = Month, y = Precipitation, fill = Station)) +
      geom_col(position = "dodge") +
      labs(
        title = "Monthly Precipitation by Weather Station",
        x = "Month",
        y = "Precipitation (mm)",
        fill = "Station"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  
  
  
  # GDP plot
  output$stats_plot <- renderPlotly({         
    p <- ggplot(iceland_data_long, aes(Year, Value)) +
      geom_line(color = "#0072B5") +
      geom_point(color = "#0072B5") +
      labs(x = "Year", y = "GDP (current US$)",
           title = "Iceland GDP 1960‑2022") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y"))
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
