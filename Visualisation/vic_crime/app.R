library(tidyverse)
library(shiny)
library(dplyr)
library(shinydashboardPlus)
library(shinydashboard)
library(plotly)
library(leaflet)
library(sf)
library(colorspace)
library(htmltools)
library(reshape2)
library(janitor)
#https://www.google.com/url?sa=i&url=https%3A%2F%2Fwww.pxfuel.com%2Fen%2Fquery%3Fq%3Dcrime%2Bscene&psig=AOvVaw3waATmEAT0xq8GleSpti0x&ust=1697873836676000&source=images&cd=vfe&opi=89978449&ved=0CBAQjRxqFwoTCIiCkomPhIIDFQAAAAAdAAAAABAR


load(here::here("final_dat/total_incidents.rda"))
crime_lga_tot_incidents <- crime_lga_tot_incidents %>%
  mutate(`Local Government Area` = str_to_title(`Local Government Area`))

lga_choices <- sort(unique(crime_lga_tot_incidents$`Local Government Area`), decreasing = FALSE)

load(here::here("final_dat/offence.rda"))
crime_lga_offence <- crime_lga_offence %>%
  mutate(`Local Government Area` = str_to_title(`Local Government Area`))

load(here::here("final_dat/offence_by_postcode.rda"))
crime_lga_offence_bypostcode <- crime_lga_offence_bypostcode %>%
  mutate(`Local Government Area` = str_to_title(`Local Government Area`))

load(here::here("final_dat/location_type.rda"))
crime_lga_location <- crime_lga_location %>%
  mutate(`Local Government Area` = str_to_title(`Local Government Area`))

vic_map <- read_sf(here::here("final_dat/gda2020_vicgrid/VMADMIN/LGA_POLYGON.shp"))

vic_map <- vic_map %>%
  mutate(`Local Government Area` = gsub("\\([^\\)]+\\)", "", LGA_NAME)) %>%
  select(`Local Government Area`, geometry) %>%
  mutate(`Local Government Area` = str_to_title(`Local Government Area`)) %>%
  arrange(`Local Government Area`)

sf_crime_lga_tot_incidents <- crime_lga_tot_incidents %>%
  left_join(vic_map, by = "Local Government Area")

load(here::here("final_dat/satisfaction_living_standard.rda"))

clean_ssl <- clean_ssl %>%
  rename(`Living Standard Satisfaction Index` = Average_Score)

load(here::here("final_dat/se_disadvantage.rda"))
SED_index <- SED_index %>%
  mutate(`Local Government Area` = str_to_title(`Local Government Area`))

load(here::here("final_dat/edu_occupation.rda"))
EandO_index <- EandO_index %>%
  mutate(`Local Government Area` = str_to_title(`Local Government Area`))

socio1 <- clean_ssl %>% left_join(SED_index, by = "Local Government Area")
socio_eco <- socio1 %>% left_join(EandO_index, by = "Local Government Area")

# Define UI for application that draws a histogram
ui <- dashboardPage(

  dashboardHeader(title = "Victoria Criminal",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Weclome!🎉🎉🎉",
                                 message = "Dive in the criminal analysis",
                                 icon =icon("envelope")),
                               messageItem(
                                 from = "Author",
                                 message = "Hoang Do",
                                 icon =icon("image-portrait")),
                               menuItem("GitHub profile", icon = icon("github"),
                                        href = "https://github.com/justin-git01"))),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome Page", tabName = "image", icon = icon("house")),
      menuItem("Total incidents rate", tabName = "total_rate", icon = icon("magnifying-glass")),
      menuItem("Sub-category analysis", tabName = "Sub_category_analysis", icon = icon("people-robbery")),
      menuItem("Map", tabName = "Map", icon = icon("map")),
      menuItem("Socio-economic factor", tabName = "Socio_economic_factor", icon = icon("handcuffs")),
      menuItem("About", tabName = "About", icon = icon("address-card"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem("image",
              tags$div(
                div(
                  style = "background-color: rgba(255, 255, 255, 0.5); padding: 20px; text-align: center;",
                  h1("VICTORIA CRIMINAL INCIDENT ANALYSIS")
                )
              ),
              imageOutput("main_img")
      ),

      tabItem("total_rate", class = "active",
              sidebarPanel(
                       # Input for LGA
                       selectizeInput(inputId = "LGA",
                                      label = "Local Government Area",
                                      choices = lga_choices)
                       # ,
                       # # Input for Year
                       # selectizeInput(inputId = "year",
                       #                label = "Year",
                       #                choices = unique(crime_lga_tot_incidents$Year))
                     ),
              mainPanel(
                       textOutput("plot_title"),
                       plotlyOutput("barplot_by_year"),
                       titlePanel("How to use this app？")
                     ),

                     fluidRow(
                       box(
                         width = 12,
                         class = "about-section",
                         title = "Instruction for users",
                         uiOutput("user1")
                       )
                     )
             ),

             tabItem("Sub_category_analysis",
                     fluidRow(
                       column(width = 4,
                              selectizeInput(inputId = "LGA2",
                                             label = "Local Government Area",
                                             choices = lga_choices),

                              selectizeInput(inputId = "year2",
                                             label = "Year",
                                             choices = unique(crime_lga_tot_incidents$Year))
                       )
                       ,

                       column(width = 8,
                              box(
                                width = 10,
                                class = "about-section",
                                title = "Instruction for users",
                                uiOutput("user2")
                              ))
                     ),

                     fluidRow(column(width = 4, plotlyOutput("radar_chart_location_type")),
                              column(width = 4, plotlyOutput("radar_chart_suburbs")),
                              column(width = 4, plotlyOutput("radar_chart_principal_offence"))
                     )
             ),

             tabItem("Map",
                     fluidRow(
                       column(width = 4,
                              selectizeInput(inputId = "PR",
                                             label = "Police Region",
                                             choices = c("All", unique(crime_lga_tot_incidents$`Police Region`))),
                              selectizeInput(inputId = "LGA3",
                                             label = "Local Government Area",
                                             choices = c("All", lga_choices)),
                              selectizeInput(inputId = "year3",
                                             label = "Year",
                                             choices = unique(crime_lga_tot_incidents$Year))
                       )
                        ,

                       column(width = 8,
                              box(
                                width = 10,
                                class = "about-section",
                                title = "Instruction for users",
                                uiOutput("user3")
                              ))
                     ),
                     fluidRow(
                       leafletOutput("map")
                     )
             ),

             tabItem("Socio_economic_factor",

                     fluidRow(
                       box(
                         width = 10,
                         class = "about-section",
                         title = "Instruction for users",
                         uiOutput("user4")
                       )
                     ),
                     fluidRow(
                       plotlyOutput("socio_eco")
                     )
             ),

      tabItem("About",
              fluidRow(column(width = 10, box(
                width = 10, status = "success", title = "About this app", collapsible = TRUE, solidHeader = TRUE,
                HTML(
          ' <li> This app aims to provide users with detailed insights about Victoria criminal incidents. </li> <br>
            <li> Users can interact with the app by choosing different input for LGAs and Year. </li> <br>
            <li> Analysis result and user guide can be found in each tab. </li> <br>
           <li> Feel free to send any feedback or improvement via author email. </li>')
              ))
              ),
              fluidRow(
                column(width = 10,
                       box(width = 10, status = "success", title = "Creator", collapsible = TRUE, solidHeader = TRUE,
                           p("Author: Hoang Do, Master of Actuarial Studies, Monash University"),
                           p("Email: vdoo0002@student.monash.edu"),
                           p("Student ID: 29780977"),
                           p("App URL: https://justin01.shinyapps.io/vic_crime/")
                       ))
              ),
              # Include local CSS file
              includeCSS("styles.css"))
    )
  )


)


server <- function(input, output) {

  output$user1 <- renderText({
    HTML(' <li> Bar plot can be used to compare <b> Year on Year </b> incidents rate per 100,000 population. </li> <br>
            <li> Moreover, those rates can be compared to the <b> Victoria total rate </b> during that year. </li> <br>
            <li> Please hover on bars and line for further information. </li> <br>
           <li> User can switch between different <i> Local Government Area </i> using the selective box. </li> <br>
           <li> These figures are interactive with user\'s choice of LGA and Year. </li> ')
  })


  tot_crime_vic <- crime_lga_tot_incidents %>%
    group_by(Year) %>%
    summarise(`Total Victoria incidents` = sum(`Rate per 100,000 population`))

  joined_dat <- reactive({
    crime_lga_tot_incidents %>%
      filter(`Local Government Area` == input$LGA) %>%
      left_join(tot_crime_vic, by = "Year") %>%
      rowwise() %>%
      mutate(percentage = round(`Rate per 100,000 population`/`Total Victoria incidents`*100, 3))
  })

  output$barplot_by_year <- renderPlotly({
    p <- plot_ly()

    # Add the left y-axis for specific LGA
    p <- add_bars(p, data = joined_dat(), x = ~Year, y = ~`Rate per 100,000 population`, type = 'bar', name = input$LGA,
                  hoverinfo = "text",
                  text = ~paste("For the year of <b>", Year, "</b>, the criminal incident rate per 100,000 population for <b>", input$LGA, "</b> was <b>",
                                round(`Rate per 100,000 population`, 2), "</b>, as <b>", percentage[Year == Year], "%</b> of the Victorian rate",  sep = ""),
                  hoverlabel = list(bgcolor = toRGB("white"),
                                    font = list(color = toRGB("black"))
                  )
    )


    # Add the right y-axis for Victoria as a whole
    p <- add_lines(p, data = joined_dat(), x = ~Year, y = ~`Total Victoria incidents`, type = 'scatter', mode = 'lines', yaxis = 'y2', name = 'Victoria',
                   hoverinfo = "text",
                   text = ~paste("For the year of <b>", Year, "</b>, the criminal incident rate per 100,000 population for Victoria was <b>", round(`Total Victoria incidents`, 2),"</b>", sep = ""),
                   hoverlabel = list(bgcolor = toRGB("orange"),
                                     font = list(color = toRGB("white"))
                   )
    )

    # Customize the layout to have two y-axes
    p <- layout(p, barmode = 'group', yaxis = list(title = ""), yaxis2 = list(overlaying = 'y', side = 'right'))

    p
  })


  # Dynamic plot title
  output$plot_title <- renderText({
    paste(input$LGA, "Criminal Incident Rate compared to Victoria per 100,000 population")
  })


  # Tab 2

  output$user2 <- renderText({
    HTML('<li>These interactive radar charts enable users to assess <b> Year on Year </b> total incidents across the top 5 items in <i> 3 distinct categories </i>. </li>
            <li>These three radar charts display the <i> top 5 location types </i>, <i> top 5 suburbs </i>, <i>and top 5 principal offenses</i>, respectively, for the selected Local Government Area and year. </li>
            <li>Feel free to hover over data points for additional details. User can also rotate the axis by hovering over the titles on the left side of the chart. </li> ')
  })

  ## Location Type
  selected_data_location <- reactive({
    filter(crime_lga_location, `Local Government Area` == input$LGA2)

  })

  # Find the top 5 location types with the highest number of incidents for the current year
  top_location_types_current <- reactive({
    selected_data_location() %>%
      filter(Year == input$year2) %>%
      group_by(`Location Subdivision`) %>%
      summarise(`Total Incidents` = sum(`Incidents Recorded`)) %>%
      arrange(desc(`Total Incidents`)) %>%
      head(5)
  })

  # Find the top 5 location types with the highest number of incidents for the previous year
  top_location_types_previous <- reactive({
    selected_data_location() %>%
      filter(Year == as.numeric(input$year2) - 1) %>%
      group_by(`Location Subdivision`) %>%
      summarise(`Total Incidents` = sum(`Incidents Recorded`)) %>%
      arrange(desc(`Total Incidents`)) %>%
      head(5)
  })

  # Create a radar chart using plotly
  output$radar_chart_location_type <- renderPlotly({
    current_data_location <- top_location_types_current()
    previous_data_location <- top_location_types_previous()

    # Radar chart function
    plot_ly(current_data_location, type = "scatterpolar", mode = "lines+markers") %>%
      add_trace(
        r = current_data_location$`Total Incidents`,
        theta = current_data_location$`Location Subdivision`,
        name = "2023",
        fill = "toself",
        line = list(shape = "linear", color = "red")
      ) %>%
      add_trace(
        r = previous_data_location$`Total Incidents`,
        theta = previous_data_location$`Location Subdivision`,
        name = "2022",
        fill = "toself",
        line = list(shape = "linear", color = "blue")
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE),
          angularaxis = list(categoryarray = current_data_location$`Location Subdivision`)
        ),
        showlegend = TRUE,
        annotations = list(
          list(
            x = 0.5,  # X position for centering the title
            y = -0.1,  # Y position for placing the title at the bottom
            xref = "paper",  # x reference is the paper (entire chart)
            yref = "paper",  # y reference is also the paper
            text = "Top 5 Location Types",
            showarrow = FALSE,
            font = list(size = 14)
          )
        )
      )

  })


  ## Top 5 Suburb
  selected_data_suburb <- reactive({
    filter(crime_lga_offence_bypostcode, `Local Government Area` == input$LGA2)

  })

  # Find the top 5 suburbs with the highest number of incidents for the current year
  top_suburb_current <- reactive({
    selected_data_suburb() %>%
      filter(Year == input$year2) %>%
      group_by(`Suburb/Town Name`) %>%
      summarise(`Total Incidents` = sum(`Incidents Recorded`)) %>%
      arrange(desc(`Total Incidents`)) %>%
      head(5)
  })

  # Find the top 5 suburbs with the highest number of incidents for the previous year
  top_suburb_previous <- reactive({
    selected_data_suburb() %>%
      filter(Year == as.numeric(input$year2) - 1) %>%
      group_by(`Suburb/Town Name`) %>%
      summarise(`Total Incidents` = sum(`Incidents Recorded`)) %>%
      arrange(desc(`Total Incidents`)) %>%
      head(5)
  })

  # Create a radar chart using plotly
  output$radar_chart_suburbs <- renderPlotly({
    current_data_suburb <- top_suburb_current()
    previous_data_suburb <- top_suburb_previous()

    # Radar chart function
    plot_ly(current_data_suburb, type = "scatterpolar", mode = "lines+markers") %>%
      add_trace(
        r = current_data_suburb$`Total Incidents`,
        theta = current_data_suburb$`Suburb/Town Name`,
        name = "2023",
        fill = "toself",
        line = list(shape = "linear", color = "red")
      ) %>%
      add_trace(
        r = previous_data_suburb$`Total Incidents`,
        theta = previous_data_suburb$`Suburb/Town Name`,
        name = "2022",
        fill = "toself",
        line = list(shape = "linear", color = "blue")
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE),
          angularaxis = list(categoryarray = current_data_suburb$`Suburb/Town Name`)
        ),
        showlegend = TRUE,
        annotations = list(
          list(
            x = 0.5,  # X position for centering the title
            y = -0.1,  # Y position for placing the title at the bottom
            xref = "paper",  # x reference is the paper (entire chart)
            yref = "paper",  # y reference is also the paper
            text = "Top 5 Suburbs",
            showarrow = FALSE,
            font = list(size = 14)
          )
        )
      )

  })

  ## Top 5 Principal Offences
  selected_data_po <- reactive({
    filter(crime_lga_offence, `Local Government Area` == input$LGA2)

  })

  # Find the top 5 principal offences with the highest number of incidents for the current year
  top_po_current <- reactive({
    selected_data_po() %>%
      filter(Year == input$year2) %>%
      group_by(`Offence Subgroup`) %>%
      summarise(`Total Incidents` = sum(`Incidents Recorded`)) %>%
      arrange(desc(`Total Incidents`)) %>%
      head(5)
  })

  # Find the top 5 principal offences with the highest number of incidents for the previous year
  top_po_previous <- reactive({
    selected_data_po() %>%
      filter(Year == as.numeric(input$year2) - 1) %>%
      group_by(`Offence Subgroup`) %>%
      summarise(`Total Incidents` = sum(`Incidents Recorded`)) %>%
      arrange(desc(`Total Incidents`)) %>%
      head(5)
  })

  # Create a radar chart using plotly
  output$radar_chart_principal_offence <- renderPlotly({
    current_data_po <- top_po_current()
    previous_data_po <- top_po_previous()

    # Radar chart function
    plot_ly(current_data_po, type = "scatterpolar", mode = "lines+markers") %>%
      add_trace(
        r = current_data_po$`Total Incidents`,
        theta = current_data_po$`Offence Subgroup`,
        name = "2023",
        fill = "toself",
        line = list(shape = "linear", color = "red")
      ) %>%
      add_trace(
        r = previous_data_po$`Total Incidents`,
        theta = previous_data_po$`Offence Subgroup`,
        name = "2022",
        fill = "toself",
        line = list(shape = "linear", color = "blue")
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE),
          angularaxis = list(categoryarray = current_data_po$`Offence Subgroup`)
        ),
        showlegend = TRUE,
        annotations = list(
          list(
            x = 0.5,  # X position for centering the title
            y = -0.1,  # Y position for placing the title at the bottom
            xref = "paper",  # x reference is the paper (entire chart)
            yref = "paper",  # y reference is also the paper
            text = "Top 5 Principal Offenses",
            showarrow = FALSE,
            font = list(size = 14)
          )
        )
      )

  })



  # Tab 3 Map

  output$user3 <- renderText({
    HTML(' <li> This choropleth map plot provides comparison of <b>criminal rate per 100,000 population</b> between LGAs for the same year.  </li>
            <li> The darker the colour, the higher the criminal rate for that region. </li>
           <li> User can switch between different <i> Police Region</i>, <i>Local Government Area </i> and <i> Year </i> using the selective box. </li>
           <li> It is interesting that for almost every year, 3 most dangerous areas, in descending order, are <b> Melbourne </b>, <b> Yarra </b> and <b> Latrobe </b>. </li>
           <li> Please hover on each polygons on the map for further information. </li> ')
  })

  filtered_data <- reactive({
    # Filter your data based on inputs
    data <- sf_crime_lga_tot_incidents

    data <- st_transform(st_as_sf(data),4326)

    # Apply filters if "All" option is not selected
    if (input$PR != "All") {
      data <- data %>%
        filter(`Police Region` == input$PR)
    }

    # observe({
    #   if (input$PR != "All") {
    #     # Filter the data based on the selected Police Region
    #     data <- data %>%
    #       filter(`Police Region` == input$PR)
    #
    #     lga_choices <- unique(data$`Local Government Area`)
    #   } else {
    #     lga_choices <- unique(data$`Local Government Area`)
    #   }
    #
    #   # Update the choices for the "LGA3" selectize input
    #   updateSelectizeInput(session, "LGA3", choices = lga_choices)
    # })

    if (input$LGA3 != "All") {
      data <- data %>%
        filter(`Local Government Area` == input$LGA3)
    }

    data <- data %>%
      filter(Year == input$year3)

    data
  })


  output$map <- renderLeaflet({
    # Create a leaflet map
    m <- leaflet() %>%
      addProviderTiles("CartoDB.Positron")

    ratepal <- colorFactor("Greens", filtered_data()$`Rate per 100,000 population`)

    # Add polygons for LGA
    m <- m %>% addPolygons(
      data = filtered_data(),
      fillColor = ~ratepal(`Rate per 100,000 population`),
      color = "white",
      weight = 1,
      opacity = 1,
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "red",
        fillOpacity = 0.7
      ),
      label = ~paste("LGA:", `Local Government Area`, ",",
                     "<br>Year:", Year, ",",
                     "<br>Rate per 100,000 population:", round(`Rate per 100,000 population`, 2)),
      group = "Local Government Area"
    )

    # # Add a legend
    # m <- m %>% addLegend(
    #   position = "bottomright",
    #   pal = ratepal,
    #   values = range(filtered_data()$`Rate per 100,000 population`),
    #   labels = "Incident Rate",
    #   labFormat = labelFormat(suffix = NULL)
    # )

    m
  })

  # Tab 4 Socio-Economic Factor

  output$user4 <- renderText({
    HTML(' <li> The parallel coordinate plot shows how <i>crime rate per 100,000 population</i> interacts with different <i>socio-economic factors</i>. </li> <br>
            <li> <b>Red lines</b> represent LGAs with crime rate above average while <b> blue lines </b> accounted for lower than average. </li> <br>
            <li> <b>Three yellow lines </b> represents <i>Melbourne</i>, <i>Yarra</i> and <i>Latrobe</i> (3 LGAs with highest criminal rate). </li> <br>
           <li> In general, living standard satisfaction index <b>does not</b> indicate any relationship with crime rate. </li> <br>
           <li> Moreover, 2 other rankings vary with the crime rate. </li> <br>
           <li> Note that, this poor analysis result is due to data limitations (<b>explained more in the report</b>). </li> ')
  })

  filter_dat <- reactive({
    mean <- crime_lga_tot_incidents %>%
      summarise(mean = mean(`Rate per 100,000 population`))

    data <- crime_lga_tot_incidents %>%
      mutate(higher_than_mean = ifelse(`Local Government Area` %in% c("Melbourne", "Yarra", "Latrobe"), 0.5, ifelse(`Rate per 100,000 population` > mean$mean, 1, 0))) %>%
      mutate(higher_than_mean = as.factor(higher_than_mean)) %>%
      left_join(socio_eco,
                by = "Local Government Area")

    data

  })

  output$socio_eco <- renderPlotly({
    # Set our colours for the par coords, and also tour
    clrs <- divergingx_hcl(palette="Zissou 1", n=3)

    plot <- filter_dat() %>%
      plot_ly(type = 'parcoords',
              line = list(color = ~higher_than_mean,
                          colorscale = list(c('0',clrs[1]), c('0.5',clrs[2]), c('1',clrs[3])),
                          text = ~`Local Government Area`),
              dimensions = list(
                list(range = c(0,1),
                     label = 'Above mean rate per 100,000 population', values = ~higher_than_mean),
                list(range = c(0,100),
                     label = 'Living Standard Satisfaction Index', values = ~`Living Standard Satisfaction Index`),
                list(range = c(0,78),
                     label = 'Socio-economic Disadvantage Rank', values = ~Rank_SED),
                list(range = c(0,78),
                     label = 'Education and Occupation Disadvantage Rank', values = ~Rank_EandO)
              )
      )

    plot
  })

  output$main_img <- renderImage({

    list(src = "image1.jpeg",
         width = "100%",
         height = 330)

  }, deleteFile = F)



}



# Run the application
shinyApp(ui = ui, server = server)
