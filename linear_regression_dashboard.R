library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(shinycssloaders)
library(reshape2)
library(plotly)
library(leaps)
library(broom)
library(ggfortify)
library(shinya11y)

ui <- dashboardPage(
  
  skin ="purple",
  
  dashboardHeader(
    title = span(img(src = "shiny.png", height = 40, alt = "Rshiny logo"))
    ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home",  icon = icon("house")),
      menuItem("User Guide", tabName = "guide",  icon = icon("bookmark")),
      menuItem("Exploratory Data Analysis", tabName = "eda", icon = icon("images")),
      menuItem("Feature Selection", tabName = "feat", icon = icon("diagram-project")),
      menuItem("Model Building", tabName = "build", icon = icon("calculator")),
      menuItem("Model Prediction", tabName = "pred", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    use_tota11y(),
    tags$body(
      tags$style(
        HTML(
          ".content-wrapper {
              background-color: black;
            }
            .skin-purple .main-sidebar {
              background-color: black;
            }
            .irs--shiny .irs-grid-text {
            bottom: 5px;
            color:black
            }
            .irs--shiny .irs-min, .irs--shiny .irs-max {
            top: 0;
            padding: 1px 3px;
            text-shadow: none;
            background-color: (108, 122, 137, 1);
            border-radius: 3px;
            font-size: 10px;
            line-height: 1.333;
            color: black;
            }
            table {
            font-size: 14px;  
            color: black;  
            }
            table th {
            font-size: 16px;  
            color: black;
            } 
            .shiny-output-error-validation {
            font-size: 16px;  
            color: red; 
            font-weight: bold;
            }
            h2 {
            text-align: left;
            color: white; 
            font-size: 34px;
            font-weight: bold;
          }"
        )
      )
    ),
    tabItems(
      tabItem(
        tabName = "home",
        tags$h1(
          style = "font-weight: bold; color: white;", "An Interactive Linear Regression Analysis Dashboard on Energy Consumption"
        ),
        br(),
        br(),
        box(
          title = tags$span(style = "font-weight: bold; font-size: 20px; color: #000000;", 
                            "Descriptive Statistics on Regression Modelling Data"),
          style = "background-color: white; border: 2px solid  white; font-weight: bold;", 
          verbatimTextOutput(outputId = "descriptive.stats"),
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = FALSE,
          width = 9
        ),
        column(
          htmlOutput("intro"),
          width = 9
        ),
      ),
      tabItem(
        tabName = "guide",
        fluidRow(
          column(
            tags$h1(style = "font-weight: bold; color: white;", "User Guide"),
            htmlOutput(outputId = "guide"),
            background = "black",
            style = "font-size: 18px; color: black; background-color: #000000",
            width = 6
          ),
          box(
            tags$img(
              src = "energy_consumption_img.jpg", 
              alt = "Image of solar panels and moving light on a rooftop at sunset", 
              height = 750, 
              width = 690,
              style = "display: block; 
              margin-left: auto; 
              margin-right: auto;
              margin-bottom: auto;"),
            style = "background-color: black; border: 2px solid #ffffff;",
            width = 6
          )
        )
      ),
      tabItem(
        tabName = "eda",
        fluidRow(
          box(
            tags$h1(
              span(style = "font-weight: bold;", "Data Visualisations")
              ),
            background = "purple",
            selectInput(
              inputId = "viz",
              label = tags$div(style = "color: #ffff00; font-size: 19px; font-weight: bold;","Select plot to explore"), 
              choices=list(
                "Boxplot of Energy Consumtpion, Day of Week and Holiday" = "box.plot1",
                "Boxplot of Energy Consumtpion, Occupancy Day of Week" = "box.plot2",
                "Bubble Chart of Energy Consumption, Property Square Footage, Occupancy and Lighting Usage" = "bubble.chart2",
                "Bubble Chart of Energy Consumption, Temperature, Humidity and HVAC usage" = "bubble.chart1",
                "Heatmap of Average Energy Consumption, Time of Day and Month" = "heatmap.plot",
                "Histogram of Energy Consumption" = "dist.plot"
              )
            ),
            width = 6
          ),
          column(
            width = 3,
            infoBox(title = "Categorical Features",
                    value = tags$div(style = "font-size: 35px; color: white;", "4"),
                    icon = icon("list-ol"),
                    color = "purple",
                    width = NULL,
                    fill = TRUE)
          ),
          column(
            width = 3,
            infoBox(title = "Numeric Features",
                    value = tags$div(style = "font-size: 35px; color: white;", "6"),
                    icon = icon("hashtag"),
                    color = "purple",
                    width = NULL,
                    fill = TRUE)
          ),
          column(
            width = 3,
            infoBox(title = "Temporal Features",
                    value = tags$div(style = "font-size: 35px; color: white;", "2"),
                    icon = icon("clock"),
                    color = "purple",
                    width = NULL,
                    fill = TRUE)
          ),
          column(
            width = 3,
            infoBox(title = "Total Observations",
                    value = tags$div(style = "font-size: 35px; color: white;", "5000"),
                    icon = icon("table"),
                    color = "purple",
                    width = NULL,
                    fill = TRUE)
          )
        ),
        fluidRow(
          box(
            style = "background-color: #000000;",
            background = "black",
            withSpinner(
              uiOutput("plot.output", style = "height: 250px; width: 100%;"),
              type = 7,
              color = "#b4a7d6",
              size = 1.5,
              proxy.height = "200px"
            ),
            width = 12
          )
        )
      ),
      tabItem(
        tabName = "feat",
        fluidRow(
          box(
            tags$h1(
              tags$strong("Regression Model Performance", style = "color: white;")
            ),
            wellPanel(
              selectInput(inputId = "size", 
                          label = tags$span(style = "color: #000000; font-weight: bold; font-size: 18px;", 
                                            "Model size comparisons for predicting energy consumption"), 
                          multiple = TRUE, 
                          choices = 1:8,
                          selected = 4:6),
              actionButton(inputId = "go", label = "Go")
            ),
            
            withSpinner(plotlyOutput("scatterplot.size"),
                        type = 7,
                        color = "black"
            ),
            style = "background-color: #000000; border: 2px solid #ffffff;",
            width = 6
          ),
          column(
            width = 6,
            valueBox(value = tags$div(style = "font-size: 25px; color: black", "Adjusted R-Squared"),
                     subtitle = tags$div(style = "color: black; font-size: 16px;", "Measures model accuracy by the percentege of variance explained by the independent variables"),
                     icon = icon("info-circle"),
                     color = "red",
                     width = NULL)
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Day of the Week"),
              subtitle = "Monday-Sunday",
              icon = icon("calendar"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Energy Consumption"),
              subtitle = "Total energy consumed (kWh)",
              icon = icon("bolt"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Holiday"),
              subtitle = "Yes or No",
              icon = icon("umbrella-beach"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Hour"),
              subtitle = "Hourly energy readings, 0-23",
              icon = icon("clock"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Humidity"),
              subtitle = "Percentage level",
              icon = icon("sun"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "HVAC Usage"),
              subtitle = "On or Off",
              icon = icon("fan"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Lighting Usage"),
              subtitle = "On or Off",
              icon = icon("lightbulb"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Month"),
              subtitle = "Numbered months of the year",
              icon = icon("calendar"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Occupancy"),
              subtitle = "Number of occupants in the space, 0-9",
              icon = icon("building"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Renewable Energy"),
              subtitle = "Percentage of renewable energy sources",
              icon = icon("solar-panel"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Square Footage"),
              subtitle = "Measurement of the space in square feet",
              icon = icon("building"),
              color = "purple",
              width = NULL
            )
          ),
          column(
            width = 3,
            valueBox(
              value = tags$div(style = "font-size: 30px; color: white;", "Temperature"),
              subtitle = "Degree celsius",
              icon = icon("temperature-half"),
              color = "purple",
              width = NULL
            )
          ),
          box(
            style = "background-color: white; border: 2px solid  white; font-weight: bold;", 
            verbatimTextOutput(outputId = "best.predictors"),
            title = tags$p("Model size with the highest adjusted r-squared percentege", style = "color: #000000; font-size: 19px; font-weight: bold;"),
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 6
          )
        )
      ),
      tabItem(
        tabName = "build",
        fluidRow(
          box(
            background = "black",
            style = "background-color: #000000;",
            radioButtons(
              inputId = "dep.var",
              label = tags$span(style = "color: #E7B200; font-weight: bold; font-size: 17px;", "Dependent variable"),
              choices = list(
                "Energy Consumption" = "EnergyConsumption"
              ),
              selected = "EnergyConsumption"
            ),
            width = 3
          ),
          box(
            style = "background-color: #000000;",
            background = "black",
            checkboxGroupInput(inputId = "predictors",
                              label = tags$span(style = "color: #E7B200; font-weight: bold; font-size: 18px;", "Independent variables"),
                              choices = list(
                                "Day of the Week" = "DayOfWeek",
                                "Holiday" = "Holiday",
                                "Hour" = "Hour",
                                "Humidity" = "Humidity",
                                "HVAC Usage" = "HVACUsage",
                                "Lighting Usage" = "LightingUsage",
                                "Month" = "Month",
                                "Occupancy" = "Occupancy",
                                "Renewable Energy" = "RenewableEnergy",
                                "Square Footage" = "SquareFootage",  
                                "Temperature" = "Temperature"
                              ),
                              selected = "Temperature"),
            width = 3
          ),
          box(
            style = "background-color: #000000;",
            background = "black",
            title = tags$span(style = "font-weight: bold; font-size: 20px; color: #000000;", "Coefficient Estimates"),
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 6,
            withSpinner(
              verbatimTextOutput(outputId = "reg.stats"),
              type = 7,
              color = "#b4a7d6",
              size = 1.5,
              proxy.height = "200px"
            )
          )
        ),
        fluidRow(
          column(
            background = "black",
            tags$h1(style = "font-weight: bold;", "Building a Regression Model"),
            htmlOutput(outputId = "assumptions"),
            style = "font-size: 16px; color: white; background-color: #000000;",
            width = 6
          ),
          box(
            style = "background-color: #000000;",
            background = "black",
            title = tags$span(style = "font-weight: bold; font-size: 20px; color: #000000;", "Diagnostic Plots"),
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 6,
            withSpinner(
              plotOutput(outputId = "diag.plots"),
              type = 7,
              color = "#b4a7d6",
              size = 1.5,
              proxy.height = "200px"
            )
          )
        ), 
        fluidRow(
          box(
            style = "background-color: #000000;",
            title = tags$span(style = "font-weight: bold; font-size: 20px; color: #000000;", "Adjusted R-Squared, Coefficient and Confidence Interval Interpretation"),
            withSpinner(
              uiOutput("results"),
              type = 7,
              color = "#b4a7d6",
              size = 1.5,
              proxy.height = "200px"
            ),
            status = "warning",
            background = "black",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 8
          ),
          box(
            style = "background-color: #000000;",
            background = "black",
            title = tags$span(style = "font-weight: bold; font-size: 20px; color: #000000;", "Confidence Interval"),
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            width = 4,
            withSpinner(
              verbatimTextOutput(outputId = "ci.table"),
              type = 7,
              color = "#b4a7d6",
              size = 1.5,
              proxy.height = "200px"
            )
          )
        )
      ),
      tabItem(
        tabName = "pred",
        fluidRow(
          column(
            tags$h1(style = "font-weight: bold; color: white;", "Energy Consumption Prediction"),
            htmlOutput(outputId = "pred.text"),
            background = "black",
            style = "background-color: black;",
            width = 12
          ),
          fluidRow(
            box(
              background = "black",
              style = "font-size: 16px; color: black; background-color: black", 
              wellPanel(
                radioButtons(
                  inputId = "dep.var2",
                  label = tags$span(style = "color: black; font-weight: bold; font-size: 17px;", "Dependent variable"),
                  choices = list("Energy Consumption" = "EnergyConsumption"),
                  selected = "EnergyConsumption"
                ),
                radioButtons(inputId = "Holiday",
                             label = tags$span(style = "color: black; font-weight: bold; font-size: 17px;", "Holiday"), 
                             choices = list(
                                     "Yes" = "Yes"),
                             inline = TRUE),
                radioButtons(inputId = "HVACUsage", 
                             label = tags$span(style = "color: black; font-weight: bold; font-size: 17px;", "HVAC Usage"),
                             choices = list("On" = "On"),
                             inline = TRUE),
                numericInput(inputId = "Occupancy", label = tags$span(style = "color: black; font-weight: bold; font-size: 17px;", "Occupancy"), value = 5),
                numericInput(inputId = "Temperature", label = tags$span(style = "color: black; font-weight: bold; font-size: 17px;", "Temperature"), value = 24),
                numericInput(inputId = "RenewableEnergy", label = tags$span(style = "color: black; font-weight: bold; font-size: 17px;", "Renewable Energy"), value = 15),
                numericInput(inputId = "Humidity", label = tags$span(style = "color: black; font-weight: bold; font-size: 17px;", "Humidity"), value = 45),
                actionButton(inputId = "model", label = "Model")
              ),
              width = 3
            ),
            box(
              style = "background-color: #000000;",
              background = "black",
              title = tags$span(style = "font-weight: bold; font-size: 20px; color: #000000;", "Prediciton"),
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              width = 4,
              withSpinner(
                uiOutput(outputId = "pred"),
                type = 7,
                color = "#b4a7d6",
                size = 1.5,
                proxy.height = "200px"
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session){
  
  data <- read.csv("Energy_consumption_dataset.csv")
  
  day.of.week.factor <- factor(data$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  
  occupancy.factor <- factor(data$Occupancy, levels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
  
  predictors <- data |>
    select(Temperature, Humidity, SquareFootage, Month, Hour, Occupancy, RenewableEnergy)
  
  leap.mod <-leaps(predictors, 
                   data$EnergyConsumption,
                   method="adjr2", 
                   nbest = 6, 
                   names = names(predictors))
  
  result.tab <- data.frame(adjr2 = leap.mod$adjr2,
                           size = leap.mod$size,
                           leap.mod$which,
                           row.names=NULL)
  
  best.model <- lm(EnergyConsumption ~ Temperature + Occupancy + RenewableEnergy + 
                     HVACUsage + Humidity + Holiday, data = data)
  
  best.formula <- summary(best.model)
  
  
  output$plot.output <- renderUI({
    if(input$viz  == "box.plot1") {
      plotlyOutput("boxplot1.output")
    } else if(input$viz  == "box.plot2") {
      plotlyOutput("boxplot2.output") 
    } else if(input$viz  == "bubble.chart1") {
      plotlyOutput("bubblechart1.output")  
    } else if(input$viz  == "bubble.chart2") {
      plotlyOutput("bubblechart2.output") 
    } else if(input$viz  == "dist.plot") {
      plotlyOutput("dist.plot.output") 
    } else if(input$viz  == "heatmap.plot") {
      plotlyOutput("heatmap.output")
    }
  })
  
  output$boxplot1.output <- renderPlotly({
    Sys.sleep(3)
    
    boxplot1 <- ggplot(data) +
      geom_boxplot(aes(x = day.of.week.factor, y = EnergyConsumption, colour = factor(Holiday), fill = factor(Holiday))) +
      theme_minimal() +
      labs(
        x = "Day of Week",
        y = "Energy Consumption",
        title = "Distribution of Energy Consumption by Day of Week and Holiday",
        color  = "Bank Holiday",
        fill = "Bank Holiday"
      ) + 
      scale_fill_manual(values = c("#6666e0", "#e06666")) +
      scale_color_manual(values = c("#6666e0", "#e06666")) +
      theme(
        plot.title = element_text(family = "sans", face = "bold", hjust = 0.5, size = 19, colour = "white"),
        axis.title.x = element_text(family = "sans", face = "bold", size = 16, colour = "white"),
        axis.title.y = element_text(family = "sans", face = "bold", size = 16, colour = "white"),
        axis.text = element_text(family = "sans", hjust = 0.5, size = 13, face = "bold", colour = "white"),
        legend.position = "top",
        legend.title = element_text(family = "sans", face = "bold", size = 14),
        legend.text = element_text(colour = "white", size = 12),
        plot.background = element_rect(fill = "black", color = "black", colour = "white"),  
        panel.background = element_rect(fill = "black", colour = "white"),  
        panel.grid.major = element_line(color = "gray", colour = "white"),  
        panel.grid.minor = element_line(color = "gray", colour = "white")
      )
    
    
    ggplotly(boxplot1, dynamicTicks = TRUE) |> 
      layout(
        hovermode = "x",
        showlegend = TRUE,
        legend = list(
          bgcolor = "black",
          title=list(font = list(color = "white",
                                 face = "bold",
                                 family = "Open Sans",
                                 weight = 5)))) 
    
  })
  
  output$boxplot2.output <- renderPlotly({
    Sys.sleep(3)
    
    boxplot2 <- ggplot(data) +
      geom_boxplot(aes(x = occupancy.factor, y = EnergyConsumption, fill = day.of.week.factor, colour = day.of.week.factor)) +
      theme_minimal() +
      labs(
        x = "Occupancy",
        y = "Energy Consumption",
        title = "Distribution of Energy Consumption by Occupancy and Day of Weeek",
        fill  = "Day of Week",
        color = "Day of Week"
      ) + 
      scale_fill_manual(values = RColorBrewer::brewer.pal(10, "Set1")) +
      scale_color_manual(values = RColorBrewer::brewer.pal(10, "Set1")) +
      theme(
        plot.title = element_text(family = "sans", face = "bold", hjust = 0.5, size = 19, colour = "white"),
        axis.title.x = element_text(family = "sans", face = "bold", size = 16, colour = "white"),
        axis.title.y = element_text(family = "sans", face = "bold", size = 16, colour = "white"),
        axis.text = element_text(family = "sans", hjust = 0.5, size = 14, face = "bold", colour = "white"),
        legend.position = "top",
        legend.title = element_text(family = "sans", face = "bold", size = 14, colour = "white"),
        legend.text = element_text(colour = "white", size = 12),
        plot.background = element_rect(fill = "black", color = "black", colour = "white"),  
        panel.background = element_rect(fill = "black", colour = "white"),  
        panel.grid.major = element_line(color = "gray", colour = "white"),  
        panel.grid.minor = element_line(color = "gray", colour = "white")
      )
    
    
    ggplotly(boxplot2, dynamicTicks = TRUE) |> 
      layout(
        hovermode = "x",
        showlegend = TRUE,
        legend = list(
          bgcolor = "black",
          title=list(font = list(color = "white",
                                 face = "bold",
                                 family = "Open Sans",
                                 weight = 5)))) 
    
  })
  
  output$bubblechart1.output <- renderPlotly({
    Sys.sleep(3)
    
    bubblechart <- data |>
      select(Temperature, EnergyConsumption, Humidity, HVACUsage) |>
      rename(`Energy Consumption` = EnergyConsumption,
             `HVAC Usage` = HVACUsage) |>
      mutate(
        `Energy Consumption` = round(`Energy Consumption`, 2),
        Temperature = round(Temperature, 2),
        Humidity = round(Humidity, 2)
      ) |>
      ggplot(aes(x = Temperature, 
                 y = `Energy Consumption`, 
                 size = Humidity, 
                 color = `HVAC Usage`)) +
      geom_point(alpha = 0.6, stroke = 1.5, shape = 19, show.legend = TRUE) + 
      labs(
        x = "Temperature",
        y = "Energy Consumption",
        title = "Energy Consumption by Temperature, Humidity and Heating, Ventilation & Air Conditioning Systems"
      ) +
      scale_color_manual(values = c("#b300b3", "#e0e066")) +
      scale_x_continuous(
        breaks = seq(20, 30, by = 1)
      ) +
      scale_size(range = c(1, 24), name="Humidity") +
      theme(
        plot.title = element_text(family = "sans", face = "bold", hjust = 0.5, size = 19, colour = "white"),
        axis.title.x = element_text(family = "sans", face = "bold", size = 16, colour = "white"),
        axis.title.y = element_text(family = "sans", face = "bold", size = 16, colour = "white"),
        axis.text = element_text(family = "sans", hjust = 0.5, size = 14, face = "bold", colour = "white"),
        legend.position = "top",
        legend.title = element_text(family = "sans", face = "bold", size = 14, colour = "white"),
        legend.text = element_text(colour = "white", size = 12),
        plot.background = element_rect(fill = "black", color = "black", colour = "white"),  
        panel.background = element_rect(fill = "black", colour = "white"),  
        panel.grid.major = element_line(color = "gray", colour = "white"),  
        panel.grid.minor = element_line(color = "gray", colour = "white")
      ) 
    
    
    ggplotly(bubblechart, dynamicTicks = TRUE) |> 
      rangeslider() |>
      layout(
        hovermode = "x",
        hoverlabel = list(
          font = list(
            size = 17,
            weight = 20
          )
        ),
        showlegend = TRUE,
        legend = list(
          bgcolor = "black",
          title=list(font = list(color = "white",
                                 family = "Open Sans")))
      )
    
  })
  
  output$bubblechart2.output <- renderPlotly({
    Sys.sleep(3)
    
    bubblechart2 <- data |>
      select(RenewableEnergy, EnergyConsumption, SquareFootage, LightingUsage) |>
      rename(`Energy Consumption` = EnergyConsumption,
             `Square Footage` = SquareFootage,
             `Lighting Usage` = LightingUsage,
             `Renewable Energy` = RenewableEnergy) |>
      mutate(
        `Energy Consumption` = round(`Energy Consumption`, 2),
        `Renewable Energy` = round(`Renewable Energy`, 2)
      ) |>
      ggplot(aes(x = `Square Footage`, 
                 y = `Energy Consumption`, 
                 size = `Renewable Energy`, 
                 color = `Lighting Usage`)) +
      geom_point(alpha = 0.6, stroke = 1.5, shape = 19, show.legend = TRUE) + 
      labs(
        x = "Square Footage",
        y = "Energy Consumption",
        title = "Energy Consumption by Square Footage, Renewable Energy and Lighting Usage"
      ) +
      scale_color_manual(values = c("#b300b3", "#b3b300")) +
      scale_size(range = c(1, 24), name="Renewable Energy") +
      theme(
        plot.title = element_text(family = "Sans", face = "bold", hjust = 0.5, size = 19, colour = "white"),
        axis.title.x = element_text(family = "Sans", face = "bold", size = 16, colour = "white"),
        axis.title.y = element_text(family = "Sans", face = "bold", size = 16, colour = "white"),
        axis.text = element_text(family = "Sans", hjust = 0.5, size = 14, face = "bold", colour = "white"),
        legend.position = "top",
        legend.title = element_text(family = "sans", face = "bold", size = 14, colour = "white"),
        legend.text = element_text(colour = "white", size = 12),
        plot.background = element_rect(fill = "black", color = "black", colour = "white"),  
        panel.background = element_rect(fill = "black", colour = "white"),  
        panel.grid.major = element_line(color = "gray", colour = "white"),  
        panel.grid.minor = element_line(color = "gray", colour = "white")
      ) 
    
    ggplotly(bubblechart2, dynamicTicks = TRUE) |> 
      rangeslider() |>
      layout(
        hovermode = "x",
        hoverlabel = list(
          font = list(
            size = 17,
            weight = 20
          )
        ),
        showlegend = TRUE,
        legend = list(
          bgcolor = "black",
          title = list(font = list(color = "white",
                                   family = "Open Sans",
                                   weight = 5)))
      )
    
  })
  
  output$dist.plot.output <- renderPlotly({
    Sys.sleep(3)
    
    histogram.plot <- data |> 
      select(EnergyConsumption) |>
      mutate(EnergyConsumption = round(EnergyConsumption, 2)) |>
      rename(`Energy Consumption` = EnergyConsumption) |>
      ggplot(aes(x = `Energy Consumption`)) +
      geom_histogram(binwidth = 5, fill = "#CC5500", color = "#5500cc", 
                     alpha = 1, linetype = "solid", linewidth = 1) + 
      geom_density(aes(y = ..density..), color = "#CC5500", size = 1) + 
      labs(
        x = "Energy Consumption",
        y = "Count",
        title = "Distribution of Energy Consumption"
      ) +
      theme_minimal(base_family = "sans") +
      theme(
        plot.title = element_text(family = "sans", face = "bold", hjust = 0.5, size = 19, colour = "white"),
        axis.title.x = element_text(family = "sans", face = "bold", size = 16, colour = "white"),
        axis.title.y = element_text(family = "sans", face = "bold", size = 16, colour = "white"),
        axis.text = element_text(family = "sans", hjust = 0.5, size = 14, face = "bold", colour = "white"),
        legend.position = "top",
        plot.background = element_rect(fill = "black", color = "black", colour = "white"),  
        panel.background = element_rect(fill = "black", colour = "white"),  
        panel.grid.major = element_line(color = "gray", colour = "white"),  
        panel.grid.minor = element_line(color = "gray", colour = "white")
      )
    
    
    ggplotly(histogram.plot) |> 
      layout(
        hovermode = "x",
        hoverlabel = list(
          bgcolor = "#CC5500",
          font = list(
            size = 17,
            weight = 20))
      )
    
  })
  
  output$heatmap.output <- renderPlotly({
    Sys.sleep(3)
    
    energy.hour.month <- data |>
      select(Hour, Month, EnergyConsumption) |>
      group_by(Hour, Month) |>
      summarise(`Mean Energy Consumption` = mean(EnergyConsumption)) |>
      mutate(
        `Mean Energy Consumption` = round(`Mean Energy Consumption`, 2)
      ) 
    
    energy.hour.month.pivot <- energy.hour.month |>
      pivot_wider(names_from = Month, 
                  values_from = `Mean Energy Consumption`) |>
      column_to_rownames("Hour") 
    
    energy.melt <- melt(as.matrix(energy.hour.month.pivot))
    
    
    
    colnames(energy.melt) <- c("Hour", "Month", "Mean Energy Consumption")
    
    heatmap.chart <- ggplot(energy.melt, aes(x = Hour, 
                                             y = Month,
                                             fill = `Mean Energy Consumption`)) +
      geom_tile(show.legend = FALSE) +
      labs(
        x = "Hour", 
        y = "Month",
        title = "Average Energy Consumption by Hour and Month",
        fill = "Mean Energy Consumption"
      ) + 
      scale_fill_gradient(high = "#FFFF00", low = "#0028ff") +
      theme(
        plot.title = element_text(family = "sans", face = "bold", hjust = 0.5, size = 19, colour = "white"),
        axis.title.x = element_text(family = "sans", face = "bold", size = 16, colour = "white"),
        axis.title.y = element_text(family = "sans", face = "bold", size = 16, colour = "white"),
        axis.text.x = element_text(family = "sans", hjust = 0.5, size = 14, colour = "white"),
        axis.text.y = element_text(family = "sans", hjust = 0.5, size = 14, colour = "white"),
        panel.grid = element_blank(),
        legend.title = element_text(family = "sans", face = "bold", size = 10, colour = "white"),
        legend.text = element_text(colour = "white"),
        plot.background = element_rect(fill = "black", color = "black", colour = "white"),  
        panel.background = element_rect(fill = "black", colour = "white")
      )
    
    ggplotly(heatmap.chart, dynamicTicks = TRUE) |> 
      layout(
        hovermode = "x",
        hoverlabel = list(
          bgcolor = "#FFFF00",
          font = list(
            size = 17,
            weight = 20
          )
        ),
        showlegend = FALSE)
  })
  
  size.stats <- reactive({
    
    validate(
      need(!1 %in% input$size, 
           "A simple linear regression won't optimally predict energy consumption. Please exclude '1' from the selection.")
    )
    
    result.tab |> 
      filter(size %in% input$size) |>
      select(size, adjr2)
    
  }) |>
    bindEvent(input$go, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$scatterplot.size <- renderPlotly({
    
    scatter.plot <- ggplot(size.stats()) + 
      geom_point(aes(x = size, y = adjr2), 
                 colour = "white", fill = "#FF00FF", shape = 21, size = 6) +
      labs(
        x = "Size",
        y = "Adjusted R-Sqaure",
        title = "Regression Model Performance by Size"
      ) +
      theme(
        plot.title = element_text(family = "sans", face = "bold", hjust = 0.5, size = 18, colour = "white"),
        axis.title.x = element_text(family = "sans", face = "bold", size = 14, colour = "white"),
        axis.title.y = element_text(family = "sans", face = "bold", size = 14, colour = "white"),
        axis.text = element_text(family = "sans", hjust = 0.5, size = 12, face = "bold", colour = "#ffff00"),
        legend.position = "top",
        plot.background = element_rect(fill = "black", color = "gray", colour = "white"),  
        panel.background = element_rect(fill = "black", colour = "white"),  
        panel.grid.major = element_line(color = "gray", colour = "white"),  
        panel.grid.minor = element_line(color = "gray", colour = "white")
      ) 
    
    ggplotly(scatter.plot) |>
      layout(
        hoverlabel = list(
          font = list(
            size = 17,
            weight = 20
          )
        )
      )
  })
  
  
  output$best.predictors <- renderPrint({
    
    best.formula$call
    
  })
  
  
  formula <- reactive(
    
    as.formula(paste(input$dep.var, "~", paste(input$predictors, collapse = "+")))
    
  )
  
  reg.model <- reactive(
    
    lm(formula(), data = data)
  )
  
  output$reg.stats <- renderPrint({
    Sys.sleep(2)
    
    summary(reg.model())
    
  })
  
  output$diag.plots <- renderPlot ({
    Sys.sleep(2)
    
    autoplot(reg.model()) +
      theme_bw()
  })
  
  output$ci.table <- renderPrint({
    Sys.sleep(2)
    
    confint(reg.model())
    
  })
  
  r.squared <- reactive({
    summary(reg.model())$r.squared
  })
  
  output$results <- renderText({
    
    predictor.labels <- c(
      "DayOfWeek" = "Day of the Week",
      "Holiday" = "Holiday",
      "Hour" = "Hour",
      "Humidity" = "Humidity",
      "HVACUsage" = "HVAC Usage",
      "LightingUsage" = "Lighting Usage",
      "Month" = "Month",
      "Occupancy" = "Occupancy",
      "RenewableEnergy" = "Renewable Energy",
      "SquareFootage" = "Square Footage",
      "Temperature" = "Temperature"
    )
    
    labels <- sapply(input$predictors, function(x) predictor.labels[x])
    
    value <- r.squared()
    
    adjr2.info <- ""
    if (value >= 0.4) {
      adjr2.info <- paste0("The adjusted r-squared percentage, ", round(value * 100, 2), " , denotes a moderate model fit. The model has some predictive power as the ",
                           paste(labels, collapse = ", "), " moderately explain influences on Energy Consumption.<br>")
    } else if (value >= 0.2) {
      adjr2.info <- paste0("The adjusted r-squared percentage, ", round(value * 100, 2), " , indicates a weak model fit. The model limitedly explains the association between ",
                           paste(labels, collapse = ", "), " and Energy Consumption.<br>")
    } else if (value > 0) {
      adjr2.info <- paste0("The adjusted R-squared percentage, ", round(value * 100, 2), " , signifies a very weak model fit. ", paste(labels, collapse = ", "),
                           " marginally explain influences on Energy Consumption.<br>")
    } else {
      adjr2.info <- paste0("The Adjusted R-Squared percentage, ", round(value * 100, 2), " , suggests that the model is fit poorly therefore the average amount of total energy consumed is a better predictor than ",
                           paste(labels, collapse = ", "), ".<br>")
    }
    
    predictor.labels2 <- c(
      "DayOfWeekMonday" = "Day of the Week: Modnday",
      "DayOfWeekSaturday" = "Day of the Week: Saturday",
      "DayOfWeekSunday" = "Day of the Week: Sunday",
      "DayOfWeekThursday" = "Day of the Week: Thursday",
      "DayOfWeekTuesday" = "Day of the Week: Tuesday",
      "DayOfWeekWednesday" = "Day of the Week: Wednesday",
      "HolidayYes" = "Holiday: Yes",
      "Hour" = "Hour",
      "Humidity" = "Humidity",
      "HVACUsageOn" = "HVAC Usage: On",
      "LightingUsageOn" = "Lighting Usage: On",
      "Month" = "Month",
      "Occupancy" = "Occupancy",
      "RenewableEnergy" = "Renewable Energy",
      "SquareFootage" = "Square Footage",
      "Temperature" = "Temperature"
    )
    
    conf.inter <- confint(reg.model())
    
    coeff <- coef(reg.model())
    
    coeff <- coeff[!names(coeff) == "(Intercept)"]

    ci.info <- ""
    for (pred in names(coeff)) {

      labels2 <- predictor.labels2[pred]
      
      ci.lower <- conf.inter[pred, 1]
      
      ci.upper <- conf.inter[pred, 2]
      
      coeff.value <- coeff[pred]
      
      if (ci.lower <= 0 & ci.upper >= 0) {
        ci.info <- paste0(ci.info,
                          "The confidence intevral for the explanatory variable, ", labels2, ", includes 0 therefore the usefulness of ", labels2, " is negligible.<br><br>")
      } 
      else if (coeff.value >= ci.lower & coeff.value <= ci.upper) {
        ci.info <- paste0(ci.info,
                          "The coefficient estimate for the explanatory variable ", labels2, ", is within the confidence interval which ranges between ", round(ci.lower, 3),
                          " and ", round(ci.upper, 3), ". For every one unit increase in ", labels2, ", total energy consumed increases by ", round(coeff.value, 3), " kWh.<br><br>")
      } 
      else {
        ci.info <- paste0(ci.info,
                          "The coefficient estimate for the explanatory variable, ", labels2, ", is not within the confidence interval which ranges between ", round(ci.lower, 3),
                          " and ", round(ci.upper, 3), ". For every one unit increase in ", labels2, ", total energy consumed increases by ", round(coeff.value, 3), " kWh.<br><br>")
      }
    }
    
    rse.value <- summary(reg.model())$sigma
    
    rse <- paste0("The residual standard error estimates that on average, predictions on total energy consumption would differ from the true measurement by ", paste(round(rse.value, 3)), " kWh.<br>")
    
    pvalue <- paste0("The overall p-value tests for the significance of the linear regression model. The small p-vale suggests that at least one of the independent variables
                      has a significant effect on energy consumption.<br>")
    
    HTML(paste0(
      '<h4 style = "text-align: left; font-weight: bold; margin-bottom: 5px; color: #E7B200;">Residual Standar Error</h4>',
      '<p style = "line-height: 2; text-align: left; font-size: 16px; margin-top: 5px;">', rse, '</p>',
      '<h4 style = "text-align: left; font-weight: bold; margin-top: 20px; margin-bottom: 5px; color: #E7B200;">P-Value</h4>',
      '<p style = "line-height: 2; text-align: left; font-size: 16px; margin-top: 5px;">', pvalue, '</p>',
      '<h4 style = "text-align: left; font-weight: bold; margin-bottom: 5px; color: #E7B200;">Adjusted R-Squared</h4>',
      '<p style = "line-height: 2; text-align: left; font-size: 16px; margin-top: 5px;">', adjr2.info, '</p>',
      '<h4 style = "text-align: left; font-weight: bold; margin-top: 20px; margin-bottom: 5px; color: #E7B200;">Confidence Interval</h4>',
      '<p style = "line-height: 2; text-align: left; font-size: 16px; margin-top: 5px;">', ci.info, '</p>'
    ))
    
  })
  
  
  observeEvent(input$model, {
    
    Sys.sleep(2)
    
    selected.vars <- c("Temperature", "Occupancy", "RenewableEnergy", "HVACUsage", "Humidity" , "Holiday")
    
    selected.values <- c(input$Temperature, input$Occupancy, input$RenewableEnergy, input$HVACUsage, input$Humidity, input$Holiday)
    
    input.data <- as.data.frame(t(selected.values))
    
    colnames(input.data) <- selected.vars
    
    input.data$Temperature = as.numeric(input.data$Temperature)
    
    input.data$Occupancy = as.integer(input.data$Occupancy)
    
    input.data$RenewableEnergy = as.numeric(input.data$RenewableEnergy)
    
    input.data$HVACUsage = as.character(input.data$HVACUsage)
    
    input.data$Humidity = as.numeric(input.data$Humidity)
    
    input.data$Holiday = as.character(input.data$Holiday)
    
    prediction <- predict(best.model, newdata = input.data)
    
    prediction2 <- round(prediction, 2)
    
    output$pred <- renderText({
      
      HTML(
        paste0(
          '<h4 style="font-size: 18px;">Total energy consumed is ', 
          prediction2, 
          ' kWh</h4>'
        )
      )
      
    })
    
  })
  
  output$descriptive.stats <- renderPrint({
    summary(data)
  })
  
  output$assumptions <- renderText({
    
    HTML(
      '<h3 style="text-align: left; font-weight: bold; margin-bottom: 5px; color: #E7B200;"><br>Regression Model Assumptions</h3>
    <p style="line-height: 2; text-align: justify; font-size: 18px; margin-top: 5px;">
      The four principal assumptions of linear regression analysis are linearity and additivity, statistical independence, 
      heteroscedasticity, and normality. If the residuals which are the cumilative differences between observed and predicted values, 
      marginally diverge from the diagonal blue line of the Normal Q-Q plot, and the residuals
      deviate randomly and unsystematically across the Residual vs Fitted, Scale-Location, and Residuals vs. Leverage plots, 
      and,  a flat blue line is present, energy consumption predictions may likely be reflective of the true kWH value.
    </p>'
    )
  })
  
  output$pred.text <- renderPrint({
    HTML(
      '<p style="text-align: left; font-weight: bold; font-size: 18px; margin-top: 5px; color: #E7B200;"><br>Input numeric values for the listed features.</p>'
    )
  })
  
  output$intro <- renderPrint({
    HTML(
      '<h3 style="line-height: 2; text-align: justify; font-weight: bold; margin-bottom: 5px; color: white;">
      <br>Hybrid learning tools have gained significant traction in modern education, blending traditional
teaching methods with interactive digital experiences. This data dashboard provides an effective way to
interactively visualise relationships between explanatory and predictor variables on energy consumption, 
manipulate regression equations, and improve an understanding of model parameters that influence predictions.</h3>'
    )
  })
  
  output$guide <- renderPrint({
    
    HTML(
      '<p style="line-height: 1.5; text-align: justify; margin-bottom: 5px; color: white;">
      <br>The regression modelling dashboard has four sections. The first is an exploratory data analysis
      of energy consumption and eleven additional features that may usefully 
      predict total energy consumed. The features are subset across four types of data 
      visualisations which include a boxplot, bubble chart, heatmap and histogram. To make the most of plot interactivity:<br><br>
•	Zoom in on the bubble charts and utilise the range slider along the x-axis to gain a granular view of variable relationships.<br> 
•	Deselect boxplot key values to progressively discern location measures.<br>
•	Dynamically move your mouse to access information on individual observations.<br><br>
The second section entails selecting independent variables for regression modelling. To assess predictive modelling power, 
regression models of varying sizes are visualised on a scatterplot to 
gauge the combination of features that explain the highest percentage of 
energy consumption variability.<br><br>
The third section is the model building process. Select explanatory variables to build a model with. 
The checkboxes can be deselected to amend the selection of features. 
Read the description on regression model assumptions to understand how to assess the diagnostic plots.  
Thereafter, read the interpretation box on key statistics that describe model performance. 
The adjusted r-squared, coefficient and confidence interval values dynamically update
according to the selected independent variables. <br><br>
The final section involves predicting energy consumption. Input numeric values to 
model the linear regression formula with the highest adjusted r-squared value. 
Skim through the descriptive statistics on the Home tab for possible values to input. 
 
</p>'
    )
    
})
    
  
  
}

shinyApp(ui, server)
