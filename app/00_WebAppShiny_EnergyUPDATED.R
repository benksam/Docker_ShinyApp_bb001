library(tidyverse)
library(shiny)
library(janitor)
library(ggplot2)
library(DT)
library(shinythemes)


# library(purrr)
# library(DT)

Energy <- read_csv("EnergyReduced.csv")
# Energy <- read_csv("https://sanktpetriskole.maps.arcgis.com/sharing/rest/content/items/f1483f9642fb4a21b93e993087647ba0/data")
# Energy <- read_csv("https://sanktpetriskole.maps.arcgis.com/sharing/rest/content/items/f1483f9642fb4a21b93e993087647ba0/data")


str(Energy)
colabels = names(Energy)

Energy[,3:4] = lapply(Energy[,3:4], as.factor)

# Reformat to snake case
Energy <- Energy %>%  
  clean_names("lower_camel")  #  vs. snake


Energy = Energy %>%
  mutate(incomeGroup = fct_relevel(incomeGroup, "Low income", 
                                   "Lower middle income", 
                                   "Upper middle income",
                                   "High income"))


ui <- fluidPage(theme = shinytheme("lumen"), # themeSelector(),
  
  tabsetPanel(type = "tabs",
              id = "tabsetpanel",
              
              # tab0          
              tabPanel(title = "About this App", 
                       
                       
                       # Application title
                       titlePanel(h3(tags$b("Income, Energy Consumption and CO2-emissions (Source: WB)")),
                                  windowTitle = ("Income and Energy")),
                       
                       # h4(em("Source: WB")),
                       
                       hr(), 
                       
                       
                       sidebarLayout(
                         
                         sidebarPanel(offset = 1,
                                      h3(strong("Instructions")),
                                      p(h4(em("This part of the panel is where you change the inputs to visualize your: "))),
                                      
                                      radioButtons("button", "", choices = c("Plots", "Tables"), selected = "Plots"),
                                      
                                      
                                      conditionalPanel(
                                        condition = "input.button == 'Plots'",
                                        plotOutput("plot0", height = 270, width = 370)
                                      ),
                                      
                                      conditionalPanel(
                                        condition =  "input.button == 'Tables'",
                                        tableOutput("table0")
                                      ),
                                      
                                      # img(src = "rstudio.png", height = 140, width = 200),
                                      
                                      h5("This App was made using Shiny, a product of ", 
                                         span("RStudio", style = "color:blue"))
                         ),
                         
                         mainPanel(width = 7, 
                                   
                                   h3(strong("Introduction to this App")),
                                   p(h4("This App helps you visualize some relationships among:")),
                                   p(h4(em(strong("1. Income")))),
                                   p(h4(em(strong("2. Energy Consumption")))),
                                   p(h4(em(strong("3. CO2-emissions")))), 
                                   p(h4("in an ", em("incredibly easy way"), 
                                        "and to interact with different dimensions of the data.")),
                                   br(),
                                   # p(h4("For an introduction and live examples, visit the ",
                                   #   a("Shiny homepage.", 
                                   #     href = "http://shiny.rstudio.com"))),
                                   
                                   h3(strong("Features")),
                                   p(h4("1. Create beautiful", strong("plots"), "by changing the inputs from the sidepanel on the left.")),
                                   p(h4("2. Manipulate the", strong("tables"),"by choosing or sorting interesting data from them."))))
                       
                       
                       
              ), 
              
              
              
              
              # tab1          
              tabPanel(title = "Energy Use by Region", 
                       
                       
                       # Application title
                       titlePanel(h3(tags$b("Income, Energy Consumption and CO2-emissions (Source: WB)"))),
                       
                       
                       hr(), 
                       
                       # Sidebar layout with a input and output definitions
                       fluidRow(   # sidebarLayout(
                         
                         # Inputs(s)
                         column(width = 3, # sidebarPanel(sidebarLayout(
                         
                         # Inputs(s)
                         # sidebarPanel(width = 3,
                           
                           textInput("title1", "Title", "GDP vs Energy Consumption"),
                           
                           br(), 
                           
                           # Action button for plot title
                           actionButton(inputId = "update_plot_title1", 
                                        label = "Update plot title"),
                           
                           br(), br(),
                           
                           sliderInput(inputId = "energyUse1", label = "Energy Consumption (Kg Oil Equivalent)",
                                       min = 0, 20000,
                                       value = c(min(Energy$energyUsePerCapitaKgOilEq), 
                                                 max(Energy$energyUsePerCapitaKgOilEq))),
                           
                           br(), 
                           
                           selectInput("region1", "Region",
                                       choices = c("All", levels(Energy$region))),
                           
                           
                           br(), 
                           
                           
                           
                           # Add a checkbox for line of best fit
                           checkboxInput("fit1", "Check for any relashionship", FALSE),
                         
                         # Show data table
                         checkboxInput(inputId = "show_data1",
                                       label = "Showing the data table ...",
                                       value = FALSE),
                         
                         
                         hr()
                         ),
                         
                       column(width = 9, 
                         # Output(s)
                         # mainPanel(width = 9,
                           # Add a plot output
                           plotOutput("plot1", height = 500))),
                         
                         
                       # fluidRow(   # sidebarLayout(
                       #   
                       #   # Inputs(s)
                       #   column(width = 12,
                       #   
                       #   DT::dataTableOutput("table1")
                       #   ))),
                       # 
              
              fluidRow(
                column(12, 
                       
                       conditionalPanel("input.show_data1 == true"),     # , h3("Data table") Third level header: Data table
                       DT::dataTableOutput(outputId = "table1")))),
              
              
              # tab2
              tabPanel(title = "Energy Use by Income Group", 
                       
                       
                       
                       # Application title
                       titlePanel(h3(tags$b("Income, Energy Consumption and CO2-emissions (Source: WB)"))),
                       
                       
                       hr(), 
                       
                       # Sidebar layout with a input and output definitions
                       fluidRow(
                         
                         # Inputs(s)
                         column(width = 3,
                           
                           textInput("title2", "Title", "GDP vs Energy Consumption by Region"),
                           
                           br(), 
                           
                           # Action button for plot title
                           actionButton(inputId = "update_plot_title2", 
                                        label = "Update plot title"),
                           
                           br(), br(),
                           
                           
                           sliderInput(inputId = "energyUse2", label = "Energy Consumption (Kg Oil Equivalent)",
                                       min = 0, 20000,
                                       value = c(min(Energy$energyUsePerCapitaKgOilEq), 
                                                 max(Energy$energyUsePerCapitaKgOilEq))),
                           
                           br(), 
                           
                           # # Add a checkbox for line of best fit
                           # radioButtons("group", "Filtering by ...", choices("Region" = "region",
                           #                                                   "Income Group" = "incomeGroup"), 
                           #              selected = "incomeGroup")),
                           
                           radioButtons("incomeGroup1", "Income Group",
                                        choices = c("All", levels(Energy$incomeGroup))),
                           
                           br(),  
                           
                           
                           
                           # Add a checkbox for line of best fit
                           checkboxInput("fit2", "Check for any relashionship", FALSE),
                       
                           # Show data table
                           checkboxInput(inputId = "show_data2",
                                         label = "Showing the data table ...",
                                         value = FALSE),
                           
                         hr()
                         
                         ),
                         
                         
                         # Output(s)
                         column(width = 9,
                           # Add a plot output
                           plotOutput("plot2", height = 500))),
                       
                       # fluidRow(  
                       #   # Output(s)
                       #   column(width = 12,
                       #     DT::dataTableOutput("table2")))),
                       
                       fluidRow(
                         column(12, 
                                
                                conditionalPanel("input.show_data2 == true"),     # , h3("Data table") Third level header: Data table
                                DT::dataTableOutput(outputId = "table2")))),                     
                   
              
              # tab3
              tabPanel(title = "CO2-emissions by Income Group", 
                       
                       
                       
                       # Application title
                       titlePanel(h3(tags$b("Income, Energy Consumption and CO2-emissions (Source: WB)"))),
                       
                       
                       hr(), 
                       
                       # Sidebar layout with a input and output definitions
                       sidebarLayout(
                         
                         # Inputs(s)
                         sidebarPanel(width = 3,
                           
                           textInput("title3", "Title", "GDP vs CO2-emissions by Income Group"),
                           
                           br(), 
                           
                           # Action button for plot title
                           actionButton(inputId = "update_plot_title3", 
                                        label = "Update plot title"),
                           
                           br(), br(), 
                           
                           
                           sliderInput(inputId = "co2emissions", label = "CO2 emissions (tons per capita)",
                                       min = 0, 50,
                                       value = c(min(Energy$co2EmissionsTonsPerCapita), 
                                                 max(Energy$co2EmissionsTonsPerCapita))),
                           
                           br(), 
                           
                           # # Add a checkbox for line of best fit
                           # radioButtons("group", "Filtering by ...", choices("Region" = "region",
                           #                                                   "Income Group" = "incomeGroup"), 
                           #              selected = "incomeGroup")),
                           
                           radioButtons("incomeGroup2", "Income Group",
                                        choices = c("All Countries", levels(Energy$incomeGroup))),
                           
                           br(),  
                           
                           
                           
                           # Add a checkbox for line of best fit
                           checkboxInput("fit3", "Check for any relashionship", FALSE)),
                           
                         
                         
                         
                         # Output(s)
                         mainPanel(width = 9,
                           # Add a plot output
                           plotOutput("plot3", brush = "plot_brush"),
                           # DT::dataTableOutput("table2"))
                           # Show data table
                           DT::dataTableOutput(outputId = "tablebrush"), # = table3
                           br()
                         ))),
              
              
              
              # tab4
              tabPanel(title = "Summarised Energy Data by Income Group", 
                       
                       
                       
                       # Application title
                       titlePanel(h3(tags$b("Income, Energy Consumption and CO2-emissions (Source: WB)"))),
                       
                       
                       hr(), 
                       
                       # Sidebar layout with a input and output definitions
                       fluidRow(
                         
                         # Inputs(s)
                         column(3, # offset = 0.5,
                                
                                br(), 
                                
                                
                                # Add a checkbox for line of best fit
                                selectInput("region2", "Distribution for ...", 
                                            choices = c("All Countries", levels(Energy$region)), #"All", 
                                            selected = "All Countries"),
                                
                                
                                br(),
                                
                                
                                # Set alpha level
                                sliderInput(inputId = "alpha", 
                                            label = "Transparency", 
                                            min = 0, max = 1, 
                                            value = 0.5)),
                         
                         
                         # Output(s)
                         
                         column(8, offset = 1, 
                                # Add a plot output
                                
                                h3("Summary Statistics"),
                                tableOutput(outputId = "table4"), # DT::dataTableOutput(outputId = "table4"),
                                br()),
                         hr(), br()), 
                       
                       
                       fluidRow(
                         
                         # Inputs(s)
                         column(4,  
                                plotOutput("plot41")),
                         
                         column(4,  
                                plotOutput("plot42")),
                         
                         
                         column(4,
                                plotOutput("plot43"))
                         
                       ))))





server <- function(input, output) {
  
  
  # Tab0 
  
  output$table0 <- renderTable({ 
    colnames(Energy) = colabels
    head(Energy[,c(1:4)], 3)  
  }) 
  # if (input$button == "Tables") {
  
  
  output$plot0 <- renderPlot({
    # Use the same filtered data that the table uses
    
    
    data = Energy
    g0 = ggplot(data, aes(gdpPerCapita, energyUsePerCapitaKgOilEq, col = incomeGroup)) +
      geom_point() +
      labs(x = "GDP per Capita", y = "Energy Consumption Per Capita") +
      ggtitle(input$title1) +
      theme(plot.title = element_text(size = 15, face = "bold")) + 
      guides(color = guide_legend(title = "Income Group", title.position = "top")) +
      theme_minimal()
    # scale_x_log10()
    # if (input$fit1 == TRUE) {
    #   g1 <- g1 + geom_smooth(method = "lm", se = FALSE)
    # if (input$button == "Plots") {}
    g0
    
    
  })   
  
  
  # Create FIRST a reactive variable named "filtered_data"
  # Tab1  
  filtered_data1 <- reactive({
    # Filter the data
    data1 <- Energy
    data1 <- subset(data1,  energyUsePerCapitaKgOilEq >= input$energyUse1[1] & 
                      energyUsePerCapitaKgOilEq <= input$energyUse1[2])
    if (input$region1 != "All"){
      data1 <- subset(data1,region == input$region1)
    }
    # colnames(data1) = colabels 
    data1
  })
  
  # Tab2  
  filtered_data2 <- reactive({
    # Filter the data
    data2 <- Energy
    data2 <- subset(data2,  energyUsePerCapitaKgOilEq >= input$energyUse2[1] & 
                      energyUsePerCapitaKgOilEq <= input$energyUse2[2])
    if (input$incomeGroup1 != "All"){
      data2 <- subset(data2, incomeGroup == input$incomeGroup1)
    }
    # colnames(data2) = colabels 
    data2
  })
  
  
  # Tab3  
  filtered_data3 <- reactive({
    # Filter the data
    data3 <- Energy
    data3 <- subset(data3,  co2EmissionsTonsPerCapita >= input$co2emissions[1] & 
                      co2EmissionsTonsPerCapita <= input$co2emissions[2])
    if (input$incomeGroup2 != "All Countries"){
      data3 <- subset(data3, incomeGroup == input$incomeGroup2)
    }
    # colnames(data2) = colabels 
    data3
  })
  
  
  # Tab4  - Summary Data
  filtered_data40 <- reactive({
    # Filter the data
    data40 <- Energy
    data40 <- Energy %>% 
      group_by(incomeGroup) %>% 
      summarise(`Average Income` = round(mean(gdpPerCapita),0),
                `Average Energy Consumption` = round(mean(energyUsePerCapitaKgOilEq),1),
                `Average CO2-emissions` = round(mean(co2EmissionsTonsPerCapita),1))
    
    # if (input$region != "All"){
    #   data4 <- subset(data4,region == input$region)
    # }
    # colnames(data4) = colabels 
    colnames(data40)[1] = "Income Group"
    data40
  })  
  
  
  # Tab4  - Summary Data
  filtered_data41 <- reactive({
    # Filter the data
    data41 <- Energy
    data41 <- Energy %>% 
      group_by(incomeGroup, region) %>% 
      summarise(`Average Income` = round(mean(gdpPerCapita),0),
                `Average Energy Consumption` = round(mean(energyUsePerCapitaKgOilEq),1),
                `Average CO2-emissions` = round(mean(co2EmissionsTonsPerCapita),1))
    
    # if (input$region != "All"){
    #   data4 <- subset(data4,region == input$region)
    # }
    # colnames(data4) = colabels 
    colnames(data41)[1] = "Income Group"
    
    data41
  })  
  
  
  # Tab4  - Plots
  filtered_data42 <- reactive({
    # Filter the data
    data42 <- Energy
    if (input$region2 != "All Countries"){
      data42 <- subset(data42,  region %in% input$region2)
      
      
    }
    
    data42
  }) 
  
  output$table1 <- DT::renderDataTable({
    if(input$show_data1){ 
    DT::datatable(filtered_data1(), 
                  colnames = colabels,
                  options = list(pageLength = 10), 
                  rownames = FALSE) 
    }
    
  })
  
  
  output$table2 <- DT::renderDataTable({
    if(input$show_data2){
    DT::datatable(filtered_data2(), 
                  colnames = colabels,
                  options = list(pageLength = 10), 
                  rownames = FALSE)  
    
    }
  })
  
  output$table4 <- renderTable({ # DT::renderDataTable({
    if (input$region2 == "All Countries"){
      filtered_data40()} else {
    data = filtered_data41() %>% filter(region %in% input$region2) 
    colnames(data)[2] = "World Region"
    data
    }
    # DT::datatable(
    #               colnames = colabels) 
    
    
  })  
  
  # New plot title1
  new_plot_title1 <- eventReactive(
    eventExpr = input$update_plot_title1, 
    valueExpr = { (input$title1) }, #toTitleCase(
    ignoreNULL = FALSE
  )
  
  
  # Create the plot render function  
  output$plot1 <- renderPlot({
    # Use the same filtered data that the table uses
    data = filtered_data1()
    g1 = ggplot(data, aes(gdpPerCapita, energyUsePerCapitaKgOilEq, col = incomeGroup)) +
      geom_point() +
      labs(x = "GDP per Capita", y = "Energy Consumption Per Capita") +
      ggtitle(new_plot_title1()) +
      theme(plot.title = element_text(size = 15, face = "bold")) + 
      guides(color = guide_legend(title = "Income Group", title.position = "top")) +
      theme_minimal()
    # scale_x_log10()
    if (input$fit1 == TRUE) {
      g1 <- g1 + geom_smooth(method = "lm", se = FALSE)
    }
    g1
    
  })
  

  # New plot title2
  new_plot_title2 <- eventReactive(
    eventExpr = input$update_plot_title2, 
    valueExpr = { (input$title2) }, #toTitleCase(
    ignoreNULL = FALSE
  )  
  
  # Create the plot render function  
  output$plot2 <- renderPlot({
    # Use the same filtered data that the table uses
    data = filtered_data2()
    g2 = ggplot(data, aes(gdpPerCapita, energyUsePerCapitaKgOilEq, col = region)) +
      geom_point() +
      labs(x = "GDP per Capita", y = "Energy Consumption Per Capita") +
      facet_wrap(~ region) +
      ggtitle(new_plot_title2()) +
      theme(plot.title = element_text(size = 15, face = "bold")) + 
      guides(color = guide_legend(title = "Region", title.position = "top")) +
      theme_minimal()
    # scale_x_log10()
    if (input$fit2 == TRUE) {
      g2 <- g2 + geom_smooth(method = "lm", se = FALSE)
    }
    g2
    
  })
  
  # New plot title3
  new_plot_title3 <- eventReactive(
    eventExpr = input$update_plot_title3, 
    valueExpr = { (input$title3) }, #toTitleCase(
    ignoreNULL = FALSE
  )  
  
   
  # Create the plot render function  
  output$plot3 <- renderPlot({
    # Use the same filtered data that the table uses
    data = filtered_data3()
    g3 = ggplot(data, aes(gdpPerCapita, co2EmissionsTonsPerCapita, col = incomeGroup)) +
      geom_point() +
      labs(x = "GDP per Capita", y = "CO2-emissions Per Capita") +
      ggtitle(new_plot_title3()) +
      theme(plot.title = element_text(size = 15, face = "bold")) + 
      guides(color = guide_legend(title = "Income Group", title.position = "top")) +
      theme_minimal()
    # scale_x_log10()
    if (input$fit3 == TRUE) {
      g3 <- g3 + geom_smooth(method = "lm", se = FALSE)
    }
    g3
    
  })
  
  #BRUSHED table 
  
  # Create data table
  output$tablebrush <- DT::renderDataTable({
    brushedPoints(filtered_data3(), brush = input$plot_brush) %>%
      select("Country Name" = countryName, 
             "Region" = region, 
             "Income Group" = incomeGroup,
             "GDP per Capita" = gdpPerCapita,
             "CO2-emissions per Capita" = co2EmissionsTonsPerCapita)
  }) 
  
  
  
  # Create the plot render function  
  output$plot41 <- renderPlot({
    # Use the same filtered data that the table uses
    data = filtered_data42()
    g41 = ggplot(data, aes(gdpPerCapita, fill = incomeGroup)) +
      geom_density(alpha = input$alpha) +
      #      facet_wrap(~ incomeGroup) +
      labs(x = "Log(GDP per Capita)", y = "Density") +
      ggtitle("Income Distribution") +
      theme(plot.title = element_text(size = 30, face = "bold")) + 
      guides(fill = guide_legend(title = "Income Group", title.position = "top")) +
      
      labs(xlim = c(0,120000)) +
      theme_minimal()
    # scale_x_log30()
    # if (input$fit1 == TRUE) {
    #   g3 <- g3 + geom_smooth(method = "lm", se = FALSE)
    # }
    g41
    
  })    
  
  # Create the plot render function  
  output$plot42 <- renderPlot({
    # Use the same filtered data that the table uses
    data = filtered_data42()
    g42 = ggplot(data, aes(energyUsePerCapitaKgOilEq, fill = incomeGroup)) +
      geom_density(alpha = input$alpha) +
      #      facet_wrap(~ incomeGroup) +
      labs(x = "Log(Energy use per capita (kg oil eq))", y = "Density") +
      ggtitle("Energy Consumption Distribution") +
      theme(plot.title = element_text(size = 30, face = "bold")) + 
      guides(fill = guide_legend(title = "Income Group", title.position = "top")) +
      
      labs(xlim = c(0,120000)) +
      theme_minimal()
    # scale_x_log30()
    # if (input$fit1 == TRUE) {
    #   g3 <- g3 + geom_smooth(method = "lm", se = FALSE)
    # }
    g42
    
  })
  
  # Create the plot render function  
  output$plot43 <- renderPlot({
    # Use the same filtered data that the table uses
    data = filtered_data42()
    g43 = ggplot(data, aes(co2EmissionsTonsPerCapita, fill = incomeGroup)) +
      geom_density(alpha = input$alpha) +
      #      facet_wrap(~ incomeGroup) +
      labs(x = "Log(CO2 emissions (tons per capita))", y = "Density") +
      ggtitle("CO2-emissions Distribution") +
      theme(plot.title = element_text(size = 30, face = "bold")) + 
      guides(fill = guide_legend(title = "Income Group", title.position = "top")) +
      
      labs(xlim = c(0,120000)) +
      scale_x_log10() +
      theme_minimal()
    
    # if (input$fit1 == TRUE) {
    #   g3 <- g3 + geom_smooth(method = "lm", se = FALSE)
    # }
    g43      
    
    
  })  
  
  
  
}

shinyApp(ui = ui, server = server)



