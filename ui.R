options(warn=-1)
library(shiny)
library(leaflet)
library(DT)
vars <- c("% >= High School", "% >= Bachelors", "% White Alone", "% Black Alone", "% Indian Native Alone", 
          "% Asian Alone", "% Other Alone", "% Below Poverty Level","% Uninsur 18-65","% Medicaid Means-Tested Coverage Alone Only",
          "% Medicaid Means-Tested Coverage Alone and Combination","% Private Health Insurance Alone","% Below Poverty Level")
# NCCountyList = read.csv("~/Desktop/NC Spatial Analysis/New Shiny/Data/NCCountyRegions.csv")
NCCountyList = read.csv("data\\NCCountyRegions.csv")

# Define UI for dataset viewer application
fluidPage(
  
  # Application title
  titlePanel("NC County vs. Health Regions with Factfinder and AHRF/CDC Wonder Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a county below"),
      selectInput("county",label="Choose a county",
                  choices=NCCountyList$County.Name, selected="Alamance"),
      width = 3,
      br()
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("County Statistics", DT::dataTableOutput("factfinderCo")),
        tabPanel("County Tracts",textOutput("NumberTracts"),DT::dataTableOutput("CountyTracts"),htmlOutput("countySummary"), DT::dataTableOutput("TractMSR")),
        tabPanel("Region Statistics",htmlOutput("region"),DT::dataTableOutput("RegionCounties"),DT::dataTableOutput("factfinderRe")),
        tabPanel("State Measure Comparisons",DT::dataTableOutput("StateMeans")),
        tabPanel("Map with Selected County",leafletOutput("Map")),
        tabPanel("Population Map",leafletOutput("PopulationCoMap")),
        tabPanel("Percent Variables Map", selectInput("variable",label="Choose a Variable",
                                                         choices=vars, selected=">= High School"),
                 sliderInput("range",label="Range of Interest(%):",min=0,max=100,value=c(0,100)), 
                 htmlOutput("Calculated"),leafletOutput("CalculatedMap"),
                 htmlOutput("Collected"),leafletOutput("CollectedMap")),
        tabPanel("Comparisons Info", DT::dataTableOutput("CountyPValue"),textOutput("ComparisonInfo"), htmlOutput("Missing"),
                 fluidRow(column(width = 7, DT::dataTableOutput("MissingTracts")), column(width = 5, DT::dataTableOutput("MissingCounties"))),
                textOutput("ComparisonInfo2")),
        tabPanel("Region List",DT::dataTableOutput("RegionList")),
        tabPanel("NC General Info",htmlOutput("NCInfo"),DT::dataTableOutput("factfinderSt"))
        
        
        )
      )
      
  )
)