library(shiny)
library(highcharter)
library(countrycode)
library(shinydashboard)
library(dplyr)
library(openintro)

sales <- read.csv('salespoint.csv')
ui<-
dashboardPage(
  dashboardHeader(title = "Map"),
  dashboardSidebar(
    
    sidebarMenu( 
    selectInput('yearid','Select Year for Global Sales',choices = c(2003,2004,2005),selected = 2003)
    )),
  dashboardBody(
    tabBox(title = 'ToyShop',id = 'tabset1',width = 12, tabPanel('WorldSales',highchartOutput('chart',height = '500px')),tabPanel('US-Sales',selectInput('product','Select ProductClass',choices = unique(sales$PRODUCTLINE)),highchartOutput('charts',height = '400px')))
    
  )
)

server <- function(input, output, session){
  
 
  
  total <- reactive(
    {
    sales %>%
    filter(YEAR_ID == as.numeric(input$yearid)) %>% 
    group_by(COUNTRY) %>%
    summarize("TOTAL_SALES" = as.integer(sum(SALES)))%>% 
    mutate(iso3 = countrycode(COUNTRY,"country.name","iso3c"))
    }
    )
  
  
  prsale <- reactive({
    sales %>% 
      filter(COUNTRY == 'USA') %>%
      filter(PRODUCTLINE == input$product) %>%  
      group_by(STATE) %>% 
      summarise("TOTAL_SALES" = as.integer(sum(SALES)))%>% 
      mutate(State = abbr2state(STATE))
  })
  
 
  
  output$chart <- renderHighchart(highchart(type = "map") %>% 
                                    hc_add_series_map(map = worldgeojson, df = total(), value = "TOTAL_SALES", joinBy = "iso3") %>% 
                                    hc_colorAxis(stops = color_stops()) %>% 
                                    hc_tooltip(useHTML=TRUE,headerFormat='',pointFormat = paste0(input$yearid,'  {point.COUNTRY} Sales : {point.TOTAL_SALES} ')) %>% 
                                    hc_title(text = 'Global Sales') %>% 
                                    hc_subtitle(text = paste0('Year: ',input$yearid)) %>% 
                                    hc_exporting(enabled = TRUE,filename = 'custom')
                                    )
  output$charts <- renderHighchart(highchart() %>%
                                     hc_title(text = "Sales in US $") %>%
                                     hc_subtitle(text = paste0("Product Class: ",input$product)) %>%
                                     hc_add_series_map(usgeojson, prsale(),
                                                       name = "State",
                                                       value = "TOTAL_SALES",
                                                       joinBy = c("woename", "State")) %>%
                                     hc_mapNavigation(enabled = T) %>% 
                                     hc_colorAxis(stops = color_stops()) )
  
  observeEvent(input$yearid,{
    updateTabItems(session,'tabset1','WorldSales')
  })
}

shinyApp(ui=ui, server=server)