library(shiny)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(scales)
library(viridis)
library(viridisLite)
library(readxl)

### loading the dataset
# 
infection_neg <- read_excel("Book1.xlsx", sheet = 1)
infection_post <- read_excel("Book1.xlsx", sheet = 2)
resistance_neg <- read_excel("Book1.xlsx", sheet = 3)
resistance_post <- read_excel("Book1.xlsx", sheet = 4)
sensitivity_neg <- read_excel("Book1.xlsx", sheet = 5)
sensitivity_post <- read_excel("Book1.xlsx", sheet = 6)
CRE <- read_excel("Book1.xlsx", sheet = 7)
consumption <- read_excel("Book1.xlsx", sheet = 8)




### Writing Shiny App


ui <- fluidPage(
  titlePanel("Saudi Arabia Antimicrobial Data"),
  
  wellPanel( tags$h2("Resistant Gram Negative Isolates"),
             fluidRow(
               column(
                 width = 12,
                 plotOutput("saudi_map_plot")),
               
               column(
                 width = 12,
                 plotOutput("barplot"))
             )),
  
  wellPanel( tags$h2("Resistant Gram positive Isolates"),
             fluidRow(
               column(
                 width = 12,
                 plotOutput("saudi_map_plot2")),
               
               column(
                 width = 12,
                 plotOutput("barplot2"))
               
             )),
  
  wellPanel( tags$h2("CRE Isolates"),
             fluidRow(
               column(
                 width = 12,
                 plotOutput("saudi_map_plot3")),
               
               column(
                 width = 12,
                 plotOutput("barplot3"))
               
             )),
  
  wellPanel( tags$h2("Antibiotic Consumption per Region"),
             fluidRow(
               column(
                 width = 12,
                 plotOutput("saudi_map_plot4")),
               
               column(
                 width = 12,
                 plotOutput("barplot4"))
               
             ))
  
)
server <- function(input, output) {
  # Prepare the data frame for plotting
  plot_data <- resistance_post %>% 
    select(everything()) %>% 
    drop_na() %>% 
    pivot_longer(cols = c(Ampicillin, Erythromycin, Linezolid, Moxifloxacin,
                          Vancomycin, Deptomycin, Teicoplanin, Oxacallin, Cefoxtin),
                 names_to = "Antibiotic", values_to = "Resistance")
  
  plot_data1 <- resistance_neg %>% 
    select(everything()) %>% 
    drop_na() %>% 
    pivot_longer(cols = c(Meropenem, Impenem, Cefixime, Cefotaxime ,         
                          Ceftriaxone,  Cefuroxime, Ceftazidime, Piperacilin_Tazobactam,
                          Ciprofloxacin, Tigecycline, Colistin,  Azithromycin,          
                          Linezolid, Fosfomycin),
                 names_to = "Antibiotic", values_to = "Resistance")
  
  plot_data2 <- consumption %>% 
    select(everything()) %>% 
    drop_na() %>% 
    pivot_longer(cols = c(Meropenem, Imipenem_cilastatin, Piperacillin_tazobactam,
                          Vancomycin, Colistin),
                 names_to = "Antibiotic", values_to = "consumption_rate")
   
  
  ### Preparing the Map dataset
  
  KSA <- read_sf("gadm40_SAU_1.shp")
  
  KSAc <- st_centroid(KSA)
  
  ### Rename the Region to correspond with the in the dataset
  
  KSA$NAME_1[KSA$NAME_1=="'Asir"] <- "Asir"
  
  KSA$NAME_1[KSA$NAME_1=="Al Bahah"] <- "Al-Baha"
  
  KSA$NAME_1[KSA$NAME_1=="Al Hudud ash Shamaliyah"] <- "Northern Borders"
  
  KSA$NAME_1[KSA$NAME_1=="Al Jawf"] <- "Al-Jouf"
  
  KSA$NAME_1[KSA$NAME_1=="Al Madinah"] <- "Medinah"
  
  KSA$NAME_1[KSA$NAME_1=="Al Qassim"] <- "Al-Qassim"
  
  KSA$NAME_1[KSA$NAME_1=="Ar Riyad"] <- "Riyadh"
  
  KSA$NAME_1[KSA$NAME_1=="Ash-Sharqīyah"] <- "Eastern"
  
  KSA$NAME_1[KSA$NAME_1=="Ḥaʼil"] <- "Hail"
  
  KSA$NAME_1[KSA$NAME_1=="Jizan"] <- "Jazan"
  
  KSA$NAME_1[KSA$NAME_1=="Makkah"] <- "Makkah"
  
  KSA$NAME_1[KSA$NAME_1=="Najran"] <- "Najran"
  
  KSA$NAME_1[KSA$NAME_1=="Tabuk"] <- "Tabuk"
  
  
  
  region_order <- sort(c(
    "Asir Region", "Al-Baha Region", "Northern Borders Region", "Al-Jouf Region",         
    "Medinah Region", "Al-Qassim Region", "Riyadh Region", "Eastern Region",         
    "Hail Region", "Jazan Region", "Makkah  Region", "Najran Region",          
    "Tabuk  Region")
  )
  
  # Define colors (adjust as needed)
  colors <- c("#f8766d", "#e18a00", "#be9c00", "#8cab00", "#24b700", "#00be70",
              "#00c1ab", "#00bbda", "#00acfc", "#8b93ff", "#d575fe", "#f962dd",
              "#ff65ac")
  
  # Create the barplot1
  output$barplot <- renderPlot({
    ggplot(plot_data1, aes(x = Antibiotic, y = Resistance, fill = Region)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Resistance of Antibiotics by Region",
           x = "Antibiotics", y = "Resistance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = colors, labels = region_order) +
      facet_wrap(~ Pathogens)
  })
  
  
  # Create the barplot2
  output$barplot2 <- renderPlot({
    ggplot(plot_data, aes(x = Antibiotic, y = Resistance, fill = Region)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Resistance of Antibiotics by Region",
           x = "Antibiotics", y = "Resistance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = colors, labels = region_order) +
      facet_wrap(~ Pathogens)
  })
  
  
  # Plot the barplot3
  output$barplot3 <- renderPlot({
    ggplot(CRE, aes(x = Total_Carbapenemases_Isolated_Cases , y = Region, fill = Region )) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Number CRE by Region",
           x = "Number CRE Isolates", y = "Resistance") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = colors, labels = region_order) +
      facet_wrap(~ Pathogens)
  })
  
  # Plot the barplot4
  output$barplot4 <- renderPlot({
    ggplot(plot_data2, aes(x = Antibiotic , y = consumption_rate, fill = Region )) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Consumption of Antibiotics by Region",
           x = "Antibiotics", y = "Consumption") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = colors, labels = region_order)
    
  })
  
  # Plot the map1
  output$saudi_map_plot <- renderPlot({
    ggplot() +
      geom_sf(data = KSA, aes(fill = NAME_1)) +
      geom_sf(data = KSAc) +
      labs(title = "Resistance  Antibiotics by Region in Saudi Arabia",
           fill = "Region") +  # Set legend title
      theme_void() +
      theme(aspect.ratio = 1)
  })
  
  
  # Plot the map2
  output$saudi_map_plot2 <- renderPlot({
    ggplot() +
      geom_sf(data = KSA, aes(fill = NAME_1)) +
      geom_sf(data = KSAc) +
      labs(title = "Resistance Antibiotics by Region in Saudi Arabia",
           fill = "Region") +  # Set legend title
      theme_void() +
      theme(aspect.ratio = 1)
  })
  
  
  # Plot the map3
  output$saudi_map_plot3 <- renderPlot({
    ggplot() +
      geom_sf(data = KSA, aes(fill = NAME_1)) +
      geom_sf(data = KSAc) +
      labs(title = "Rate of CRE by Region in Saudi Arabia",
           fill = "Region") +  # Set legend title
      theme_void() +
      theme(aspect.ratio = 1)
    
  })
  
  # Plot the map4
  output$saudi_map_plot4 <- renderPlot({
    ggplot() +
      geom_sf(data = KSA, aes(fill = NAME_1)) +
      geom_sf(data = KSAc) +
      labs(title = "Antibiotics Consumption by Region in Saudi Arabia",
           fill = "Region") +  # Set legend title
      theme_void() +
      theme(aspect.ratio = 1)
    
  })
  
}

shinyApp(ui = ui, server = server)
