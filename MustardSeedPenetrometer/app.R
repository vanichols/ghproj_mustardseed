library(tidyverse)
library(shinythemes) 
library(plotly)
library(ggpubr) # for theme_pubclean
#devtools::install_github("johannesbjork/LaCroixColoR")
library(LaCroixColoR)


dat <- read_csv("data-resis.csv") %>% 
    mutate(field = str_to_title(field),
           field = ifelse(field == "Og", "OG", field))



dd_field <- c("All Fields", dat %>% select(field) %>% pull() %>% unique() %>% sort())
dd_lc <- lacroix_palettes %>% 
    unlist() %>% 
    names() %>% 
    parse_character() %>% 
    str_remove_all("[[:digit:]]") %>% 
    unique()




ui <-
    fluidPage(theme = shinytheme("sandstone"),
              
              navbarPage(
                  "Mustard Seed Penetrometer",
                  
                  tabPanel("Map Overview",
                           plotOutput('plot1'),
                           
                           hr(),
                           
                           sliderInput(
                                   "mydepth",
                                   "Select a Depth Range (cm):",
                                   min = 0,
                                   max = 30,
                                   step = 5,
                                   value = c(0, 10)
                               )
                           ),
                  
                  tabPanel("Field Profiles",
                           
                           sidebarLayout(
                               sidebarPanel(
                                   selectizeInput('myf',
                                                  'Select a Field:',
                                                  dd_field,
                                                  "All Fields"),
                                   selectizeInput(
                                       'mylc',
                                       'Select Your Favorite LaCroix Flavor:',
                                       dd_lc,
                                       "PeachPear"
                                   )
                               ),
                               
                               mainPanel(plotOutput('plot2'))
                           )),
                  
                  tabPanel("Mean Profiles",
                           
                           sidebarLayout(
                               sidebarPanel(
                                   selectizeInput('myf2',
                                                  'Select a Field:',
                                                  dd_field,
                                                  "All Fields"),
                                   selectizeInput(
                                       'mylc2',
                                       'Select Your Favorite LaCroix Flavor:',
                                       dd_lc,
                                       "PeachPear"
                                   )
                               ),
                               
                               mainPanel(plotlyOutput('plot3'))
                           ))
              ))


server <- function(input, output) {
    
    dataset1 <- reactive({
        dat %>%
            filter(depth_cm >= input$mydepth[1]) %>%
            filter(depth_cm <= input$mydepth[2]) %>%
            group_by(field, row, column) %>%
            summarise(resis_kpa = mean(resis_kpa))
        
    })
    
    output$plot1 <- renderPlot({
        
        dataset1() %>% 
            ggplot(aes(column, row)) +
            geom_raster(aes(fill = resis_kpa), interpolate = TRUE) +
            scale_y_reverse() +
            facet_wrap( ~ field, scales = "free") +
            scale_fill_viridis_c(option = "plasma") +
            labs(title = "Top Is North", 
                 fill = "Resistance (kPa)") +
            theme_minimal() +
            theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  legend.position = "bottom")
        
        
    })
    
    dataset2 <- reactive({
        
        if (input$myf == "All Fields") {
            dat} else {
        dat %>%
            filter(field == input$myf)
            }
        
    })
    

   
    output$plot2 <- renderPlot({

        dataset2() %>%
            ggplot(aes(depth_cm, resis_kpa, group = samp_id)) +
            geom_line(aes(color = field)) +
            facet_grid(.~field) +
            scale_color_manual(values = lacroix_palette(as.name(input$mylc), type = "discrete")) +
            labs(y = "Resistance (kPa)",
                 x = "Depth (cm)") +
            guides(color = F) +
            scale_x_reverse() +
            coord_flip() +
            theme_minimal() +
            theme(legend.position = "bottom")

        

    })
    
    dataset3 <- reactive({
        
        if (input$myf2 == "All Fields") {
            dat} else {
                dat %>%
                    filter(field == input$myf2)
            }
        
    })
    
    
    
    output$plot3 <- renderPlotly({
        
        p3 <- dataset3() %>%
            group_by(depth_cm, field) %>% 
            summarise(resis_kpa = mean(resis_kpa, na.rm = T)) %>% 
            ggplot(aes(depth_cm, resis_kpa, group = field,
                       text = paste("Field:", field))) +
            geom_line(aes(color = field), size = 4) +
            scale_color_manual(values = lacroix_palette(as.name(input$mylc2), type = "discrete")) +
            labs(y = "Resistance (kPa)",
                 x = "Depth (cm)") +
            guides(color = F) +
            scale_x_reverse() +
            coord_flip() +
            theme_minimal() +
            theme(legend.position = "bottom")
        
        ggplotly(p3, tooltip = "text" )
        
        
        
    })
    
    
}

shinyApp(ui, server)