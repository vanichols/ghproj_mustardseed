library(tidyverse)
library(shinythemes) 
library(plotly)
library(ggpubr) # for theme_pubclean
#devtools::install_github("johannesbjork/LaCroixColoR", force = T)
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
              
              navbarPage("Mustard Seed Penetrometer"),
              
              tabsetPanel(
                  
                  ###--start tab1----
                  tabPanel(
                      "What is this?",
                      
                      fluidRow(column(12, includeMarkdown("about.md"))),
                      fluidRow(
                          column(2,
                                 tags$img(
                                     src = "field-map-labelled.png",
                                     height = 400,
                                     width = 300,
                                     align = "center"
                                 )),
                          column(3,
                                 tags$img(
                                     src = "20180510_penetrometer.jpg",
                                     height = 400,
                                     width = 400,
                                     align = "center"
                                 )),
                          
                          column(4,
                                 tags$img(
                                     src = "mustardseed2.jpg",
                                     height = 400,
                                     width = 500,
                                     align = "center"
                                 )),
                          
                          column(3,
                                 tags$img(
                                     src = "love-of-carrots.jpg",
                                     height = 400,
                                     width = 400,
                                     align = "center"
                                 ))
                                  )
                      ),
                  #--end tab
                 
                  ###--start tab2----
                   tabPanel(
                      "Bird's Eye View",
                      
                      fluidRow(
                          column(3,
                                 br(),
                                 sliderInput(
                                     "mydepth",
                                     "Select a Depth Range (cm):",
                                     min = 0,
                                     max = 30,
                                     step = 5,
                                     value = c(0, 10)
                                 ),
                                 br(),
                                 tags$img(
                                     src = "field-map-labelled.png",
                                     height = 400,
                                     width = 300,
                                     align = "center"
                                     
                                 )),
                          
                          column(9,
                                 plotOutput("plot1", height = "800px", )
                                 )
                           )
                           ),
                           #--end tab
                  
                  ###--start tab3----
                  tabPanel(
                      "Side Profile",
                      
                      fluidRow(
                          column(3,
                                 br(),
                                 selectizeInput('myf',
                                                'Select a Field:',
                                                dd_field,
                                                "All Fields"),
                                 br(),
                                 selectizeInput(
                                     'mylc',
                                     'Select Your Favorite LaCroix Flavor:',
                                     dd_lc,
                                     "PeachPear"
                                 ),
                                 br(),
                                 tags$img(
                                     src = "field-map-labelled.png",
                                     height = 400,
                                     width = 300,
                                     align = "center"
                                     
                                 )
                          ),
                          
                          column(9,
                                 plotOutput("plot2")
                          )
                      )
                  ),
                  #--end tab
                  
                  tabPanel("Mean Profiles",
                           
                           fluidRow(
                               column(3,
                                      br(),
                                      selectizeInput('myf2',
                                                     'Select a Field:',
                                                     dd_field,
                                                     "All Fields"),
                                      br(),
                                      selectizeInput(
                                          'mylc2',
                                          'Select Your Favorite LaCroix Flavor:',
                                          dd_lc,
                                          "PeachPear"
                                      ),
                                      br(),
                                      tags$img(
                                          src = "field-map-labelled.png",
                                          height = 400,
                                          width = 300,
                                          align = "center"
                                          
                                      )
                               ),
                               
                               column(9,
                                      plotOutput("plot3")
                               )
                           )
                  )
                  #--end tab
                           
              ))


server <- function(input, output) {
    
    ###--plot1----
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
                  legend.position = "left",
                  legend.title = element_text(size = rel(1.5)),
                  strip.text = element_text(size = rel(1.5)),
                  legend.text = element_text(size = rel(1.3)))
        
        
    })
    
    ###--plot2----
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
    
    ###--plot3----
    dataset3 <- reactive({
        
        if (input$myf2 == "All Fields") {
            dat} else {
                dat %>%
                    filter(field == input$myf2)
            }
        
    })
    
    
    
    output$plot3 <- renderPlotly({
        
        #p3 <- 
            dataset3() %>%
            group_by(depth_cm, field) %>% 
            summarise(resis_kpa = mean(resis_kpa, na.rm = T)) %>% 
            ungroup() %>% 
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
        
        #ggplotly(p3, tooltip = "text" )
        
        
        
    })
    
    
}

shinyApp(ui, server)