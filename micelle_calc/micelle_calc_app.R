#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application
ui <- navbarPage("Micelle/Vesicle property Calculator", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
               tabPanel("Micelles, a and N",
                        fluidPage(
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput("mic_type", 
                                                label = "Choose micelle type",
                                                c("Spherical" = 3,
                                                  "Cylindrical" = 2,
                                                  "Planar" = 1), selected = 'Spherical'),
                                    sliderInput("chain_len", 
                                                label = "Length of aliphatic C chain:",
                                                min = 10, max = 30, value = 10)
                                    ), #end sidebarpanel
                                
                                mainPanel(
                                    p("This app will allow you to calculate the properties of micelles and/or vesicles"),
                                    textOutput('a_sele'),
                                    p(""),
                                    textOutput('c_chain_sele'),
                                    p(""),
                                    textOutput('a_calc'),
                                    p(""),
                                    p("The maximum aggregation number assuming max chain length:"),
                                    textOutput('N_calc')
                                ) #end mainpanel
                            ) # end sidebarlayout
                        ) #end fluidpage
                    ), # end tabpanel
               tabPanel("Calculate Free energy of bending",
                        fluidPage(
                            sidebarLayout(
                                sidebarPanel(
                                    sliderInput("epsilon",
                                                 label = "Bilayer thickness, Epsilon (Å)",
                                                 min = 1, max = 100, value = 20 ),
                                    numericInput("Kc",
                                                  label = "Value of Kc",
                                                  min = 0, max = 10, value =  2e-20),
                                    numericInput("Kc_hat",
                                                  label = "Value of Kc Hat",
                                                  min = -10, max = 10, value =  -5e-21),
                                         ), #end sidebarpanel
                                         mainPanel(
                                             p("To calculate the free energy of bending")
                                         ) #end mainpanel
                                     ) #end sidebarlayout
                            ) #end fluidpage
               ), # end tabpanel
                            
               tabPanel("About",
                p("Written by Sebastian Moes")
               ) # end tabpanel
) # end navbarpage

# Define server logic required
server <- function(input, output) {
    pack_names <- c("Planar","Cylindrical","Spherical")
    vals <- reactiveValues() #initialize values
    observe({
        vals$v <- (27.7 + 26.9 * input$chain_len)
        vals$l <- (1.5 + 1.27 * input$chain_len)
        vals$pack <- as.integer(input$mic_type)
        vals$pack_name <- pack_names[vals$pack]
    }) #input values
    
    output$a_sele <- renderText({ 
        paste("You have selected ", vals$pack_name)
    })
    
    output$c_chain_sele <- renderText({ 
        paste("You have chosen a carbon chain of length ",
              input$chain_len)
    })

    output$a_calc <- renderText({
        paste("The result is =", round(vals$pack*vals$v/vals$l, digits = 2))
    })
    output$N_calc <- renderText({
        paste("The  max aggregation number is =", round( (4 * pi * vals$l^2)/(vals$pack*vals$v/vals$l),digits = 2))
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
