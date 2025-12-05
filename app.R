library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)
library(thematic)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
      bootswatch = "minty"
    ),

    titlePanel("Starwars"),
    h1("Star Wars Characters"),
    sidebarLayout(
        sidebarPanel(
          actionButton(
            inputId = "boutton",
            label = "clique moi"
          ),
            sliderInput(inputId = "taille",
                        label = "Height of characters",
                        min = 0,
                        max = 250,
                        value = 30),
        selectInput( inputId = "gender_input",
          label = "Gender of character",
          choices = c("masculine", "feminine")),
        ),

        mainPanel(
           plotOutput(outputId = "StarWarsPlot"),
           textOutput(outputId = "textestarwars"),
           DTOutput(outputId = "tableau")
           
        )
    )
)


server <- function(input, output, session) {
  
rv <- reactiveValues()

  observeEvent(input$boutton, {
    showNotification(
      "La valeur du slider a changé !",
      type = "message"
    ) 
    rv$df_filter <- starwars %>%  
      filter(height > input$taille & gender == input$gender_input)  
    
    rv$plot <- rv$df_filter |>
      ggplot(aes(x = height)) +
      geom_histogram(
        binwidth = 10, 
        fill = "darkgray", 
        color = "white"
      ) + 
      labs(
        title=paste("Gender Choice :", input$gender_input))
    
  })
    
    output$StarWarsPlot <- renderPlot({ 
      rv$plot
      })
    
    output$textestarwars<-renderText({
    nombre_ligne <- rv$df_filter |>
          nrow()
        paste("Nb lignes :", nombre_ligne)
          })
    
    output$tableau <- renderDT({
      rv$df_filter
    })
}



shinyApp(ui = ui, server = server)
