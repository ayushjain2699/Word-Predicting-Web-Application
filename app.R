library(shiny)
library(shinydashboard)
ui <- dashboardPage(
    dashboardHeader(
        title = "WordPredictR"
    ),
    dashboardSidebar(),
    dashboardBody(
        fluidPage(
            textInput("text","Enter Your Text Here")
        ),
        fluidPage(
            box(
                solidHeader = TRUE,
                title = "Prediction1",
               textOutput(outputId = "pred1")
            ),
            box(
                solidHeader = TRUE,
                title = "Prediction2",
                textOutput(outputId = "pred2")
            ),
            box(
                solidHeader = TRUE,
                title = "Prediction3",
                textOutput(outputId = "pred3")
            ),
            box(
                solidHeader = TRUE,
                title = "Prediction4",
                textOutput(outputId = "pred4")
            ),
            box(
                solidHeader = TRUE,
                title = "Prediction5",
                textOutput(outputId = "pred5")
            )
        )
    )
)

server <- function(input,output){
    source("model.R")
    prediction = reactive({
        predictions(input$text)
    })
    output$pred1 = renderText({
        if((input$text==""))
        {
            "Enter the text first"
        }
        else prediction()[1]
    })
    output$pred2 = renderText({
        if((input$text==""))
        {
            "Enter the text first"
        }
        else prediction()[2]
    })
    output$pred3 = renderText({
        if((input$text==""))
        {
            "Enter the text first"
        }
        else prediction()[3]
    })
    output$pred4 = renderText({
        if((input$text==""))
        {
            "Enter the text first"
        }
        else prediction()[4]
    })
    output$pred5 = renderText({
        if((input$text==""))
        {
            "Enter the text first"
        }
        else prediction()[5]
    })
}

shinyApp(ui = ui,server = server)