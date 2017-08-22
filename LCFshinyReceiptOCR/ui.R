

#check that the dependencies exist

library(shiny)
library(jpeg)
library(tesseract)


shinyUI(fluidPage(
  
  # Application title
  titlePanel("LCF Project   :  Scanned receipts  :  OCR a receipt with the tesseract R/Ubuntu package"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
      
      fileInput('file1', 'Choose an image (max 5MB)'),
      tags$hr()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Introduction",
          htmlOutput("intro")
        ),
        tabPanel(
          "Image & extracted text",
          fluidRow(
            column(
              width=7,
              imageOutput("IMAGE")
            ),
            column(
              width=5,
              verbatimTextOutput("OCRtext")
            )
          )
        )
      )
    )
  )
))
