library(shiny)

shinyServer(function(input, output, session) {
  
  extractedText <- reactive({
    
    # Extraxt text that has been in an uploaded image
    
    # progress bar setup
    progress <- Progress$new(session, min=1, max=20)
    on.exit(progress$close())
    
    progress$set(
      message = 'OCR in progress', 
      detail = 'This may take 5-20 sec'
    )
    
    # if not empty ocr it. if empty use default image
    
    inFile = input$file1
    
    if (!is.null(inFile))
    {
      Extext <- ocr(inFile$datapath)
    }
    else
    {
      Extext <- ocr("www/receiptexample.jpg")
    }
    
    #show extracted text 
    
    Extext
  })
  
  
  
  
  
  output$intro <- renderUI({
    list(
      
      h4("This shiny app uses the tesseract ubuntu/  R package to perform OCR on an uploaded  image."),
      h4("There is a default receipt picture or you can upload your own!")
      
      )
    
  })
  
  output$IMAGE <- renderImage({
    
    #show uploaded image.if there isnt one show default image.
    
    inFile = input$file1
    print(inFile)
    if (!is.null(inFile))
    {
      
      width  <- session$clientData$output_IMAGE_width
      height <- session$clientData$output_IMAGE_height
      list(
        src = inFile$datapath,
        width=width,
        height=height
      )
    }
    else
    {
      list(src="www/receiptexample.jpg")
    }
  },
  deleteFile = FALSE
  )
  
  
  output$OCRtext = renderPrint({
    
    
    #show extracted OCR'ed text
    
    cat(extractedText())
  })
  

  
  

  
  
  
})
