output$download <- downloadHandler(
# filename <- "myReport.pdf",
#  filename <- "myReport.html",
  
  filename = function(){
    paste('myReport', sep = '.', switch(
                  input$format, HTML = 'html', PDF = 'pdf',  Word = 'docx'
                  )
             )
    },
  
  
  content <- function(file) {
    
    src <- normalizePath("report.RMD")
    #owd <- setwd("./")
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    
    file.copy(src, './report.RMD',overwrite = TRUE)
    library(rmarkdown)
#    out <- render("./report.Rmd",pdf_document()) ## <- file path to report goes here 
    out <- render("report.RMD",

                  switch(
                    input$format,
                    PDF = pdf_document(), HTML = html_document(), Word = word_document()
                  )
                  
                  
    ) ## <- file path to report goes here 
    
    file.rename(out, file)
  }
)