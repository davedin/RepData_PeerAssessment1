## this must be "sourced" before running kniting HTML
options(rstudio.markdownToHTML = 
            function(inputFile, outputFile) {      
                require(markdown)
                markdownToHTML(inputFile, outputFile, stylesheet='custom.css')   
            }
)