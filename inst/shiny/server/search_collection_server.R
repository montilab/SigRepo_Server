

output$search_inputs <- renderUI({
  
  req(input$search_options)
  
  options <- isolate({ input$search_options })

  if(length(options) == 1){
    
    input_options <- textAreaInput(
      inputId = options,
      label = stringr::str_to_title(options),
      width = "100%",
      height = "30px"
    ) %>% as.character()
    
  }else{
    
    input_options <- seq_along(options) %>% 
      map_chr(
        function(p){
          #p=1;
          if(p < length(options)){
            
            text_area <- textAreaInput(
              inputId = options[p],
              label = stringr::str_to_title(options[p]),
              width = "100%",
              height = "30px"
            )
            
            filter <- radioButtons(
              inputId = paste0(options[p], "_filter"),
              label = NULL,
              choices = c("AND", "OR"),
              width = "100%",
              inline = TRUE
            )
            
            paste0(text_area, filter, sep="\n")
            
          }else{
            
            text_area <- textAreaInput(
              inputId = options[p],
              label = stringr::str_to_title(options[p]),
              width = "100%",
              height = "30px"
            ) %>% as.character()
            
          }
        }
      )
  }
  
  HTML(input_options)
  
})


output$collection_table <- DT::renderDataTable({
  
  mtcars
  
})
