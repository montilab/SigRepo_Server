

output$search_inputs <- renderUI({
  
  req(input$search_options)
  
  options <- isolate({ input$search_options })

  if(length(options) == 1){
    
    input_options <- textAreaInput(
      inputId = options,
      label = paste0(options[p], ":"),
      width = "100%",
      height = "30px"
    ) %>% as.character()
    
  }else{
    
    input_options <- seq_along(options) %>% 
      map_chr(
        function(p){
          #p=1;
          if(p < length(options)){
            
            text_area <- textInput(
              inputId = options[p],
              label = paste0(options[p], ":"),
              width = "100%"
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
            
            text_area <- textInput(
              inputId = options[p],
              label = paste0(options[p], ":"),
              width = "100%",
            ) %>% as.character()
            
          }
        }
      )
  }
  
  HTML(input_options)
  
})

# Create reactive values 
search_sig_error_msg <- reactiveVal()
search_signature_tbl <- reactiveVal()

observeEvent(input$search_signature, {
  
  options <- isolate({ input$search_options })
  
  if(length(options) == 0){
    
    search_sig_error_msg("Please select one of the search options above.")
    return(NULL)
    
  }else{
    
    seq_along(options) %>% 
      purrr::walk(
        function(p){
          #p=1;
          input_value <- input[[options[p]]]
          if(input_value == ""){
            search_sig_error_msg(sprintf("%s cannot be empty.", options[p]))
            return(NULL)
          }
        }
      )  
    
    search_signature_tbl(mtcars)
    
  }
  
  # tbl <- lookup_multiple_id_sql(
  #   conn, 
  #   table = "signatures", 
  #   id_var = c(options), 
  #   coln_var = , 
  #   coln_val
  # )

  
})
  
output$search_sig_error_msg <- renderUI({
  
  req(search_sig_error_msg())
  
  p(class="primary", search_sig_error_msg())
  
})


output$signature_table <- DT::renderDataTable({
  
  req(search_signature_tbl())
  
})
