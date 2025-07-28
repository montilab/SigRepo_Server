# signature_info_server.R
source("signature_info_module.R")

signature_info_server <- function(input, output, session) {
  query <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  sig_id <- reactive({ query()[["sig_id"]] })
  sig_name <- reactive({ query()[["sig_name"]] })
  conn_handler <- reactive({ user_conn_handler() })
  
  signatureInfoServer("sig_info", conn_handler, sig_id, sig_name)
}

