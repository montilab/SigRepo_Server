library(httr)
library(jsonlite)

res1 <- 
  httr::POST(
    url = "http://127.0.0.1:8020/store_difexp?api_key=1bcc4d9e4aa18d29098822d7a546241f&signature_hashkey=af7b21277e406e106fd2a42ef75267e2",
    body = list(
      difexp = httr::upload_file(file.path(base::system.file("inst/data/sample_types", package = "SigRepo"), "BRENDA_sample_types.rds"), "application/rds")
    )
  )

res1$status_code

res2 <- 
  httr::GET(
    url = "http://127.0.0.1:8020/get_difexp?api_key=1bcc4d9e4aa18d29098822d7a546241f&signature_hashkey=af7b21277e406e106fd2a42ef75267e2"
  )

res2$status_code

difexp <- fromJSON(fromJSON(rawToChar(res2$content)))


res3 <- 
  httr::DELETE(
    url ="http://127.0.0.1:8020/delete_difexp?api_key=1bcc4d9e4aa18d29098822d7a546241f&signature_hashkey=af7b21277e406e106fd2a42ef75267e2"
  )

res3$status_code
