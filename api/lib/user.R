# User activation logic backing /activate_user.

mark_user_active <- function(conn_handler, user_name) {
  SigRepo::updateUser(conn_handler = conn_handler, user_name = user_name, active = TRUE)
}

send_user_activation_email <- function(user_name, api_key) {
  api_url <- base::sprintf(
    "https://montilab.bu.edu/SigRepo/send_notifications/activate_user?user_name=%s&api_key=%s",
    user_name,
    api_key
  )

  httr::GET(url = api_url)
}
