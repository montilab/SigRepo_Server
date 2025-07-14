
# Function to generate random password
randPassword <- function(n = 1){
  a <- base::do.call(base::paste0, base::replicate(5, base::sample(LETTERS, n, TRUE), FALSE))
  base::paste0(a, base::sprintf("%04d", base::sample(9999, n, TRUE)), base::sample(LETTERS, n, TRUE))
}

# Function to send registered users to admin
registerUser <- function(
    from_sender = "sigrepo@bu.edu",
    user_name = "rchau88",
    user_email = "rchau88@bu.edu",
    user_first = "Reina",
    user_last = "Chau",
    user_affiliation = "Boston University",
    api_key = "dienfkdingnkgggiidndkkdidingnn"
){
  
  msg <- sendmailR::mime_part(
    base::paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>REGISTER USER</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>SigRepo Admin,</strong></p>',
      '<br>',
      '<p>A user has registered to gain access to the <strong>SigRepo</strong> database.</p>',
      '<p>Username: <strong>', user_name, '</strong></p>',
      '<p>Email: <strong>', user_email, '</strong></p>',
      '<p>First name: <strong>', user_first, '</strong></p>',
      '<p>Last name: <strong>', user_last, '</strong></p>',
      '<p>Affiliation: <strong>', user_affiliation, '</strong></p>',
      '<br>',
      '<p>To give access to this user, please click on the API link below to activate the user!</p>',
      '<p><strong>https://sigrepo.org/api/activate_user/?api_key=', api_key, '&user_name=', user_name, '</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>'
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- base::paste0("\"Montilab Team\"<", from_sender, ">")
  to <- base::paste0("<", user_email, ">")
  subject <- "SigRepo Register User Do Not Reply"
  msg <- base::list(msg)
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = msg, control = base::list(smtpServer = "smtp.bu.edu", smtpPort = "25"))
  
}

# Function to send email to registered users
notifyUser <- function(
    from_sender = "sigrepo@bu.edu",
    user_name = "rchau88",
    user_email = "rchau88@bu.edu",
    user_first = "Reina",
    user_last = "Chau",
    user_affiliation = "Boston University"
){
  
  msg <- sendmailR::mime_part(
    base::paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>SIGREPO</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>', user_name, ',</strong></p>',
      '<br>',
      '<p>Thank you for signing up to use our <strong>SigRepo</strong> database! Our administrator will contact you as soon as they had reviewed and activated our account.</p>',
      '<br>',
      '<p>Below is the registration information that you submitted:</p>',
      '<p>Username: <strong>', user_name, '</strong></p>',
      '<p>Email: <strong>', user_email, '</strong></p>',
      '<p>First name: <strong>', user_first, '</strong></p>',
      '<p>Last name: <strong>', user_last, '</strong></p>',
      '<p>Affiliation: <strong>', user_affiliation, '</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>'
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- base::paste0("\"Montilab Team\"<", from_sender, ">")
  to <- base::paste0("<", user_email, ">")
  subject <- "SigRepo Account Creation Do Not Reply"
  msg <- base::list(msg)
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = msg, control = base::list(smtpServer = "smtp.bu.edu", smtpPort = "25"))
  
}

# Function to send temporary password to user
sendPassword <- function(
    from_sender = "sigrepo@bu.edu",
    user_name = "rchau88",
    temp_password = "123456789",
    user_email = "rchau88@bu.edu"
){
  
  msg <- sendmailR::mime_part(
    base::paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>SIGREPO</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>', user_name, ',</strong></p>',
      '<br>',
      '<p>Below is a temporary password for accessing your account on the <strong>SigRepo</strong> database.</p>',
      '<br>',
      '<p>Username: <strong>', user_name, '</strong></p>',
      '<p>Temporary password: <strong>', temp_password, '</strong></p>',
      '<br>',
      '<p>To log back in? Follow this link, <strong>https://sigrepo.org/</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>'
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- base::paste0("\"Montilab Team\"<", from_sender, ">")
  to <- base::paste0("<", user_email, ">")
  subject <- "SigRepo Temporary Password Do Not Reply"
  msg <- base::list(msg)
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = msg, control = base::list(smtpServer = "smtp.bu.edu", smtpPort = "25"))
  
}
