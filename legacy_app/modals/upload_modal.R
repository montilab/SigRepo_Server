# The upload dialog the Signature and Collection tabs open. Each tab observes
# its own upload_btn and adds the uploaded .rds itself.

upload_modal_ui <- function(ns, type = c("Collection", "Signature")) {
  modalDialog(
    title = paste("Upload", type),

    fileInput(ns("upload_file"), paste("Choose", type, "RDS file"), accept = ".rds"),

    radioButtons(
      inputId = ns("visibility"),
      label = "Set visibility:",
      choices = c("Private" = FALSE, "Public" = TRUE),
      selected = FALSE,
      inline = TRUE
    ),

    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("upload_btn"), paste("Upload", type), class = "btn-primary")
    )
  )
}
