library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)

# Sample dummy data
set.seed(123)
signatures_df <- data.frame(
  Signature = paste0("Sig", sample(1:50, 200, replace = TRUE)),
  Organism = sample(c("Human", "Mouse", "Rat"), 200, replace = TRUE),
  OmicsType = sample(c("Genomics", "Proteomics", "Transcriptomics"), 200, replace = TRUE),
  Platform = sample(c("Illumina", "Nanopore", "PacBio"), 200, replace = TRUE)
)


  titlePanel("Signature Distribution Dashboard")
  fluidRow(
    column(width = 6,  # Left panel
           div(
             style = "background-color: white; padding: 20px; border: 0.5px solid #ccc;",
             
             tabsetPanel(
               id = "group_by",
               tabPanel("None", plotOutput("signaturePie", height = "300px")),
               tabPanel("Organism", plotOutput("signaturePie", height = "300px")),
               tabPanel("OmicsType", plotOutput("signaturePie", height = "300px")),
               tabPanel("Platform", plotOutput("signaturePie", height = "300px"))
             )
           )
    ),
    
    column(width = 6,  # Right panel
           div(
             style = "background-color: white; padding: 20px; border: 0.5px solid #ccc;",
             
             h4("Second Panel"),
             p("This is the content of the second panel."),
             tableOutput("summaryTable")
           )
    )
  )
  