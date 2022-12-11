#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

fund_metadata <- readRDS("../../fund_metadata.rds")
fund_res <- readRDS("../../fund_res.rds")

select_all_label <- "_ALL"
stopifnot(all(purrr::map_lgl(dimnames(fund_res), ~ select_all_label %in% .x == FALSE)))

# Define a function to remove additional dimensions to target from an array
drop_dims <- function(data., target_dim = 2L) {
  if (length(dim(data.)) <= target_dim) {
    return(data.)
  }
  if (sum(dim(data.) > 1L) > target_dim) {
    stop("Too many dimensions that cannot be dropped:",
         "\nthe minimal number of dimensions for [data.] is: ", sum(dim(data.) > 1L),
         ", which is larger that target_dim = ", target_dim)
  }
  drop_dim_idx <- head(which(dim(data.) == 1L), n = length(dim(data.)) - target_dim)
  array(data., dim = dim(data.)[-drop_dim_idx], dimnames = dimnames(data.)[-drop_dim_idx])
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Fund viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    rlang::inject(
      sidebarPanel(
        !!!purrr::pmap(
          list(dn_name = names(dimnames(fund_res)),
               dn = dimnames(fund_res),
               dn_idx = seq_along(dimnames(fund_res))),
          function(dn_name, dn, dn_idx) {
            selectizeInput(
              dn_name,
              paste0("Select [", dn_name, "]:"),
              choices = c(select_all_label, dn),
              selected = if (dn_idx %in% c(1L, length(dim(fund_res))) == TRUE) select_all_label else dplyr::first(dn),
              multiple = FALSE
            )
          }
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Fund statistics",
          # Add line break before datatable
          htmlOutput("padding1"),
          DT::dataTableOutput("fundRes")
        ),
        tabPanel(
          "Fund metadata",
          # Add line break before datatable
          htmlOutput("padding2"),
          DT::dataTableOutput("fundMetadata")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$padding1 <- renderText("<br/>")
  output$padding2 <- renderText("<br/>")
  
  output$fundMetadata <- DT::renderDT(
    DT::datatable(
      fund_metadata %>%
        tibble::rownames_to_column(paste0("_", dplyr::first(names(dimnames(fund_res))))),
      options = list(
        dom = "lBfrtip",
        buttons = c('copy', 'csv', 'excel', 'pdf', 'selectNone'),
        pageLength = 25L,
        select = list(style = "multi+shift", item = "row")
      ),
      rownames = FALSE,
      filter = "top",
      selection = "none",
      extension = c("Buttons", "Select")
    ),
    server = FALSE
  )
  
  output$fundRes <- DT::renderDT(
    {
      # Drop unnecessary dimensions
      fund_res_subset <- tryCatch(
        drop_dims(
          # Subset fund_res array
          do.call(`[`,
                  c(list(fund_res),
                    purrr::map(names(dimnames(fund_res)),
                               ~ {
                                 if (input[[.x]] %in% select_all_label == TRUE) {
                                   metric_select <- dimnames(fund_res)[[.x]]
                                 } else {
                                   metric_select <- input[[.x]]
                                 }
                               }),
                    list(drop = FALSE))),
          target_dim = 2L
        ),
        error = function(err) {
          if (stringr::str_starts(conditionMessage(err),
                                  "Too many dimensions that cannot be dropped") == TRUE) {
            stop("Please select non-\"", select_all_label, "\" on at least ", length(dim(fund_res)) - 2L, " ",
                 if (length(dim(fund_res)) - 2L == 1L) "dimension" else "dimensions", "!")
          }
        }
      )
      
      DT::datatable(
        fund_res_subset %>%
          round(digits = 4L) %>%
          as.data.frame() %>%
          # Move row names to a column to enable filtration on row names
          tibble::rownames_to_column(
            { 
              col_name <- names(dimnames(fund_res_subset))[1L]
              if (is.null(col_name) == TRUE | nchar(col_name) == 0L) {
                " "
              } else {
                col_name
              }
            }
          ),
        options = list(
          dom = "lBfrtip",
          buttons = c('copy', 'csv', 'excel', 'pdf', 'selectNone'),
          pageLength = 25L,
          select = list(style = "multi+shift", item = "row")
        ),
        rownames = FALSE,
        filter = "top",
        selection = "none",
        extension = c("Buttons", "Select")
      )
    },
    server = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
