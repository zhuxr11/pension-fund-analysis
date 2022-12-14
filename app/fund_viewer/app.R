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

# Define a function to load RDS file locally or from website
load_local_or_web_file <- function(file_name) {
  stopifnot(stringr::str_ends(file_name, stringr::regex("\\.rds", ignore_case = TRUE)) == TRUE)
  if (file.exists(file.path("..", "..", file_name)) == TRUE) {
    file_conn <- file(file.path("..", "..", file_name))
  } else {
    file_conn <- url(paste0("https://raw.githubusercontent.com/zhuxr11/pension-fund-analysis/master/", file_name))
  }
  on.exit(close(file_conn), add = TRUE)
  readRDS(file_conn)
}

fund_metadata <- load_local_or_web_file("fund_metadata.rds")
fund_res <- load_local_or_web_file("fund_res.rds")

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
  
  # Enable Font Awesome icons
  includeScript("https://kit.fontawesome.com/6ecbd6c532.js"),
  
  # Add script to open new window
  includeScript("open_url_from_shiny.js"),
  
  # Application title
  titlePanel('Fund Viewer'),
  
  # Sidebar layout
  sidebarLayout(
    do.call(
      sidebarPanel,
      c(
        purrr::map(
          names(dimnames(fund_res)),
          # Use placeholders to update later
          ~ selectizeInput(
            .x,
            label = "",
            choices = NULL,
            selected = NULL,
            multiple = FALSE
          )
        ),
        list(htmlOutput("update_on"),
             htmlOutput("git_repo"))
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
server <- function(input, output, session) {
  
  # Show disclosure modal dialog
  disclosure_modal <- modalDialog(
    HTML(paste(
      "欢迎使用Fund Viewer！在使用前请查看关于风险和责任的披露事项。",
      "Welcome to Fund viewer! Before use, please view the disclosure on risk and liability.",
      "",
      "您同意所述披露事项并希望继续使用吗？",
      "Would you like to continue by showing your consent with the disclosure?",
      sep = "<br/>"
    )),
    title = "欢迎使用Fund Viewer (Welcome to Fund Viewer)",
    footer = tagList(
      actionButton("agree_disclosure", "是 (Yes)"),
      actionButton("view_disclosure", "查看披露事项 (View disclosure)"),
      actionButton("disagree_disclosure", "否 (No)")
    )
  )
  showModal(disclosure_modal)
  observeEvent(
    input$view_disclosure,
    session$sendCustomMessage(
      type = "open-url-new-tab",
      message = "https://github.com/zhuxr11/pension-fund-analysis#披露事项-disclosure"
    )
  )
  observeEvent(
    input$disagree_disclosure,
    stopApp()
  )
  observeEvent(
    input$agree_disclosure,
    {
      removeModal(session)
      
      # Initialize table spacing, update time and instructions
      output$padding1 <- renderText("<br/>")
      output$padding2 <- renderText("<br/>")
      output$update_on <- renderText("Last updated on: <b> 2022-12-03 </b> <br/> <br/>")
      output$git_repo <- renderText(as.character(
        tags$a(
          tags$i(class = "fab fa-github"),
          "查看使用说明 (View instructions)",
          href = "https://github.com/zhuxr11/pension-fund-analysis#fund-viewer应用程序-fund-viewer-application",
          target = "_blank"
        )
      ))
      
      # Initialize dimension choices selections to derive initial view on fund statistics
      purrr::pwalk(
        list(dn_name = names(dimnames(fund_res)),
             dn = dimnames(fund_res),
             dn_idx = seq_along(dimnames(fund_res))),
        function(dn_name, dn, dn_idx) {
          updateSelectizeInput(
            session,
            dn_name,
            label = paste0("Select [", dn_name, "]:"),
            choices = c(select_all_label, dn),
            selected = if (dn_idx %in% c(1L, length(dim(fund_res))) == TRUE) select_all_label else dn[1L]
          )
        }
      )
    }
  )

  # Update fund statistics
  output$fundMetadata <- DT::renderDT(
    if (input$agree_disclosure > 0L) {
      DT::datatable(
        fund_metadata %>%
          tibble::rownames_to_column("_FUND"),
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
    server = FALSE
  )
  
  # Update fund metadata
  output$fundRes <- DT::renderDT(
    {
      # Drop unnecessary dimensions
      fund_res_subset <- tryCatch(
        drop_dims(
          # Subset fund_res array
          do.call(
            `[`,
            c(list(fund_res),
              purrr::map(
                names(dimnames(fund_res)),
                ~ {
                  if (input[[.x]] %in% select_all_label == TRUE) {
                    metric_select <- dimnames(fund_res)[[.x]]
                  } else {
                    metric_select <- input[[.x]]
                  }
                }
              ),
              list(drop = FALSE))
          ),
          target_dim = 2L
        ),
        error = function(err) {
          if (stringr::str_starts(conditionMessage(err),
                                  "Too many dimensions that cannot be dropped") == TRUE) {
            NULL
          }
        }
      )
      
      if (is.null(fund_res_subset) == FALSE && input$agree_disclosure > 0L) {
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
      } else {
        NULL
      }
    },
    server = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
