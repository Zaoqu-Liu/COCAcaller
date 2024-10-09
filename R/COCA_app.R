#' COCA_app
#'
#' A Shiny application to identify COCA Subtypes from miRNA Expression.
#' This function sets up and launches a Shiny app that allows users to input
#' miRNA feature files, predict COCA subtypes, and view the results.
#'
#' @title COCA Shiny Application
#' @description A Shiny app for identifying COCA subtypes based on miRNA expression data.
#' @details This Shiny application is designed to identify COCA subtypes from miRNA expression data.
#' The app checks and installs necessary packages, sets up the UI, and handles user input to predict
#' COCA subtypes using a pre-trained neural network model. The app includes a contact page for user
#' feedback and support, and an informative home page with a carousel and updates.
#'
#' It first checks and installs any necessary packages. Then, it sets up the user interface (UI)
#' and server logic. The UI consists of multiple pages, including a home page and a contact page.
#' The server logic handles user input, data validation, model prediction, and feedback form submission.
#' @return This function does not return a value. It launches a Shiny application.
#' @export
#' @author Zaoqu Liu; Email: liuzaoqu@163.com
COCA_app <- function() {
  # -----Packages-----
  message("[+] Checking dependencies...")
  options(BioC_mirror = "https://mirrors.tuna.tsinghua.edu.cn/bioconductor")
  options(repos = structure(c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")))

  message("[++] It will cost a while for the first time.")

  # Packages from CRAN and Bioconductor
  pkgs.to.check <- c(
    "shiny",
    "shinyjs",
    "bslib",
    "readr",
    "stringr",
    "slickR",
    "markdown",
    "DT",
    "shinyWidgets",
    "shinyFeedback",
    "neuralnet"
  )

  for (pkg.name in pkgs.to.check) {
    if (!requireNamespace(pkg.name, quietly = TRUE)) {
      message(paste("[++] Installing", pkg.name))
      tryCatch(
        install.packages(pkg.name, repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"),
        error = function(e) {
          message(paste("[++] ERROR: R package [", pkg.name, "] cannot be installed."))
        }
      )
    }
    library(pkg.name, character.only = TRUE)
  }

  # -----Setting up-----
  options(shiny.maxRequestSize = 1024 * 1024^2)
  message("[+] Starting...")

  # -----UI part-----
  # this script render the UI of help page
  set_md_path <- function(filename) {
    system.file("www", filename, package = "COCAcaller")
  }

  ui.page_help_privacy_policy <- function() {
    tabPanel(
      title = "Privacy Policy",
      value = "PrivacyPolicy",
      fluidPage(
        shiny::includeMarkdown(set_md_path("privacy_policy.md")),
        style = "width:80%;"
      )
    )
  }

  ui.page_help_terms_and_conditions <- function() {
    tabPanel(
      title = "Terms and Conditions",
      value = "TermsAndConditions",
      fluidPage(
        shiny::includeMarkdown(set_md_path("terms_and_conditions.md")),
        style = "width:80%;"
      )
    )
  }

  ui.page_contact <- function() {
    tabPanel(
      title = "Contact",
      value = "Contact",
      fluidPage(
        style = "width:80%;",
        tags$br(),
        fluidRow(
          column(
            6,
            tags$h2("Contact"),
            tags$p(
              "Feedback can be sent to liuzaoqu@163.com or by using the form below. We would like to receive feedback on how to improve this resource.",
              style = "font-size:100%"
            ),
            tags$hr(),
            tags$br(),
            fluidRow(
              column(
                6,
                icon("map-marker", style = "font-size:120%;"),
                tags$b("Address", style = "color:#000;"),
                tags$p("168 Changhai Road, Yangpu District, Shanghai, China",
                  style = "font-size:100%"
                ),
                icon("hospital-o", style = "font-size:120%;"),
                tags$b("Affiliation", style = "color:#000;"),
                tags$br(),
                tags$a(href = "https://www.chhospital.com.cn/html/", target = "_blank", "Shanghai Changhai Hospital"),
                tags$p(""),
                icon("envelope", style = "font-size:120%;"),
                tags$b("Email address", style = "color:#000;"),
                tags$br(),
                tags$a(href = "mailto:liuzaoqu@163.com", "liuzaoqu@163.com"),
                tags$p(""),
                icon("phone", style = "font-size:120%;"),
                tags$b("Phone number", style = "color:#000;"),
                tags$p("+86-18439932426", style = "font-size:100%"),
              ),
              column(
                6,
                # icon("wechat", style = "font-size:120%;"),
                # tags$b("WeChat Official Account", style = "color:#000;"),
                # tags$br(),
                # tags$img(src = "wechat_qr_code.png", height = "200px"),
                tags$p(""),
                icon("desktop", style = "font-size:120%;"),
                tags$b("Bilibili station", style = "color:#000;"),
                tags$br(),
                tags$a(href = "https://space.bilibili.com/375135306", target = "_blank", "Welcome to our video blog")
              )
            )
          ),
          column(1, tags$p(" ")),
          column(
            5,
            tags$h2("Comment and Feedback"),
            fluidRow(
              column(
                6,
                textInput(
                  "ContactPageNameInput",
                  label = "",
                  placeholder = "Name",
                  width = "100%"
                )
              ),
              column(
                6,
                textInput(
                  "ContactPageEmailInput",
                  label = "",
                  placeholder = "Email",
                  width = "100%"
                )
              )
            ),
            fluidRow(
              column(
                6,
                textInput(
                  "ContactPagePhoneInput",
                  label = "",
                  placeholder = "Institution",
                  width = "100%"
                )
              ),
              column(
                6,
                textInput(
                  "ContactPageSubjectInput",
                  label = "",
                  placeholder = "Subject",
                  width = "100%"
                )
              )
            ),
            textAreaInput(
              "ContactPageMsgInput",
              label = "",
              placeholder = "Message",
              width = "100%",
              rows = 7
            ),
            actionButton(
              "ContactPageSubmitBtn",
              label = "Submit",
              icon = icon("check"),
              class = "btn btn-success",
              style = "background-color:#0e8daa;"
            )
          )
        )
      )
    )
  }

  home.news.and.updates <- read_lines(set_md_path("home_news_and_updates.txt"))
  home.news.and.updates.html <- paste(home.news.and.updates, "</p>  <p>", collapse = " ")
  home.news.and.updates.html <- paste("<p>", home.news.and.updates.html)
  home.news.and.updates.html <- str_replace(home.news.and.updates.html, "</p>  <p>$", "</p>")
  home.news.and.updates.html <- str_remove_all(home.news.and.updates.html, "<p>\\s+</p>")

  ui.page_home <- function() {
    tabPanel(
      title = "Home",
      value = "Home",
      fluidPage(
        style = "width:80%;",
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = set_md_path("static/css/mystyle.css"))
        ),
        tags$br(),
        # div(
        #   img(src = "logo_single_small.png", height = 80, width = 85, style = "padding-top:0px;padding-bottom:0px;"),
        #   p("C O C A", style = "font-weight:bold;font-size:220%;color:#276e7f;padding-bottom:0px;margin-bottom:0px;font-family:\"Comic Sans MS\", \"Comic Sans\", cursive;"),
        #   tags$p("Identify COCA Subtype from miRNA Expression",
        #     style = "font-size:110%;color:#999;font-style:italic;padding-top:0px;margin-top:0px;"
        #   ),
        #   style = "text-align:center;"
        # ),
        fluidRow(
          column(
            12,
            # div(
            #   div(
            #     slickR(
            #       obj = home.carousel.images.url,
            #       height = 390,
            #       width = "95%"
            #     ) +
            #       settings(dots = FALSE, autoplay = FALSE)
            #   ),
            #   style = "height:450px;box-shadow:5px 5px 10px 5px #ccc;padding:20px 0px 20px 0px;border-radius:10px;"
            # )
          )
        ),
        tags$br(),
        tags$hr(),
        tags$p("Please input your miRNA feature file.",
          style = "font-size:120%;font-weight:bold;"
        ),
        tags$p("Note: Only one sample at a time. You can use the InputExample data
             to make adjustments",
          style = "font-size:90%; color:#777777"
        ),
        tags$br(),
        div(
          fluidRow(
            column(
              4,
              fluidRow(
                column(
                  7,
                  shiny::fileInput(
                    inputId = "inputFileBtn",
                    multiple = FALSE,
                    label = NULL,
                    accept = ".csv",
                    buttonLabel = "Select file"
                  )
                ),
                column(
                  5,
                  shiny::downloadButton(
                    outputId = "demoDownloadBtn",
                    label = "InputExample",
                    icon = NULL,
                    style = "border-width:0px;background-color:#fff;font-style:italic;text-decoration:underline;"
                  )
                )
              )
            ),
            column(
              2,
              shinyWidgets::actionBttn(
                inputId = "runBtn",
                label = "Run",
                icon = icon("rocket"),
                style = "pill",
                color = "success",
                block = TRUE
              )
            ),
            column(1),
            column(
              4,
              shiny::htmlOutput(
                outputId = "resultOutput"
              )
            )
          )
        ),
        tags$br(),
        tags$hr(),
        tags$br(),
        div(
          bs4Dash::box(
            div(
              HTML(home.news.and.updates.html),
              style = "height:170px;overflow-y:scroll;border:1px solid #cecece;padding:10px 20px 10px 20px;text-align:justify;"
            ),
            id = "HomePageLastInfoCard",
            title = "News and updates",
            solidHeader = FALSE,
            height = "200px",
            closable = FALSE,
            maximizable = FALSE,
            width = 12,
            collapsible = FALSE,
            icon = icon("dove")
          ),
          style = "box-shadow:2px 2px 5px 2px #ccc;"
        ),
        tags$br(),
        tags$br(),
        tags$br()
      )
    )
  }

  ## -----Set up UI-----
  ui <- shinyUI(
    fluidPage(
      tags$head(
        tags$title("COCA"),
        tags$link(rel = "stylesheet", type = "text/css", href = set_md_path("static/css/mystyle.css")),
        tags$link(rel = "shortcut icon", href = "logo.ico")
      ),
      shinyjs::useShinyjs(),
      bslib::page_navbar(
        id = "navbar",
        bg = "#00b4d8",
        !!!list(
          nav_spacer(),
          nav("Home", ui.page_home(), icon = icon("home")),
          nav("Contact", ui.page_contact(), icon = icon("paper-plane")),
          nav(title = NULL, value = "PrivacyPolicy", ui.page_help_privacy_policy()),
          nav(title = NULL, value = "TermsAndConditions", ui.page_help_terms_and_conditions())
        ),
        collapsible = TRUE,
        theme = bs_theme(bootswatch = "yeti")
      )
    )
  )

  # -----Server part-----
  server <- function(input, output, session) {
    load(set_md_path("mlpcla.rda"))

    data.input <- reactive({
      req(input$inputFileBtn, cancelOutput = FALSE)
      read.csv(input$inputFileBtn$datapath)
    })

    observeEvent(input$runBtn, {
      d <- data.input()

      if (ncol(d) > 2 & colnames(d)[1] != "Feature" & colnames(d)[2] != "Value") {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error",
          type = "error",
          text = "The data you entered does not meet the format, please refer to the InputExample data!",
          closeOnClickOutside = TRUE,
          showCloseButton = TRUE
        )
        return()
      }
      if (sum(d$Feature %in% mlpcla[["model.list"]][["variables"]]) < 12) {
        shinyWidgets::sendSweetAlert(
          session = session,
          title = "Error",
          type = "error",
          text = "The data you entered does not fit the model!",
          closeOnClickOutside = TRUE,
          showCloseButton = TRUE
        )
        return()
      }

      d <- d[d$Feature %in% mlpcla[["model.list"]][["variables"]], ]
      d2 <- as.data.frame(matrix(d$Value, nrow = 1))
      colnames(d2) <- d$Feature
      d2 <- d2[, mlpcla[["model.list"]][["variables"]]]
      mlppre <- predict(mlpcla, d2)
      mlpprelab <- apply(mlppre, 1, which.max)

      output$resultOutput <- renderUI({
        msg1 <- paste0(
          "Probability:\nNormal = ", round(mlppre[1], 3),
          "; COCA1 = ", round(mlppre[2], 3),
          "; COCA2 = ", round(mlppre[3], 3),
          "; COCA3 = ", round(mlppre[4], 3)
        )
        msg2 <- c("Normal", "COCA1", "COCA2", "COCA3")[mlpprelab]

        div(
          p(msg1, style = "font-size:90%;"),
          span("Hence, this sample was identified as ",
            style = "color:#000;font-size:100%;font-weight:bold;"
          ),
          if (mlpprelab == 1) {
            span(msg2, style = "font-size:120%;font-weight:bold;background-color:#119da4;color:#FFF")
          } else if (mlpprelab == 2) {
            span(msg2, style = "font-size:120%;font-weight:bold;background-color:#FF6666;color:#FFF")
          } else {
            span(msg2, style = "font-size:120%;font-weight:bold;background-color:#ffc857;color:#FFF")
          }
        )
      })
    })

    output$demoDownloadBtn <- downloadHandler(
      filename = function() {
        "InputExample.csv"
      },
      content = function(file) {
        data <- read.csv(set_md_path("InputExample.csv"))
        write.csv(data, file = file, row.names = FALSE)
      },
      contentType = ".csv"
    )

    user.name <- reactive(
      req(input$ContactPageNameInput, cancelOutput = TRUE)
    )
    user.email <- reactive(
      req(input$ContactPageEmailInput, cancelOutput = TRUE)
    )
    user.phone <- reactive(
      req(input$ContactPagePhoneInput, cancelOutput = TRUE)
    )
    user.subject <- reactive(
      req(input$ContactPageSubjectInput, cancelOutput = TRUE)
    )
    user.msg <- reactive(
      req(input$ContactPageMsgInput, cancelOutput = TRUE)
    )

    observeEvent(input$ContactPageSubmitBtn, {
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("ContactPageNameInput", shinyvalidate::sv_required())
      iv$add_rule("ContactPageEmailInput", shinyvalidate::sv_email())
      iv$add_rule("ContactPagePhoneInput", shinyvalidate::sv_required())
      iv$add_rule("ContactPageSubjectInput", shinyvalidate::sv_required())
      iv$add_rule("ContactPageMsgInput", shinyvalidate::sv_required())
      iv$enable()

      user.contact.db <- openxlsx::read.xlsx(set_md_path("contact_user_message.xlsx"), sheet = 1)

      tmp <- data.frame(
        user.name = user.name(),
        user.email = user.email(),
        user.phone = user.phone(),
        user.subject = user.subject(),
        user.msg = user.msg(),
        date = Sys.Date()
      )
      user.contact.db <- rbind(user.contact.db, tmp)

      write.xlsx(user.contact.db, file = set_md_path("contact_user_message.xlsx"))

      if (nrow(user.contact.db) %% 10 == 0) {
        receivers <- openxlsx::read.xlsx(set_md_path("maintainer_email.xlsx"), sheet = 1, colNames = FALSE)[, 1]

        source(set_md_path("lib/global.R"))
        sendEmail(receivers)
      }

      msg.success <- modalDialog(
        title = "Message received",
        tags$p("We value every advice. Thank you for helping us to improve the app."),
        easyClose = TRUE,
        footer = actionButton("ContactPageMsgModal",
          label = "OK",
          icon = icon("check"),
          class = "btn btn-success",
          style = "background-color:#0e8daa"
        )
      )
      showModal(msg.success)
    })

    observeEvent(
      {
        input$ContactPageMsgModal
      },
      {
        removeModal()
      }
    )

    message("[+] Shiny app run successfully! Enjoy it!\n")
  }

  shiny::shinyApp(
    ui = ui,
    server = server
  )
}
