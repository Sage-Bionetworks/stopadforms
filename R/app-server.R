#' @import shiny
app_server <- function(input, output, session) {
  synapse <- reticulate::import("synapseclient")
  session$sendCustomMessage(type = "readCookie", message = list())
  syn <- synapse$Synapse()

  observeEvent(input$cookie, {
    is_logged_in <- FALSE
    # If there's no session token, prompt user to log in
    if (input$cookie == "unauthorized") {
      waiter::waiter_update(
        html = tagList(
          img(src = "www/synapse_logo.png", height = "120px"),
          h3("Looks like you're not logged in!"),
          span("Please ", a("login", href = "https://www.synapse.org/#!LoginPlace:0", target = "_blank"),
               " to Synapse, then refresh this page.")
        )
      )
    } else {
      ### login and update session; otherwise, notify to login to Synapse first
      tryCatch({
        syn$login(sessionToken = input$cookie, rememberMe = FALSE)
        is_logged_in <- TRUE

        ## Check if user is in STOP-AD_Reviewers team
        team <- "3403721"
        user <- syn$getUserProfile()
        memb <- dccvalidator::check_team_membership(
          teams = team,
          user = user,
          syn = syn
        )

        if (inherits(memb, "check_fail")) {
          waiter::waiter_update(
            html = tagList(
              img(src = "www/synapse_logo.png", height = "120px"),
              p(memb$behavior),
              p("You can request to be added at: "),
              HTML(glue::glue("<a href=\"https://www.synapse.org/#!Team:{team}\">https://www.synapse.org/#!Team:{team}</a>"))
            )
          )
        } else {
          ### update waiter loading screen once login successful
          waiter::waiter_update(
            html = tagList(
              img(src = "www/synapse_logo.png", height = "120px"),
              h3(sprintf("Welcome, %s!", syn$getUserProfile()$userName))
            )
          )
          Sys.sleep(2)
          waiter::waiter_hide()
        }
      }, error = function(err) {
        Sys.sleep(2)
        waiter::waiter_update(
          html = tagList(
            img(src = "www/synapse_logo.png", height = "120px"),
            h3("Login error"),
            span(
              "There was an error with the login process. Please refresh your Synapse session by logging out of and back in to",
              a("Synapse", href = "https://www.synapse.org/", target = "_blank"),
              ", then refresh this page. If the problem persists, contact an administrator."
            )
          )
        )
      })
    }
    req(is_logged_in)

    if (inherits(memb, "check_pass")) {

      ## Get data
      sub_data <- get_submissions(
        syn,
        group = 9,
        statuses = "SUBMITTED_WAITING_FOR_REVIEW"
      )
      sub_data <- process_submissions(sub_data, lookup_table)

      ## Show submission data
      callModule(
        mod_review_section_server,
        "review_section",
        synapse = synapse,
        syn = syn,
        user = user,
        submissions = sub_data,
        reviews_table = "syn22014561"
      )

      callModule(
        mod_panel_section_server,
        "panel_section",
        synapse = synapse,
        syn = syn,
        user = user,
        submissions = sub_data,
        reviews_table = "syn22014561",
        submissions_table = "syn22213241"
      )

      callModule(
        mod_view_all_section_server,
        "view_all_section",
        synapse = synapse,
        syn = syn,
        group = 9,
        lookup_table = lookup_table
      )
    }
  })
}
