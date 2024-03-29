#' @import shiny
app_server <- function(input, output, session) {
  
  ## Synapse client for a specific user
  syn <- synapse$Synapse()
  ## Oauth
  syn <- callModule(
    mod_synapse_oauth_server,
    "oauth",
    syn = syn
  )
  
  shiny::req(
    inherits(syn, "synapseclient.client.Synapse"),
    logged_in(syn)
  )
  
  callModule(
    mod_main_server,
    "main",
    syn = syn
  )
}
