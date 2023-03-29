# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

if (nchar(Sys.getenv("R_CONFIG_ACTIVE"))==0) {
  Sys.setenv(R_CONFIG_ACTIVE = "testing") # Replace "default" with your config
}

pkgload::load_all()
options("golem.app.prod" = TRUE)

config <- config::get()
Sys.setenv(R_CONFIG_ACTIVE = "testing")

options(shiny.port = 8100)
stopadforms::run_app() # add parameters here (if any)