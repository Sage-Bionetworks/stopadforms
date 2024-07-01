# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file


localDevelopment = interactive()

if (nchar(Sys.getenv("R_CONFIG_ACTIVE"))==0) {
  if(localDevelopment == TRUE) {
    Sys.setenv(R_CONFIG_ACTIVE = "testing")
    options(shiny.port = 8100)
  } else {
    Sys.setenv(R_CONFIG_ACTIVE = "default")
  }
}

pkgload::load_all()
options("golem.app.prod" = TRUE)
stopadforms::run_app() # add parameters here (if any)