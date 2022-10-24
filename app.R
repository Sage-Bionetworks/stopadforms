# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

Sys.setenv(R_CONFIG_ACTIVE = "default") # Replace "default" with your config
pkgload::load_all()
options("golem.app.prod" = TRUE)

# Map AWS Secrets Manager secrets, if any, to environment variables
secrets_manager_secrets_string<-Sys.getenv("SECRETS_MANAGER_SECRETS")
if (!is.null(secrets_manager_secrets_string) && nchar(secrets_manager_secrets_string)>0) {
	secrets_manager_secrets<-rjson::fromJSON(secrets_manager_secrets_string)
	lapply(names(secrets_manager_secrets), function(x){Sys.setenv(x=secrets_manager_secrets[x])})
}

# TODO remove the following two debug lines
print("Env vars in app.R")
print(Sys.getenv())

stopadforms::run_app() # add parameters here (if any)
