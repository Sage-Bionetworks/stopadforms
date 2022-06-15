library(synapser)
library(rjson)
synLogin()

client_meta_data <- list(
  client_name='stopadforms',
  redirect_uris= list(
    'https://sagebio.shinyapps.io/stopadforms/'
  )
)

# Create the client:
client_meta_data<-synRestPOST('/oauth2/client', toJSON(client_meta_data), 
                              'https://repo-prod.prod.sagebase.org/auth/v1')

client_id <-  client_meta_data$client_id

# Generate and retrieve the client secret:
client_id_and_secret<-synRestPOST(paste0('/oauth2/client/secret/',client_id), 
                                  '', 'https://repo-prod.prod.sagebase.org/auth/v1')

print(client_id_and_secret)