# authentication uses a gmail account newzealandborderriskapp@gmail.com
# The OAuthKey to act on behalf of this user MAY BE integrated into the script in this account and may be uploaded to github.
# as of 13/8/2020 it is not integrated but this could happen.
# the public therefore access to this account. ANYONE IN THE WORLD could act in Google Sheets and Googld Drive on behalf of newzealandborderriskapp@gmail.com.
# thus it should be considered INSECURE.
#see https://googlesheets4.tidyverse.org/reference/gs4_auth_configure.html
#https://cran.r-project.org/web/packages/gargle/vignettes/non-interactive-auth.html
source("git_exclude/auth_connect.R")

#I tried the below:
options(gargle_oauth_email = "newzealandborderriskapp@gmail.com")
options(gargle_oauth_email = TRUE)
manual_corrections<-read_sheet("1hkpfinHpxT1KcTI8umh55aaiFug12jKKSMZoae4ttlA")
#this works at least locally, but hit a rate limit very quickly!

##TWO 

#this users says you need to use an oauth app:
#https://github.com/tidyverse/googlesheets4/issues/139
library(googlesheets4)
google_app <- httr::oauth_app(
  "Desktop-client-1-Border-Risk-App",
  key = "141372699862-cnn2d95dnn44gkuu64qnbm6mb4kb3v92.apps.googleusercontent.com",
  secret = borderapp_google_auth_secret,
)
api_key <- "AIzaSyAnEAdoH-yLBO1rvhmAD-kkKR9TMYqI0Rs"

gs4_auth_configure(app = google_app, api_key = api_key)
gs4_oauth_app()
gs4_api_key()

manual_corrections<-read_sheet("1hkpfinHpxT1KcTI8umh55aaiFug12jKKSMZoae4ttlA")

#Service account token
#https://gargle.r-lib.org/articles/get-api-credentials.html

library(googlesheets4)
manual_corrections<-read_sheet("1hkpfinHpxT1KcTI8umh55aaiFug12jKKSMZoae4ttlA")
gargle::token_fetch()


#
#https://gargle.r-lib.org/articles/articles/managing-tokens-securely.html
usethis::use_package("sodium", "Suggests") 
pw_name <- gargle:::secret_pw_name("gargle")
pw <- gargle:::secret_pw_gen()

gargle:::secret_write(
  package = "gargle",
  name = "gargle-testing.json",
  input = "/Users/benjaminsmith/Google Drive/politics/coronavirus/border-risk-app-service-account-credentials.json"
)




#############
#https://googlesheets4.tidyverse.org/reference/sheets_auth_configure.html

#TWO OPTIONS:




##CURRENT BAD METHOD:


options(gargle_oauth_email = "newzealandborderriskapp@gmail.com")
options(gargle_oauth_email = TRUE)
gs4_deauth()
#let's try how many times this works....
for (i in 1:200){
  print(paste0(i, "; " , now()))
  
  #set this for now, but we may need to follow the instructions below:
  manual_corrections <- read_sheet("1hkpfinHpxT1KcTI8umh55aaiFug12jKKSMZoae4ttlA")
}
#31 times over 26 seconds
#with some previous calls that were made.

# OPTION 1: OAuth app, cached to the server.
#so we could try https://googlesheets4.tidyverse.org/reference/sheets_auth.html
#it might not last long.
library(googlesheets4)
sheets_auth(email = "newzealandborderriskapp@gmail.com")
my_sheet <- googlesheets4::read_sheet("1hkpfinHpxT1KcTI8umh55aaiFug12jKKSMZoae4ttlA")

# API KEY
for (i in 1:200){
  print(paste0(i, "; " , now()))
  
  #set this for now, but we may need to follow the instructions below:
  manual_corrections <- read_sheet("1hkpfinHpxT1KcTI8umh55aaiFug12jKKSMZoae4ttlA")
}
#fails at about the same spot.
#is there any real value in using this?
#I am not sure: possibly, if it means that the logged-in user gets their own specific 100 calls.
#but it's still limited. And we will have to worry about how to store the credentials.
#perhaps though, signing up for a billing account enables us to increase quota?
#having authenticated as newzealandborderriskapp, I am not sure this actually then uses the Google API; it might use some of the googlesheets4's own magic.
#TRY USING AN OAUCTH KEY
gs4_deauth()

google_app <- httr::oauth_app(
  "Desktop-client-1-Border-Risk-App",
  key = "141372699862-cnn2d95dnn44gkuu64qnbm6mb4kb3v92.apps.googleusercontent.com",
  secret = borderapp_google_auth_secret
)
api_key <- "AIzaSyAnEAdoH-yLBO1rvhmAD-kkKR9TMYqI0Rs"

gs4_auth_configure(app = google_app, api_key = api_key)
gs4_oauth_app()
gs4_api_key()


for (i in 1:200){
  print(paste0(i, "; " , now()))
  
  #set this for now, but we may need to follow the instructions below:
  manual_corrections<-read_sheet("1hkpfinHpxT1KcTI8umh55aaiFug12jKKSMZoae4ttlA")
}


#let's try again....

