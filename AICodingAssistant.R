install.packages("chattr")
install.packages("ellmer")
usethis::edit_r_environ()

library(chattr)
library(ellmer)
chattr_use(chat_google_gemini())
chattr_app()
