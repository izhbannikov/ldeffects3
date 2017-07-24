# A simple script to deploy on shinyapps:
library(rsconnect)
rsconnect::setAccountInfo(name='iz12', token='A3B758D61D31FBC69393B0B90B633796', secret='vWrjX8HBTk2cE0soU7G8tw4GWbFw5MnYkDj2vGhq')
rsconnect::deployApp("~/Projects/ldeffects3/", account="iz12")
