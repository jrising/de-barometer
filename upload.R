## setwd("~/research/coastal/barometer/rshiny")

rsconnect::setAccountInfo(name="openmodels", token='4D47A517FA7734477B9EC3A8E280C387', secret='vr637YlcU+ssqcpZoRBQnCvDDQpiFvKlHfrGct4u')

library(rsconnect)

deployApp(appTitle='Coastal Barometer', appName='barometer')
