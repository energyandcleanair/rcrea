library(creadb)


locations <- creadb::locations(country=c("IN","CN"))
standards <- creadb::standards(collect=T)
polls <- c(creadb::PM25, creadb::PM10, creadb::NO2, creadb::O3, creadb::SO2, creadb::CO)
averagings <- c("hour", "day", "week", "month")
