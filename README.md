




## Getting started
### Install
```buildoutcfg

install.packages('devtools')
library(devtools)
url <- "https://github.com/hubert-thieriot/crea_r_package"
devtools::install_github(url)
```

## Usage
### Shiny Application
```buildoutcfg
runShinyApp()
```
### R Code
```buildoutcfg
df_locations <- creadb::locations(country='IN')
df_measurements <- creadb::measurements(country='IN', poll=creadb::PM25, city='Delhi')
``` 
