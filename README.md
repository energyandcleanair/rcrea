



## Bigger picture
CREA Scraper is part of a larger ecosystem, as shown in the diagram below.

![CREA Architecture](./crea_architecture.svg)


## Getting started
### Install
```
install.packages('devtools')
library(devtools)
url <- "https://github.com/hubert-thieriot/crea_r_package"
devtools::install_github(url)
```

## Usage
### Shiny Application
An online version is available on <https://crea.shinyapps.io/shiny/>.
Otherwise, you can run it locally using:
```
creadb::runShinyApp()
```

### Markdown examples
Markdowns are available in the examples folder
```buildoutcfg
df_locations <- creadb::locations(country='IN')
df_measurements <- creadb::measurements(country='IN', poll=creadb::PM25, city='Delhi')
``` 
