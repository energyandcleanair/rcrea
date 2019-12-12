




## Getting started
### Install
```buildoutcfg

install.packages('devtools')
library(devtools)
url <- "https://gitlab.com/creaaq/crea_r_package"
devtools::install_git(url = url)
```

### Usage
```buildoutcfg
df_locations <- creadb::locations(country='IN')
df_measurements <- creadb::measurements(country='IN') # This will take a lot of time
``` 


Try it in our Notebook here:
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gl/creaaq%2Fcrea_r_package/master?filepath=R%2Fexamples%2Fexamples.Rmd)