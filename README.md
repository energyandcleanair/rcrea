




## Getting started
### Install
```buildoutcfg

install.packages('devtools')

url <- "https://gitlab.com/creaaq/crea_r_package"
devtools::install_git(url = url)
```

### Usage
```buildoutcfg
df_locations <- creadb::locations(country='IN')
df_measurements <- creadb::measurements(country='IN') # This will take a lot of time
```