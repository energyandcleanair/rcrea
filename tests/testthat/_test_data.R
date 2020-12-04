library(testthat)

# Testing CREA data vs official or semi-official sources
test_that("Numbers match previous studies", {

  within_range <- function(value_crea, value_official, max_relative_error ){
    return(abs((value_crea-value_official)/value_official) <= max_relative_error)
  }

  #-------------
  # Beijing
  #-------------
  obs <- data.frame("country"=character(), "city"=character(), "location_id"=character(), "poll"=character(),
                    "year"=integer(), "month"=integer(), "day"=integer(), "value"=double(), "source"=character(),
                    stringsAsFactors = F)

  # The 2019 average concentration of 42 micrograms per cubic metre was 53 per cent lower
  # than the 2013 figure of 89.5, according to the municipal ecology and environment bureau.
  # https://www.scmp.com/news/china/society/article/3044747/beijings-air-quality-shows-significant-improvements-war
  link <- "https://www.scmp.com/news/china/society/article/3044747/beijings-air-quality-shows-significant-improvements-war"
  obs[nrow(obs) + 1,] <- list("CN", "Beijing|北京市", NA, PM25, 2019, NA, NA, 42, link)

  # Average PM2.5 level in Beijing was 52 micrograms per cubic meter in January,
  # up 52.9 percent from a year ago, according to the MEE data.
  # https://www.reuters.com/article/us-china-pollution-beijing/chinas-capital-beijing-vows-air-quality-improvement-but-gives-no-target-idUSKCN1Q90TU
  link <- "https://www.reuters.com/article/us-china-pollution-beijing/chinas-capital-beijing-vows-air-quality-improvement-but-gives-no-target-idUSKCN1Q90TU"
  obs[nrow(obs) + 1,] <- list("CN", "Beijing|北京市", NA, PM25, 2019, 1, NA, 52, link)
  obs[nrow(obs) + 1,] <- list("CN", "Beijing|北京市", NA, PM25, 2018, 1, NA, 52/(1+0.529), link)

  # Average concentrations of PM2.5, or particulate matter that measures 2.5 microns, in 2018
  # fell 12.1 percent from 2017 to 51 micrograms per cubic meter
  obs[nrow(obs) + 1,] <- list("CN", "Beijing|北京市", NA, PM25, 2018, NA, NA, 51, link)
  obs[nrow(obs) + 1,] <- list("CN", "Beijing|北京市", NA, PM25, 2017, NA, NA, 51/(1-0.121), link)

  # This has also resulted in the concentration of sulphur dioxide in the atmosphere dropping by 85 per cent
  # from 28 microgrammes per cubic metre in 2013 to 4 in 2019.
  obs[nrow(obs) + 1,] <- list("CN", "Beijing|北京市", NA, SO2, 2019, NA, NA, 4, link)

  #-------------
  # China
  #-------------
  # According to central government figures, in 2018, the national average concentration of PM2.5
  # was 39 micrograms per cubic meter, 9.3 per cent lower than the previous year
  # https://www.scmp.com/news/china/society/article/3044747/beijings-air-quality-shows-significant-improvements-war
  link <- "https://www.scmp.com/news/china/society/article/3044747/beijings-air-quality-shows-significant-improvements-war"
  obs[nrow(obs) + 1,] <- list("CN", NA, NA, PM25, 2019, NA, NA, 39, link)
  obs[nrow(obs) + 1,] <- list("CN", NA, NA, PM25, 2018, NA, NA, 39/(1-0.093), link)

  # The average concentration of PM10 particles and nitrogen dioxide were 68 and 37 micrograms per cubic metre,
  # both in line with national targets.
  obs[nrow(obs) + 1,] <- list("CN", NA, NA, PM10, 2019, NA, NA, 69, link)
  obs[nrow(obs) + 1,] <- list("CN", NA, NA, NO2, 2019, NA, NA, 37, link)



  #-------------
  # India
  #-------------
  # # 2017 data
  # # http://www.cpcbenvis.nic.in/air_quality_data.html
  # #           SO2	NO2	PM10	PM2.5
  # #	Hyderabad	6	  28	108	54
  # # Kolkata	  6	  41	120	71
  # # Mumbai    3	  18	151	40
  # # Delhi     7	  68	241	101
  # link <- "http://www.cpcbenvis.nic.in/air_quality_data.html"
  # obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', NA, SO2, 2017, NA, NA, 6, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', NA, NO2, 2017, NA, NA, 28, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', NA, PM10, 2017, NA, NA, 108, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', NA, PM25, 2017, NA, NA, 54, link)
  #
  # obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', NA, SO2, 2017, NA, NA, 6, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', NA, NO2, 2017, NA, NA, 41, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', NA, PM10, 2017, NA, NA, 120, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', NA, PM25, 2017, NA, NA, 71, link)
  #
  # obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', NA, SO2, 2017, NA, NA, 3, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', NA, NO2, 2017, NA, NA, 18, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', NA, PM10, 2017, NA, NA, 151, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', NA, PM25, 2017, NA, NA, 40, link)
  #
  # obs[nrow(obs) + 1,] <- list("IN", 'Delhi', NA, SO2, 2017, NA, NA, 7, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Delhi', NA, NO2, 2017, NA, NA, 68, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Delhi', NA, PM10, 2017, NA, NA, 241, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Delhi', NA, PM25, 2017, NA, NA, 101, link)
  #
  # # 2016 data
  # # http://www.cpcbenvis.nic.in/air_quality_data.html
  # #           SO2	NO2	PM10	PM2.5
  # #	Hyderabad	4	  27	100	  49
  # # Kolkata	  4	  49	113	  70
  # # Mumbai    6	  30	119   -
  # # Delhi     7	  66	278	  118
  # obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', NA, SO2, 2016, NA, NA, 4, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', NA, NO2, 2016, NA, NA, 27, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', NA, PM10, 2016, NA, NA, 100, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', NA, PM25, 2016, NA, NA, 49, link)
  #
  # obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', NA, SO2, 2016, NA, NA, 4, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', NA, NO2, 2016, NA, NA, 49, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', NA, PM10, 2016, NA, NA, 113, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', NA, PM25, 2016, NA, NA, 70, link)
  #
  # obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', NA, SO2, 2016, NA, NA, 6, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', NA, NO2, 2016, NA, NA, 30, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', NA, PM10, 2016, NA, NA, 119, link)
  #
  # obs[nrow(obs) + 1,] <- list("IN", 'Delhi', NA, SO2, 2016, NA, NA, 7, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Delhi', NA, NO2, 2016, NA, NA, 66, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Delhi', NA, PM10, 2016, NA, NA, 278, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Delhi', NA, PM25, 2016, NA, NA, 118, link)


  # CPCB CCR data
  # North Campus Delhi
  # S.No	From Date	To Date	PM2.5 (ug/m3)	NO (ug/m3)	NO2 (ug/m3)	CO (mg/m3)	Ozone (ug/m3)	PM10 (ug/m3)	NOx (ppb)
  # 2	01-Jan-2016 - 00:00	01-Jan-2017 - 00:00	66.07	11.68	13.85	1.48	26.72	167.13	25.53
  link <- 'https://app.cpcbccr.com/ccr/#/caaqm-dashboard-all/caaqm-view-data-report/'
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', PM10, 2016, NA, NA, 167.13, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', PM25, 2016, NA, NA, 66.07, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', NO, 2016, NA, NA, 11.68, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', NO2, 2016, NA, NA, 13.85, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', NOX, 2016, NA, NA, 25.53, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', CO, 2016, NA, NA, 1480, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', O3, 2016, NA, NA, 26.72, link)



  # S.No	From Date	To Date	NO (ug/m3)	PM10 (ug/m3)	PM2.5 (ug/m3)	NO2 (ug/m3)	NOx (ppb)	CO (mg/m3)Ozone (ug/m3)
  # 1	01-Nov-2018 - 00:00	02-Nov-2018 - 00:00	171.68	379.09	283.77	127.78	299.3	4.16		28.74
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', PM25, 2018, 11, 1, 283.77, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', NO, 2018, 11, 1, 171.68, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', NO2, 2018, 11, 1, 127.78, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', CO, 2018, 11, 1, 4160, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', O3, 2018, 11, 1, 28.74, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', PM10, 2018, 11, 1, 379.09, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-82', NOX, 2018, 11, 1, 299.3, link)

  # Siri fort, Delhi
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-131', PM25, 2018, 2, 1, 125.2, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-131', NO, 2018, 2, 1, 28.06, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', 'IN-131', NO2, 2018, 2, 1, 73.73, link)


  # Victoria, Kolkata - WBPCB
  # S.No	From Date	To Date	                    PM10 (ug/m3)	PM2.5 (ug/m3)	NO (ug/m3)	NO2 (ug/m3)	NOx (ppb)	CO (mg/m3)	Ozone (ug/m3)
  # 1	01-Jan-2017 - 00:00	01-Jan-2018 - 00:00	88.34		      -             49.66	        50.11	      100.07	    4.86	    31.68
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', PM10, 2017, NA, NA, 88.34, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', PM25, 2017, NA, NA, NA, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', NO, 2017, NA, NA, 49.66, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', NO2, 2017, NA, NA, 50.11, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', NOX, 2017, NA, NA, 100.07, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', CO, 2017, NA, NA, 4860, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', O3, 2017, NA, NA, 31.68, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', SO2, 2017, NA, NA, NA, link)


  # S.No	From Date	To Date	PM10 (ug/m3)	PM2.5 (ug/m3)	Ozone (ug/m3)	SO2 (ug/m3)	NO2 (ug/m3)	NO (ug/m3)	CO (mg/m3)	NOx (ppb)
  # 1	01-Jan-2018 - 00:00	01-Jan-2019 - 00:00	84.57	51.51	19.58	4.95	40.69	29.3	1.06	69.57
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', PM25, 2018, NA, NA, 51.51, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', NO, 2018, NA, NA, 29.3, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', NO2, 2018, NA, NA, 40.69, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', CO, 2018, NA, NA, 1060, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', O3, 2018, NA, NA, 19.58, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', PM10, 2018, NA, NA, 84.57, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', NOX, 2018, NA, NA, 69.57, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', SO2, 2018, NA, NA, 4.95, link)

  # S.No	From Date	To Date	PM10 (ug/m3)	PM2.5 (ug/m3)	Ozone (ug/m3)	SO2 (ug/m3)	NO2 (ug/m3)	NO (ug/m3)	CO (mg/m3)	NOx (ppb)
  # 1	20-Oct-2019 - 00:00	21-Oct-2019 - 00:00	107.9	69.54	37.38	3.28	54.67	20.08	1.44	74.75
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', PM25, 2019, 10, 20, 69.54, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', NO, 2019, 10, 20, 20.08, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', NO2, 2019, 10, 20, 54.67, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', CO, 2019, 10, 20, 1440, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', O3, 2019, 10, 20, 37.38, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', PM10, 2019, 10, 20, 107.9, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', NOX, 2019, 10, 20, 74.75, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', 'IN-142', SO2, 2019, 10, 20, 3.28, link)

  # Adarsh Nagar, Jaipur - RSPCB "IN-297"

  # "Bandra, Mumbai - MPCB"	"IN-22"
  # S.No	From Date	To Date	PM2.5 (ug/m3)	PM10 (ug/m3)	CO (mg/m3)	SO2 (ug/m3)	Ozone (ug/m3)	NO2 (ug/m3)	NO (ug/m3)	NOx (ppb)
  # 1	20-Feb-2019 - 00:00	21-Feb-2019 - 00:00	62.81	177.22	2.83	21.88	101.11	27.95	54.33	82.29
  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', 'IN-22', PM25, 2019, 2, 20, 62.81	, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', 'IN-22', PM10, 2019, 2, 20, 177.22, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', 'IN-22', CO, 2019, 2, 20, 2830, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', 'IN-22', SO2, 2019, 2, 20, 21.88, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', 'IN-22', O3, 2019, 2, 20, 101.11, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', 'IN-22', NO2, 2019, 2, 20, 27.95, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', 'IN-22', NO, 2019, 2, 20, 54.33, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', 'IN-22', NOX, 2019, 2, 20, 82.29, link)

  # S.No	From Date	To Date	PM10 (ug/m3)	PM2.5 (ug/m3)	NO (ug/m3)	NO2 (ug/m3)	NOx (ppb)	CO (mg/m3)	Ozone (ug/m3)
  # 1	01-Jan-2016 - 00:00	01-Jan-2017 - 00:00					40.2	0.0
  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', 'IN-22', NOX, 2016, NA, NA, 40.2, link)

  # "Ardhali Bazar, Varanasi - UPPCB"	"IN-84"
  # S.No	From Date	To Date	PM2.5 (ug/m3)	PM10 (ug/m3)	SO2 (ug/m3)	Ozone (ug/m3)	NO2 (ug/m3)	CO (mg/m3)	NO (ug/m3)	NOx (ppb)
  # 1	15-Mar-2017 - 00:00	16-Mar-2017 - 00:00	86.81	202.84	12.71	13.25	21.71	1.13	8.45	30.16
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM25, 2017, 3, 15, 86.81, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM10, 2017, 3, 15, 202.84, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', CO, 2017, 3, 15, 1130, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO, 2017, 3, 15, 8.45, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', SO2, 2017, 3, 15, 12.71, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', O3, 2017, 3, 15, 13.25, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO2, 2017, 3, 15, 21.71, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NOX, 2017, 3, 15, 30.16, link)

  # S.No	From Date	To Date	PM2.5 (ug/m3)	PM10 (ug/m3)	SO2 (ug/m3)	Ozone (ug/m3)	NO2 (ug/m3)	CO (mg/m3)	NO (ug/m3)	NOx (ppb)
  # 1	15-Mar-2018 - 00:00	16-Mar-2018 - 00:00	145.06	356.74	34.52	30.98	104.11	1.25	34.09	53.1
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM25, 2018, 3, 15, 145.06, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM10, 2018, 3, 15, 356.74, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', CO, 2018, 3, 15, 1250, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO, 2018, 3, 15, 34.09, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', SO2, 2018, 3, 15, 34.52, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', O3, 2018, 3, 15, 30.98, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO2, 2018, 3, 15, 104.11, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NOX, 2018, 3, 15, 53.1, link)

  # S.No	From Date	To Date	PM2.5 (ug/m3)	PM10 (ug/m3)	CO (mg/m3)	SO2 (ug/m3)	Ozone (ug/m3)	NO2 (ug/m3)	NO (ug/m3)	NOx (ppb)
  # 1	20-Feb-2019 - 00:00	21-Feb-2019 - 00:00	154.41	305.38	1.1	31.78	66.59	117.26	43.49	75.22
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM25, 2019, 2, 20, 154.41, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM10, 2019, 2, 20, 305.38, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', CO, 2019, 2, 20, 1100, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO, 2019, 2, 20, 43.49, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', SO2, 2019, 2, 20, 31.78, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', O3, 2019, 2, 20, 66.59, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO2, 2019, 2, 20, 117.26, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NOX, 2019, 2, 20, 75.22, link)

  # S.No	From Date	To Date	                  PM2.5 (ug/m3)	PM10 (ug/m3)	SO2 (ug/m3)	Ozone (ug/m3)	NO2 (ug/m3)	CO (mg/m3)	NO (ug/m3)	NOx (ppb)
  # 1	22-Jan-2020 - 00:00	23-Jan-2020 - 00:00	137.17		                  14.29	40.75	50.62		                              8.76	20.11
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM25, 2020, 1, 22, 137.17, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM10, 2020, 1, 22, NA, link)
  # obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', CO, 2020, 1, 22, NA, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO, 2020, 1, 22, 8.76, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', SO2, 2020, 1, 22, 14.29, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', O3, 2020, 1, 22, 40.75, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO2, 2020, 1, 22, 50.62, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NOX, 2020, 1, 22, 20.11, link)

  # S.No	From Date	To Date	PM10 (ug/m3)	PM2.5 (ug/m3)	NO (ug/m3)	NO2 (ug/m3)	NOx (ppb)	CO (mg/m3)	Ozone (ug/m3)
  # 1	01-Jan-2016 - 00:00	01-Jan-2017 - 00:00	266.36	154.12	11.14	20.92	32.12	1.13	12.36
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM10, 2016, NA, NA, 266.36, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM25, 2016, NA, NA, 154.12, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO, 2016, NA, NA, 11.14, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO2, 2016, NA, NA, 20.92, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NOX, 2016, NA, NA, 32.12, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', CO, 2016, NA, NA, 1130, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', O3, 2016, NA, NA, 12.36, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', SO2, 2016, NA, NA, 5.76, link)

  # S.No	From Date	To Date	PM2.5 (ug/m3)	PM10 (ug/m3)	NO2 (ug/m3)	NO (ug/m3)	NOx (ppb)	SO2 (ug/m3)	CO (mg/m3)	Ozone (ug/m3)
  # 1	01-Jan-2017 - 00:00	01-Jan-2018 - 00:00	105.54	217.7	36.5	11.48	41.55	6.91	0.57	11.54
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM25, 2017, NA, NA, 105.54, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM10, 2017, NA, NA, 217.7, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO2, 2017, NA, NA, 36.5, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO, 2017, NA, NA, 11.48, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NOX, 2017, NA, NA, 41.55, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', SO2, 2017, NA, NA, 6.91, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', CO, 2017, NA, NA, 570, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', O3, 2017, NA, NA, 11.54, link)

  # S.No	From Date	To Date	PM2.5 (ug/m3)	PM10 (ug/m3)	NO2 (ug/m3)	NO (ug/m3)	NOx (ppb)	SO2 (ug/m3)	CO (mg/m3)	Ozone (ug/m3)
  # 2	01-Jan-2018 - 00:00	01-Jan-2019 - 00:00	103.25	219.62	77.84	19.41	35.8	22.1	1.0	42.82
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM25, 2018, NA, NA, 103.25, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM10, 2018, NA, NA, 219.62, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO2, 2018, NA, NA, 77.84, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO, 2018, NA, NA, 19.41, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NOX, 2018, NA, NA, 35.8, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', SO2, 2018, NA, NA, 22.1, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', CO, 2018, NA, NA, 1000, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', O3, 2018, NA, NA, 42.82, link)

  # S.No	From Date	To Date	                  PM2.5 (ug/m3)	PM10 (ug/m3)	NO2 (ug/m3)	NO (ug/m3)	NOx (ppb)	SO2 (ug/m3)	CO (mg/m3)	Ozone (ug/m3)
  # 1	01-Jan-2019 - 00:00	01-Jan-2020 - 00:00	97.96	        207.15	      55.33	      14.17	      25.45   	22.88	      0.68	      61.97
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM25, 2019, NA, NA, 97.96, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', PM10, 2019, NA, NA, 207.15, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO2, 2019, NA, NA, 55.33, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NO, 2019, NA, NA, 14.17, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', NOX, 2019, NA, NA, 25.45, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', SO2, 2019, NA, NA, 22.88, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', CO, 2019, NA, NA, 680, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Varanasi', 'IN-84', O3, 2019, NA, NA, 61.97, link)


  # "Central University, Hyderabad - TSPCB"	"IN-63"
  # S.No	From Date	To Date	PM2.5 (ug/m3)	PM10 (ug/m3)	SO2 (ug/m3)	Ozone (ug/m3)	NO2 (ug/m3)	CO (mg/m3)	NO (ug/m3)	NOx (ppb)
  # 1	15-Mar-2018 - 00:00	16-Mar-2018 - 00:00	43.25	104.67	6.75	69.79	49.54	1.01	6.48	32.27
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', PM25, 2018, 3, 15, 43.25, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', PM10, 2018, 3, 15, 104.67, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', CO, 2018, 3, 15, 1010, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', SO2, 2018, 3, 15, 6.75, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', O3, 2018, 3, 15, 69.79, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', NO2, 2018, 3, 15, 49.54, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', NO, 2018, 3, 15, 6.48, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', NOX, 2018, 3, 15, 32.27, link)

  # S.No	From Date	To Date	PM2.5 (ug/m3)	PM10 (ug/m3)	CO (mg/m3)	SO2 (ug/m3)	Ozone (ug/m3)	NO2 (ug/m3)	NO (ug/m3)	NOx (ppb)
  # 1	20-Feb-2019 - 00:00	21-Feb-2019 - 00:00	28.11	83.2	0.42	3.83	38.56	28.72	3.18	17.87
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', PM25, 2019, 2, 20, 28.11, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', PM10, 2019, 2, 20, 83.2, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', CO, 2019, 2, 20, 420, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', SO2, 2019, 2, 20, 3.83, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', O3, 2019, 2, 20, 38.56, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', NO2, 2019, 2, 20, 28.72, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', NO, 2019, 2, 20, 3.18, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', NOX, 2019, 2, 20, 17.87, link)

  # S.No	From Date	To Date	PM2.5 (ug/m3)	PM10 (ug/m3)	SO2 (ug/m3)	Ozone (ug/m3)	NO2 (ug/m3)	CO (mg/m3)	NO (ug/m3)	NOx (ppb)
  # 1	15-Jan-2020 - 00:00	16-Jan-2020 - 00:00	33.94	70.54	2.67	36.14	20.41	0.0	4.06	14.14
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', PM25, 2020, 1, 15, 33.94, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', PM10, 2020, 1, 15, 70.54, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', CO, 2020, 1, 15, 0, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', SO2, 2020, 1, 15, 2.67, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', O3, 2020, 1, 15, 36.14, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', NO2, 2020, 1, 15, 20.41, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', NO, 2020, 1, 15, 4.06, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-63', NOX, 2020, 1, 15, 14.14, link)

  # Sanathnagar, Hyderabad - TSPCB IN-111
  # S.No	From Date	To Date	NO (ug/m3)	NO2 (ug/m3)	NOx (ppb)	CO (mg/m3)	Ozone (ug/m3)	SO2 (ug/m3)	PM2.5 (ug/m3)
  # 1	01-Jan-2019 - 00:00	01-Jan-2020 - 00:00	6.56	32.51	22.61	0.7	28.37	8.24	48.05
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-111', PM25, 2019, NA, NA, 48.05, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-111', CO, 2019, NA, NA, 700, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-111', SO2, 2019, NA, NA, 8.24, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-111', O3, 2019, NA, NA, 28.37, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-111', NO2, 2019, NA, NA, 32.51, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-111', NO, 2019, NA, NA, 6.56, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', 'IN-111', NOX, 2019, NA, NA, 22.61, link)

  # "Ashok Nagar, Udaipur - RSPCB"	"IN-139"
  # S.No	From Date	To Date	PM2.5 (ug/m3)	PM10 (ug/m3)	CO (mg/m3)	SO2 (ug/m3)	Ozone (ug/m3)	NO2 (ug/m3)	NO (ug/m3)	NOx (ppb)
  # 1	20-Feb-2019 - 00:00	21-Feb-2019 - 00:00	65.69	185.01	1.75	14.74	20.38	53.95	68.24	122.24
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', PM25, 2019, 2, 20, 65.69, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', PM10, 2019, 2, 20, 185.01, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', CO, 2019, 2, 20, 1750, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', SO2, 2019, 2, 20, 14.74, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', O3, 2019, 2, 20, 20.38, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', NO2, 2019, 2, 20, 53.95, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', NO, 2019, 2, 20, 68.24, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', NOX, 2019, 2, 20, 122.24, link)

  # S.No	From Date	To Date	PM10 (ug/m3)	PM2.5 (ug/m3)	NO (ug/m3)	NO2 (ug/m3)	NOx (ppb)	SO2 (ug/m3)	Ozone (ug/m3)	CO (mg/m3)
  # 1	01-Feb-2020 - 00:00	02-Feb-2020 - 00:00	90.46	40.37	25.88	38.6	64.48	9.57	30.5	1.61
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', PM25, 2020, 2, 1, 40.37, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', PM10, 2020, 2, 1, 90.46, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', CO, 2020, 2, 1, 1610, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', SO2, 2020, 2, 1, 9.57, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', O3, 2020, 2, 1, 30.5, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', NO2, 2020, 2, 1, 38.6, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', NO, 2020, 2, 1, 25.88, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Udaipur', 'IN-139', NOX, 2020, 2, 1, 64.48, link)


  # Carbon Copy Dashboard
  link <- "https://ncap.carboncopy.info/ncap-cities/"
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', NA, PM10, 2016, NA, NA, 113, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Kolkata', NA, PM10, 2017, NA, NA, 120, link)

  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', NA, PM10, 2016, NA, NA, 119, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Mumbai', NA, PM10, 2017, NA, NA, 151, link)

  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', NA, PM10, 2016, NA, NA, 276, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Delhi', NA, PM10, 2017, NA, NA, 240, link)

  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', NA, PM10, 2016, NA, NA, 101, link)
  obs[nrow(obs) + 1,] <- list("IN", 'Hyderabad', NA, PM10, 2017, NA, NA, 108, link)

  #------------------------
  # Calculate crea values
  #------------------------

  # For one row
  get_crea_value <- function(row) {
    city <- if(is.na(row$city)) NULL else strsplit(row$city,"\\|")[[1]]
    month_from <- ifelse(is.na(row$month), 1,row$month)
    day_from <- ifelse(is.na(row$day), 1, row$day)
    date_from = lubridate::ymd(row$year*10000 + month_from*100 + day_from)
    month_to <- ifelse(is.na(row$month),12,row$month)
    day_to <- ifelse(is.na(row$day), days_in_month(month_to), row$day)
    date_to = lubridate::ymd(row$year*10000 + month_to*100 + day_to)

    meas <- measurements(country=row$country,
                         city=city,
                         location_id = row$location_id,
                         poll=row$poll,
                         date_from=date_from,
                         date_to=date_to,
                         collect=F)


    # Average per city if we want a national figure
    if(is.na(row$city) && is.na(row$location_id)){
      meas <- meas %>% group_by(city) %>% summarize(value=mean(value, na.rm = T)) %>% ungroup()
    }

    # Finally average all measurements
    meas <- meas %>% summarize(crea_value=mean(value, na.rm = T))
    meas <- meas %>% collect()

    return(meas)
  }

  # # For each row
  # obs_crea <- obs %>% by_row(..f = get_crea_value, .to = "crea_value", .collate = "cols")
  #
  #   # Reshaping
  # obs_crea <- obs_crea %>%
  #   dplyr::mutate(rel_diff=sprintf("%1.2f%%", 100*abs(value-crea_value1)/value)) %>%
  #   dplyr::select(-source,source) %>%
  #   dplyr::rename(value_official=value, value_crea=crea_value1)

})
