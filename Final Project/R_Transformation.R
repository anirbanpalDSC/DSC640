library('magrittr')

# Set directory path for data file
datadir <- paste(getwd(), 'Data', sep='/')
infile1 <- 'airline-safety.csv'
outfile1 <- 'AirlineSafety.xlsx'

# Load raw data
AirlineSafetyRaw <- read.csv2(paste(datadir, file1, sep='/'),
                              header=TRUE,
                              sep=',',
                              stringsAsFactors = FALSE)

# Identify first world countries
FirstWorld <- c('Air Canada',
                'Air France',
                'Air New Zealand',
                'Alaska Airlines',
                'Alitalia',
                'All Nippon Airways',
                'American',
                'Austrian Airlines',
                'British Airways',
                'Delta / Northwest',
                'Finnair',
                'Hawaiian Airlines',
                'Japan Airlines',
                'KLM',
                'Lufthansa',
                'Qantas',
                'Singapore Airlines',
                'SWISS',
                'TAP - Air Portugal',
                'United / Continental',
                'US Airways / America West',
                'Virgin Atlantic')

# Add required columns
AirlineSafetyClean <- AirlineSafetyRaw %>% 
  dplyr::mutate(airline = stringr::str_trim(airline, 'both'),
                has_regional = dplyr::if_else(stringr::str_sub(AirlineSafetyRaw$airline, 
                                                               stringr::str_length(AirlineSafetyRaw$airline), 
                                                               stringr::str_length(AirlineSafetyRaw$airline))=='*',
                                              'Y',
                                              'N'),
                airline = dplyr::if_else(has_regional=='Y',
                                         stringr::str_sub(airline,
                                                          1,
                                                          stringr::str_length(airline)-1),
                                               airline),
                is_firstworld = dplyr::if_else(airline %in% FirstWorld, 
                                               'Y',
                                               'N'),
                incidents_wt_85_99 = (incidents_85_99/avail_seat_km_per_week)*10^9,
                incidents_wt_00_14 = (incidents_00_14/avail_seat_km_per_week)*10^9,
                fatalities_wt_85_99 = (fatalities_85_99/avail_seat_km_per_week)*10^9,
                fatalities_wt_00_14 = (fatalities_00_14/avail_seat_km_per_week)*10^9,
                fatal_accidents_wt_85_99 = (fatal_accidents_85_99/avail_seat_km_per_week)*10^9,
                fatal_accidents_wt_00_14 = (fatal_accidents_00_14/avail_seat_km_per_week)*10^9,                
                incidents_wt_total = incidents_wt_85_99 + incidents_wt_00_14,
                fatalities_wt_total = fatalities_wt_85_99 + fatalities_wt_00_14,
                fatal_accidents_wt_total = fatal_accidents_wt_85_99 + fatal_accidents_wt_00_14,
                range_total_wt_85_99 = incidents_wt_85_99 + fatalities_wt_85_99 + fatal_accidents_wt_85_99,
                range_total_wt_00_14 = incidents_wt_00_14 + fatalities_wt_00_14 + fatal_accidents_wt_00_14,
                grand_total_wt = range_total_wt_85_99 + range_total_wt_00_14)

# Transform data structure to long format for Power BI
# keycol <- c('airline_clean','has_regional','is_firstworld','avail_seat_km_per_week')
# valuecol <- c('incidents_85_99','fatal_accidents_85_99','fatalities_85_99',
#               'incidents_00_14','fatal_accidents_00_14','fatalities_00_14')
# gathercol <- valuecol
# 
# AirlineSafetyFinal <- AirlineSafetyClean %>% 
#   tidyr::gather(keycol,valuecol,gathercol) %>% 
#   dplyr::mutate(type = dplyr::case_when(stringi::stri_detect_fixed(keycol, 'incidents') ~ 'Incident',
#                                         stringi::stri_detect_fixed(keycol, 'fatal_accidents') ~ 'Fatal Accidents',
#                                         stringi::stri_detect_fixed(keycol, 'fatalities') ~ 'Fatalities',
#                                         TRUE ~ as.character(keycol)),
#                 period = dplyr::case_when(stringi::stri_detect_fixed(keycol, '85_99') ~ '1985-1999',
#                                           stringi::stri_detect_fixed(keycol, '00_14') ~ '2000-2014',
#                                           TRUE ~ as.character(keycol))) %>% 
#   dplyr::rename(count = valuecol,
#                 airline = airline_clean) %>% 
#   dplyr::select(airline,
#                 has_regional,
#                 is_firstworld,
#                 avail_seat_km_per_week,
#                 type,
#                 period,
#                 count)

# Store data file
xlsx::write.xlsx(AirlineSafetyClean, 
                 paste(datadir, outfile1, sep='/'), 
                 sheetName = 'Safety',
                 row.names = FALSE)
