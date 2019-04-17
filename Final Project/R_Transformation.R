library('magrittr')

# Set directory path for data file
datadir <- paste(getwd(), 'Data/Source', sep='/')
outdir <- paste(getwd(), 'Data/Target', sep='/')
infile1 <- 'airline-safety.csv'
outfile1 <- 'AirlineSafety.xlsx'
outfile1_1 <- 'AirlineSafetyLong.xlsx'
infile2 <- 'auto-fatalities.xlsx'
outfile2 <- 'Falities.xlsx'
outfile3 <- 'AvgDeath.xlsx'

### Airlines data

# Load raw data
AirlineSafetyRaw <- read.csv2(paste(datadir, infile1, sep='/'),
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
keycol <- c('airline_clean','has_regional','is_firstworld','avail_seat_km_per_week')
valuecol <- c('incidents_85_99','fatal_accidents_85_99','fatalities_85_99',
              'incidents_00_14','fatal_accidents_00_14','fatalities_00_14')
gathercol <- valuecol

AirlineSafetyLong <- AirlineSafetyClean %>%
  tidyr::gather(keycol,valuecol,gathercol) %>%
  dplyr::mutate(type = dplyr::case_when(stringi::stri_detect_fixed(keycol, 'incidents') ~ 'Incident',
                                        stringi::stri_detect_fixed(keycol, 'fatal_accidents') ~ 'Fatal Accidents',
                                        stringi::stri_detect_fixed(keycol, 'fatalities') ~ 'Fatalities',
                                        TRUE ~ as.character(keycol)),
                period = dplyr::case_when(stringi::stri_detect_fixed(keycol, '85_99') ~ '1985-1999',
                                          stringi::stri_detect_fixed(keycol, '00_14') ~ '2000-2014',
                                          TRUE ~ as.character(keycol)),
                count_wt = (valuecol/avail_seat_km_per_week)*10^9) %>%
  dplyr::rename(count = valuecol) %>%
  dplyr::select(airline,
                has_regional,
                is_firstworld,
                avail_seat_km_per_week,
                type,
                period,
                count,
                count_wt)

# Store data file
xlsx::write.xlsx2(AirlineSafetyClean, 
                 paste(outdir, outfile1, sep='/'), 
                 sheetName = 'Safety',
                 row.names = FALSE)

xlsx::write.xlsx2(AirlineSafetyLong, 
                 paste(outdir, outfile1_1, sep='/'), 
                 sheetName = 'Safety',
                 row.names = FALSE)

### Auto data

# Read in the data 
AutoFatalitiesRaw <- xlsx::read.xlsx2(paste(datadir, infile2, sep='/'), 1, stringsAsFactors = FALSE)

# Data transformation
AutoFatalitiesPrep <- AutoFatalitiesRaw %>% 
  dplyr::mutate(period = dplyr::case_when(Year >= 1985 & Year <= 1999 ~ '1985-1999',
                                          Year >= 2000 & Year <= 2014 ~ '2000-2014',
                                          TRUE ~ as.character(Year)),
                Deaths = as.numeric(Deaths),
                VMT_Vehicle_Miles_Travelled_bn = as.numeric(VMT_Vehicle_Miles_Travelled_bn),
                category = 'Auto') %>% 
  dplyr::group_by(period, category) %>% 
  dplyr::summarise(fatalities_tot = sum(Deaths),
                   VMT_bn_tot = sum(VMT_Vehicle_Miles_Travelled_bn)) %>% 
  dplyr::mutate(fatalities_wt = fatalities_tot/VMT_bn_tot) %>% 
  as.data.frame()

# Create data for fatality comparison
Fatality <- AirlineSafetyLong %>% 
  dplyr::filter(is_firstworld == 'Y') %>% 
  dplyr::mutate(category = 'Airlines') %>% 
  dplyr::group_by(period, category) %>% 
  dplyr::summarise(fatalities_tot = sum(count)) %>% 
  dplyr::select(category, period, fatalities_tot) %>% 
  dplyr::bind_rows(AutoFatalitiesPrep %>% 
                     dplyr::select(category, period, fatalities_tot)) %>% 
  as.data.frame()


# Output fatalities data
xlsx::write.xlsx2(Fatality, 
                  paste(outdir, outfile2, sep='/'), 
                  sheetName = 'AutoStats',
                  row.names = FALSE)

# Calculate likelihood of fatalities in airlines
AverageDeaths <- AirlineSafetyLong %>% 
  dplyr::filter(type=='Fatalities') %>% 
  dplyr::mutate(category = 'Airlines') %>% 
  dplyr::group_by(category) %>% 
  dplyr::summarise(totalfatality = sum(count)) %>% 
  dplyr::mutate(AvgAnnualDeath = totalfatality/(2014-1985)) %>% 
  dplyr::bind_rows(AutoFatalitiesPrep %>% 
                     dplyr::group_by(category) %>% 
                     dplyr::summarise(totalfatality = sum(fatalities_tot)) %>% 
                     dplyr::mutate(AvgAnnualDeath = totalfatality/(2014-1985))) %>% 
  as.data.frame()

# Output Average fatalities data
xlsx::write.xlsx2(AverageDeaths, 
                  paste(outdir, outfile3, sep='/'), 
                  sheetName = 'Avg Annual Deaths',
                  row.names = FALSE)
