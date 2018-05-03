library(stringr)
library(feather)

# Read and clean comma separated files ------------------------------------

read_weather_delim <- function(fileName){
  file <- read_delim(paste0("data/", fileName, ".txt"), delim = ",", skip = 1, col_names = F, trim_ws = T)
  colnames(file) <- c("id", "city", "stateYRS", month.abb, "ann")
  file <- 
    file %>%
    mutate(city = str_replace_all(city, "\\.", ""),
           state = trimws(gsub("[0-9]", "", stateYRS)),
           state = str_replace_all(state, "\\.", "")) %>% 
    unite("cityState", city, state, sep = ", ", remove = FALSE) %>% 
    gather(month, value, Jan:Dec) %>% 
    mutate(month = factor(month, levels = month.abb)) %>% 
    arrange(state, city) %>% 
    select(-stateYRS, -ann) %>% 
    mutate(variable = fileName)
  
  return(file)
}

# Normal Daily Maximum Temperature, °F ------------------------------------

nrmmax <- read_weather_delim("nrmmax")

# Normal Daily Minimum Temperature, °F ------------------------------------

nrmmin <- read.fwf(file = "data/nrmmin.txt", 
                    width = c(5, 33, 4, rep(7, 13)), 
                    skip = 1, 
                    col.names = c("id", "cityState", "YRS", month.abb, "ann"))
nrmmin <- 
  as_tibble(nrmmin) %>%
  mutate_all(trimws) %>% 
  mutate(commaCount = str_count(cityState, ",")) %>% 
  mutate(cityState = str_replace_all(cityState, "\\.", ""),
         cityState = if_else(commaCount == 2, str_replace(cityState, ", ", "-"), cityState),
         cityState = str_replace(cityState, "PADUCAH KY", "PADUCAH, KY")) %>% 
  separate(cityState, c("city", "state"), sep = ",", remove = FALSE) %>%
  mutate(state = trimws(state)) %>% 
  gather(month, value, Jan:Dec) %>% 
  mutate(value = as.numeric(value),
         variable = "nrmmin") %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  arrange(state, city) %>% 
  select(-commaCount, -YRS, -ann)


# Normal Daily Mean (Average) Temperature, °F -----------------------------

nrmavg <- read_weather_delim("nrmavg")

# Normal Heating Degree Days (July–June) ----------------------------------

nrmhdd <- read_weather_delim("nrmhdd")

# Normal Cooling Degree Days (January–December) ---------------------------

nrmcdd <- read_weather_delim("nrmcdd")

# Normal Precipitation, Inches --------------------------------------------

nrmpcp <- read_weather_delim("nrmpcp")

# Merge data --------------------------------------------------------------

weatherData <- 
  nrmmax %>% 
  full_join(nrmmin) %>% 
  full_join(nrmavg) %>% 
  full_join(nrmhdd) %>% 
  full_join(nrmcdd) %>% 
  full_join(nrmpcp)

variableLookup <- data_frame(
 variable = c("nrmmax", "nrmmin", "nrmavg", "nrmhdd", "nrmcdd", "nrmpcp"),
 variableName = c("Daily Maximum Temperature, °F",
                  "Daily Minimum Temperature, °F",
                  "Daily Average Temperature, °F",
                  "Heating Degree Days",
                  "Cooling Degree Days",
                  "Percipitation, in")
)

weatherData <- left_join(weatherData, variableLookup)

write_feather(weatherData, "data/weatherData.feather")
