library('dplyr')
library('ggplot2')
library('stringr')
library('dygraphs')
library('tidyr')
library('xts')
library('quantmod')
library('rworldmap') # world map vis
library('googleVis')
#--------following data;
# - GDP, GDP per_capita, population, babies per women, school 
setwd('/home/hieppham/Data Science/Semester 3/R visualisation/world_social_demo_viz')
lookup_table <- read.csv('data/lookup_table.csv', stringsAsFactors = FALSE)

# define a function to get an tranform the data into 'tidyr' format
get_data <- function(filename, colname = NA){
  data <-read.csv(paste0('data/',filename,'.csv'))
  names(data) <- c('country',str_extract(names(data)[2:ncol(data)],'[0-9]+'))  
  data <-data%>%
    gather(key=year,value =value, -country)%>%
    drop_na()%>%
    dplyr::group_by(country)%>%
    dplyr::arrange(country, year)%>%
    as.data.frame()
  names(data) <- c('country','year', ifelse(is.na(colname),filename,colname))
  return(data)
}
#------------
data <-get_data(filename = 'gdp')%>%
  left_join(get_data(filename = 'population'),by =c('country','year'))%>%
  left_join(get_data(filename = 'life_expectancy'), by =c('country','year'))%>%
  left_join(get_data(filename = 'fertility_rate'),by =c('country','year'))%>%
  left_join(get_data(filename = 'child_mortality'),by =c('country','year'))%>%
  left_join(get_data(filename = 'literacy_rate_adult_total',colname='liter_all'),by =c('country','year'))%>%
  left_join(get_data(filename = 'literacy_rate_adult_male',colname='liter_male'),by =c('country','year'))%>%
  left_join(get_data(filename = 'literacy_rate_adult_female',colname='liter_female'), by =c('country','year'))%>%
  left_join(get_data(filename = 'mean_years_in_school_men_15_to_24_years',colname='yearschool_m_1524'), by =c('country','year'))%>%
  left_join(get_data(filename = 'mean_years_in_school_women_15_to_24_years',colname='yearschool_fm_1524'), by =c('country','year'))%>%
  left_join(get_data(filename = 'inequality_index_gini'),by =c('country','year'))%>%
  left_join(get_data(filename = 'income_share_of_richest_20percent'),colname='poorest_20pct',by =c('country','year'))%>%
  left_join(get_data(filename = 'income_share_of_poorest_20percent',colname='richest_20pct'), by =c('country','year'))
# data interpolation ---------
interpolate <- function(x){
  for (i in 2:length(x)){
    if (is.na(x[i])==TRUE){
      x[i] <- x[i-1]
    }
  }
  return(x)
}

unique_country <- unique(data$country)
clean_data <- data.frame()
for (i in 1:length(unique_country)){
  tryCatch({
    tmp_first <- data[data$country==unique_country[i],1:2]
    tmp_df <- cbind(tmp_first,apply(data[data$country==unique_country[i],3:ncol(data)],
                                 2, function(x) interpolate(x)))
  }, error = function(e){
    tmp_df <- data[data$country==unique_country[i],1:ncol(data)]
  })
  clean_data = rbind(clean_data, tmp_df)
}
clean_data$year <- as.integer(clean_data$year)
saveRDS(clean_data, 'data.rds')
  

#-----------BUBLE CHART

#----------- POPULATION TREND
pop <- read.csv('data/population_forecast_wb.csv')
head(pop)
names(pop) <- c('country','country_code','group','indicator_name',
                       'indicator_code',str_extract(names(pop)[6:ncol(pop)],
                                                    '[0-9]+'))  
pop <-pop%>%
  gather(key=year,value =population, 6:ncol(pop))%>%
  drop_na()%>%
  dplyr::group_by(country)%>%
  dplyr::arrange(country, year)%>%
  as.data.frame()

pop%>%
  filter(group=='country', country=='Afghanistan', year==2017)%>%
  group_by( country,indicator_name)%>%
  View()

# create some variables 
indicators <-unique(pop$indicator_name)
age_group_Female <- as.vector(indicators[grep('Female population',indicators)])
age_group_Male <- as.vector(indicators[grep('Male population',indicators)])

pop_age_group <-pop%>%
  filter(group=='country',indicator_name %in% c(age_group_Female,age_group_Male))%>%
  select(country, country_code, indicator_name, year, population)%>%
  mutate(gender = sub(pattern = 'Male [+ A-Za-z 0-9 -]*',x=indicator_name,replacement = 'Male'))%>%
  mutate(gender= sub(pattern = 'Female [+ A-Za-z 0-9 -]*',x=gender,replacement = 'Female'))%>%
  mutate(age_group = sub(pattern='[A-Za-z ]* ', x = indicator_name, replacement = ''))%>%
  select(-indicator_name)
pop_age_group <- pop_age_group%>%
  left_join(lookup_table%>%select(country_code,continent))
pop_age_group <-pop_age_group%>%
  mutate(signed_population = ifelse(gender=='Male',population,-population))
saveRDS(pop_age_group,'pop_age_group.Rds')


clean_data <- unique(clean_data)

Motion=gvisMotionChart(clean_data%>%
                         left_join(lookup_table%>%select(country,continent)),
                       idvar="country",
                       timevar="year",
                       chartid = 'demo_graphic2',
                       options = list(title='World social and economcial change past decades'))
plot(Motion)

