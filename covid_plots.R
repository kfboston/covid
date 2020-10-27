########
## Build plots using JHU covid data. US only.
## author: Kevin Foster
## date: Covid 2020

## make sure you have these R packages installed. If not, uncomment the following
## line and run it.
# install.packages(c('tidyverse', 'zoo', 'chron'))

library(tidyverse)
library(zoo) # rollmean
library(chron) # is.weekend

## Your R needs to be set up to read data directly from the internet. This involves 
## setting your proxy. I can give advice on this if you need help. 
cases.url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv'
deaths.url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv'
state.pop.url <- 'https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv'
state.pop.filepath <- 'C:\\Users\\f1kxf04\\Documents\\rstats\\covid19\\state-pop-co-est2019-alldata.csv'

cases0 <- read_csv(cases.url)
deaths0 <- read_csv(deaths.url)

# read in census data so we can build 'cases per 100,000' type charts.
state.pop <- read_csv(state.pop.filepath) %>%  # can use state.pop.url or state.pop.filepath
  filter(SUMLEV == '040') %>%
  select(STNAME, POPESTIMATE2019) %>%
  add_row(STNAME = 'Puerto Rico', POPESTIMATE2019 = 3193694)

ma.window <- 7  # moving average window
days.to.look.back <- 14 

dropstates <- c('American Samoa', 'Diamond Princess', 'Grand Princess', 
                'Guam', 'Northern Mariana Islands', 'Virgin Islands')

## I want the 6th District states at the top of the All States plot
sixthdistrict <- c('Alabama', 'Florida', 'Georgia', 'Louisiana', 'Mississippi', 'Tennessee')

cases <- cases0 %>%
  pivot_longer(contains('/'), values_to = 'totalcases', names_to = 'date') %>%
  group_by(Province_State) %>%
  mutate(cases_per_day = totalcases - lag(totalcases)) %>%
  ungroup() %>%
  group_by(date, Province_State) %>%
  summarize(newcases = sum(cases_per_day)) %>%
  ungroup() %>%
  mutate(date = as.Date(date, format = '%m/%d/%y')) %>%
  rename(state = Province_State) %>%
  arrange(date) %>%
  group_by(state) %>%
  mutate(rolling = zoo::rollmean(newcases, ma.window, fill=NA, align='right')) %>%
  filter(!(state %in% dropstates)) %>%
  mutate(newcases = if_else(newcases < 0, 0, newcases)) 


us.minus.sixth <- unique(cases$state)[!(unique(cases$state) %in% sixthdistrict)]

us.sixth.first <- c(sixthdistrict, us.minus.sixth)  
  
  
cases <- cases %>%
  mutate(state_f = factor(state, levels = us.sixth.first))
  
#####################################################################################################
### total US cases
cases.us <- cases %>%
  group_by(date) %>%
  summarize(daily.new.cases = sum(newcases)) %>%
  ungroup() %>%
  mutate(rolling = zoo::rollmean(daily.new.cases, ma.window, fill=NA, align='right'))
  
ggplot(cases.us %>% filter(date >= as.Date('2020-02-29')), aes(date, daily.new.cases)) +
  geom_col(fill = 'grey80') +
  geom_line(aes(y=rolling), size = 2, color = 'indianred') +
  labs(title = 'New US cases by day, since Feb 29, 2020',
       subtitle = paste0(format(Sys.Date(), '%b %d, %Y'),'. Gray bars represent the number of daily new cases. Red line indicates the seven-day moving average.'),
       caption = paste0('Source: COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE)',
                        ' at Johns Hopkins University\nhttps://github.com/CSSEGISandData/COVID-19\n')) + 
  ylab('New cases') + xlab('') +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %d,\n%Y') +
  geom_hline(yintercept = cases.us$rolling[length(cases.us$rolling)], linetype = 2 ) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank())

#sum(cases.us$rolling > cases.us$rolling[length(cases.us$rolling)], na.rm = TRUE)

#####################################################################################################
## Show the new cases per day for one state. Type the state name into the function 
## plot.new.cases.state

plot.new.cases.state <- function(x, dd = 30){
  #newstate <- cases %>% filter(state == x & date >= as.Date('2020-02-29'))
  newstate <- cases %>% filter(state == x & date >= Sys.Date() - dd)
  pct.change <- round(100*(newstate$rolling[dim(newstate)[1]] - newstate$rolling[1])/newstate$rolling[1], 0)
  ggplot(newstate, aes(date, newcases)) +
    geom_col(fill = 'grey80') +
    geom_line(aes(y=rolling), size = 2, color = 'indianred') +
    labs(title = paste0('New cases by day in ', first(newstate$state), '. Last ',dd, ' days.'),
         subtitle = paste0(pct.change, ' percent change in 7-day average over the last ', dd, ' days. Red line represents seven-day moving average.'),
         caption = paste0('Source: COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE)',
                          ' at Johns Hopkins University\nhttps://github.com/CSSEGISandData/COVID-19\n')) + 
    ylab('New cases') + xlab('') +
    theme_bw() 
}

from.the.start <- Sys.Date() - as.Date('2020-02-29')
## type the state here and see the plot
#plot.new.cases.state('District of Columbia', 14)
plot.new.cases.state('Georgia', 14)
plot.new.cases.state('California', 14)
plot.new.cases.state('Virginia', 60)
plot.new.cases.state('Massachusetts', from.the.start) #+ ylim(0, 2500)
plot.new.cases.state('Massachusetts', 30) 
plot.new.cases.state('Massachusetts', 14)


plot.new.cases.state('New York')
plot.new.cases.state('Florida')
plot.new.cases.state('Texas')

#####################################################################################################


#####################################################################################################
### This plot shows all the states, going back 14 days (or whatever number you set
### the variable days.to.look.back). 
### WARNING: This plot takes a moment to load and it looks best if you make your 
### plot window about the same dimensions as a piece of paper in portrait mode. 
cases %>% filter(date >= Sys.Date()-days.to.look.back) %>%
  ggplot(aes(date, newcases)) +
  geom_col(fill = 'grey80') +
  geom_line(aes(y=rolling), size = 1, color = 'indianred', linetype = 1) +
  xlab('') + ylab('') +
  labs(title = paste0('Daily new cases over the past ', days.to.look.back, ' days'), 
       subtitle = paste0(format(Sys.Date(), '%b %d, %Y'),'. Gray bars represent the number of daily new cases.', 
                         ' Red lines show the seven-day moving average.\nSixth District states in top row.'),
       caption = paste0('Source: COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE)',
                        ' at Johns Hopkins University\nhttps://github.com/CSSEGISandData/COVID-19\n')) + 
  theme_light() +
  facet_wrap(vars(state_f), ncol = 6, scales = 'free_y') +
  theme(strip.background = element_rect(fill='white'),
        strip.text = element_text(face='bold', color = 'gray30', hjust = 0),
        plot.margin = unit(c(0,1,0,0), 'cm'),
        panel.border = element_blank())

#ggsave('C:\\Users\\f1kxf04\\Documents\\rstats\\covid19\\new_cases.pdf', width = 10, height = 15, units = 'in')  
#####################################################################################################

#####################################################################################################
#### deaths -- plot of the total deaths in US, with a 7-day moving average line
deaths <- deaths0 %>%
  pivot_longer(contains('/'), values_to = 'totaldeaths', names_to = 'date') %>%
  group_by(date) %>%
  summarize(sum_totaldeaths = sum(totaldeaths)) %>%
  ungroup() %>%
  mutate(date = as.Date(date, format = '%m/%d/%y')) %>%
  arrange(date) %>%
  mutate(deaths_daily = sum_totaldeaths - lag(sum_totaldeaths),
         rolling = zoo::rollmean(deaths_daily, k = 7, fill=NA, align='right'),
         weekend = is.weekend(date)) 

deaths %>% 
  filter(date >= as.Date('2020-02-29')) %>%
  ggplot(aes(date, deaths_daily)) + 
  geom_col(fill = 'grey80') + 
  #geom_col(aes(fill = weekend)) + 
  #geom_line(aes(y=rolling), size = 2, color = 'indianred', linetype = 1) +
  geom_line(aes(y=rolling), size = 2, color = 'darkblue', linetype = 1) +
  geom_hline(yintercept = deaths$rolling[length(deaths$rolling)], linetype = 2 ) +
  labs(title = paste0('Number of daily deaths from COVID-19 -- United States -- ', format(deaths$date[nrow(deaths)], '%b %d, %Y')),
       subtitle = paste0('A total of ',  format(deaths$sum_totaldeaths[nrow(deaths)], big.mark = ','), 
                         ' deaths have been reported since February 29, 2020. (Blue line shows the seven-day moving average)'),
       caption = paste0('Source: COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE)',
                                  ' at Johns Hopkins University\nhttps://github.com/CSSEGISandData/COVID-19\n')) +
  xlab('') + ylab('Deaths') +
  theme_light() +
  scale_fill_manual(values = c('grey70', 'purple')) +
  scale_x_date(date_breaks = '1 month', date_labels = '%b %d,\n%Y') +
  theme(legend.position = c(0.05, 0.85),
        legend.background = element_rect(color = 'darkblue'),
        panel.grid.minor.x = element_blank())
#####################################################################################################