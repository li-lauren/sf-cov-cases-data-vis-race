library('data.table')
library('lubridate')
library('dplyr')

dt = fread(file='Cases_Race_Final.csv')

colnames(dt) = c('date', 'race', 'new_cases', 'cum_cases', 'last_updated')

# Remove last_updated column
dt[, last_updated:=NULL]

# generate a month column based on the given dates
dt[, date:=as.Date(date)]

dt[, month:=sapply(date, FUN=function(x){month(x, label=TRUE, abbr=FALSE)})]

# Filter out January 2020 data, since it is incomplete at this time (1/16)
dt = dt[month != 'January']

# Group by month and race
# dt[, total_new_cases:=sum(new_cases), by=.(month, race)]

# Make new data table with monthly case totals keyed by month and race
dtt = dt[, .(total_new_cases = sum(new_cases)), by=.(month, race)]

# Make new data with SF montly case totals 
sf = dtt[, .(total_new_cases=sum(total_new_cases), race='SF'), by=month]

# rbind dtt and SF
dtt = rbind(dtt, sf)

dtt = dtt[!(race %in% c('Other', 'Unknown'))]

# make populations data table with SF demographics
pop_list = list('Asian' = 0.36, 'SF' = 1, 'Hispanic or Latino' = 0.152, 
'Native Hawaiian or Other Pacific Islander' = 0.005, 'White' = 0.402, 
'Black or African American' = 0.056, 'Native American' = 0.007, 'Multi-racial' = 0.045)

# add populations column as pop_list[race]
dtt[, population:=sapply(race, FUN=function(x){pop_list[[x]]})]

# add new_cases_per column (total_new_cases) / (popultion * total SF population)
total_SF_pop = 881.549 #(in thousands)

dtt[, new_cases_per:=total_new_cases/(population*total_SF_pop)]

# round new_cases_per to two sig figs
dtt[, new_cases_per:=signif(new_cases_per,2)]

# reshape the data.table into (race ~ month) form, with value.var = new_cases_per
w = dcast(dtt, formula='race~month', value.var='new_cases_per', fill=0)
#w = as.data.table(w)
w = rename(w, Race=race)

# pipe final data.table into CSV
write.csv(w, "monthly_cases_by_race.csv", row.names=FALSE, quote=FALSE)


