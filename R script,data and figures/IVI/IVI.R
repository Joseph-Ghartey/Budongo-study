dir()
Data<-read.csv('main.csv', header = TRUE, sep = ',')
data<-read.csv('spec_comp_plot_abun.csv', header = TRUE, sep = ',')
library(tidyverse)
library(BiodiversityR)
data
sapply(Data, class)
Data$plot.no<-as.factor(Data$plot.no)
Data$comp<-as.factor(Data$comp)
?importancevalue

IVI <- Data %>% as.data.frame() %>%
  mutate(count = rep(1, each = nrow(.))) %>% 
importancevalue.comp(site='plot.no', species='spec', count='count', 
                     basal='ba', factor='comp')


IVI
write.csv(IVI$N15, file = 'IVI_15.csv')
write.csv(IVI$N3, file = 'IVI_3.csv')

IVI_15<-IVI$N15
IVI_3<-IVI$N3

#save this into excel with two worksheet 
library(openxlsx)
IVI <-createWorkbook()
addWorksheet(IVI, 'IVI_15')
names(IVI)
writeData(IVI,'IVI_15', IVI_15)
saveWorkbook(IVI, 'IVI.xlsx')

IVI <- loadWorkbook('IVI.xlsx')
addWorksheet(IVI, 'IVI_3')
writeData(IVI, 'IVI_3',IVI_3 )
saveWorkbook(IVI, 'IVI.xlsx', overwrite = TRUE)

as.data.frame(IVI_15)
