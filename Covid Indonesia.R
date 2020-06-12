# As this an ongoing situation, frequent changes in the data format may occur, please visit the package news to get updates about those changes
install.packages("coronavirus")
library(coronavirus)
# library(digest)
# Install the Github version (refreshed on a daily bases):
# install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus")

#1. Input dan apa yang mau dicari dari data
#2. Total keseluruhan kasus confirm di indonesia
#3. Total keseluruhan kasus di indonesia per hari
#4. Membuat vis kasus kumulatif konfirmed
#5. Membuat vis dari nomor 3
#6. Persentase ratio dari death and recovery

# Dataset coronavirus #
#1. Input dan apa yang mau dicari dari data
library(coronavirus)
update_dataset()

data("coronavirus")
coronavirus
head(coronavirus) # data teratas
tail(coronavirus) # data terbawah

library(dplyr)
glimpse(coronavirus)
summary(coronavirus)

# Mengambil data negara indonesia dengan filter == 'Indonesia'
cases_indonesia <- coronavirus %>% #data coronavirus
  filter(country =='Indonesia') %>% #Difilter berdasar Indonesia
  select(date, country, type, cases) %>% #Mengambil col date, country, type and cases
  group_by(date) 
View(cases_indonesia)

#2. Total keseluruhan kasus confirm di indonesia 
# Membuat pivot total kejadian di Indonesia
library(tidyr)
pivot_cases_all_indonesia <- cases_indonesia %>%
  filter(date >= '2020-03-01') %>%
  group_by (country, type) %>%
  summarize (total = sum(cases)) %>%
  pivot_wider (names_from = type,
               values_from = total) %>%
  arrange (country) %>%
  ungroup() %>%
  mutate(active = confirmed - death)
View(pivot_cases_all_indonesia)

#3. Total keseluruhan kasus di indonesia per hari
# Membuat pivot total kejadian di Indonesia berdasarkan hari
pivot_cases_indonesia_day <- cases_indonesia %>%
  filter(date >= '2020-03-01') %>%
  group_by (date, type) %>%
  summarize (total = sum(cases)) %>%
  pivot_wider (names_from = type,
               values_from = total) %>%
  arrange (date) %>%
  ungroup() %>%
  mutate(active = confirmed - death,
         cum_active = cumsum(active),
         cum_confirm = cumsum(confirmed),
         cum_death = cumsum(death),
         cum_recovered = cumsum(recovered))
View(pivot_cases_indonesia_day)

df <- as.data.frame(pivot_cases_indonesia_day)

#4. Membuat vis kasus kumulatif konfirmed
# Trend Line Indonesia
# Using pivot_cases_indonesia_day, draw a line plot cum_confirm vs. date
# Add a smooth trend line using linear regression, no error bars
library(ggplot2)
ggplot(df, aes (x=date, y=cum_confirm))+
  geom_line()+
  geom_smooth(method = 'lm', se = FALSE) +
  ylab ('Total Cases')

# Vis berdasar data kumulatif perhari terkonfirmasi corona
ggplot(df) + 
  geom_line(aes(x=date, y=cum_confirm, color = cum_confirm)) + 
  ylab("Confirmed cases")+
  xlab("Date")

# Vis berdasarkan kasus per hari yang terupadate
ggplot(cases_indonesia) + 
  geom_line(aes(x=date, y=cases, group = type, color = type)) + 
  ylab("Confirmed cases")+
  xlab("Date")

# Make plot
library(plotly)
df %>%
plot_ly( x = ~date, y= ~cum_confirm,
        name = 'Confirm', type= 'scatter',
        mode = 'line+marker') %>%
        add_trace(y = ~cum_recovered, name = 'Recovered',fillcolor = '#E41317') %>%
        add_trace(y = ~cum_death, name = 'Death',fillcolor = 'forestgreen')%>%
        add_trace(y = ~cum_active, name = 'Active cases',fillcolor = ' ')%>%
        layout ( title = "Total Cases Covid 19 Indonesia",
           legend = list (x = 0.1, y = 0.9),
           yaxis = list(title = "Total"),
           xaxis = list(title = "Date")) %>%
        add_annotations(x = ('2020-03-02'), y = 1,
                        text = paste('Kasus Pertama Diketahui'), 
                        xref = 'x',
                        yref = 'y',
                        arrowhead = 5,
                        arrowhead = 3,
                        arrowsize = 1,
                        showarrow = TRUE,
                        ax = -10,
                        ay = -50 ) %>%
  add_annotations(x = ('2020-06-05'), y = '29521',
                  text = paste('29.521 confirm cases'), 
                  xref = 'x',
                  yref = 'y',
                  arrowhead = 1,
                  arrowhead = 1,
                  arrowsize = 1,
                  showarrow = TRUE,
                  ax = -0.5,
                  ay =-20) %>%
  add_annotations(x = ('2020-06-05'), y = '1770',
                  text = paste('1.770 death cases'), 
                  xref = 'x',
                  yref = 'y',
                  arrowhead = 1,
                  arrowhead = 1,
                  arrowsize = 1,
                  showarrow = TRUE,
                  ax = -0.5,
                  ay =-20) %>%
  add_annotations(x = ('2020-06-05'), y = '9443',
                  text = paste('9.443 recovery cases'), 
                  xref = 'x',
                  yref = 'y',
                  arrowhead = 1,
                  arrowhead = 1,
                  arrowsize = 1,
                  showarrow = TRUE,
                  ax = -0.5,
                  ay =-20) %>%
  add_annotations(x = ('2020-06-05'), y = '27751',
                  text = paste('27.751 active cases'), 
                  xref = 'x',
                  yref = 'y',
                  arrowhead = 1,
                  arrowhead = 1,
                  arrowsize = 1,
                  showarrow = TRUE,
                  ax = -0.5,
                  ay =-20)

#6. Persentase ratio dari death and recovery
ratio <- df %>%
              group_by(date) %>%
              summarise(death = sum(cum_death), confirmed = sum(cum_confirm),recovered = sum(cum_recovered)) %>%
              mutate(recov_rate = 100*(recovered/confirmed))%>%
              mutate(death_rate = 100*(death/confirmed))
# Karena terdapat Na pada baris 1, maka dihilangkan
Ratio <- na.omit(ratio) # menghilangkan Na

head(Ratio,5) %>% filter(date >= 2020-03-10)
tail(Ratio,5) %>% filter(date >= 2020-03-10)

ggplot(Ratio) +
    geom_line(aes(x=date, y= death_rate, color = 'death_rate')) +
    geom_line(aes(x=date, y= recov_rate, color = 'recov_rate')) +
    labs(x = "", y = 'Rate', title = 'Ratio of Death and Recovered',
       subtitle = 'Indonesia') 

# Case another country
library(tidyr)
pivot_cases_by_country <- coronavirus %>%
  filter( type== 'confirmed') %>%
  group_by (country, date) %>%
  summarise (total = sum(cases)) %>%
  pivot_wider (names_from = country,
               values_from = total)

pivot_cases_by_country %>%
  plot_ly( x = ~date, y= ~Indonesia,
           name = 'Indonesia', type= 'scatter',
           mode = 'lines+markers') %>%
  add_trace(y= ~Singapore,
            name = 'Singapore', type= 'scatter',
            mode = 'lines+markers') %>%
  add_trace(y= ~Vietnam,
            name = 'Vietnam', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Thailand,
            name = 'Thailand', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Malaysia,
            name = 'Malaysia', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Philippines,
            name = 'Philippines', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Brunei,
            name = 'Brunei', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Cambodia,
            name = 'Kamboja', type= 'scatter',
            mode = 'lines+markers')%>%
  layout ( title = "Kasus Terkonfirmasi",
           legend = list (x = 0.1, y = 1),
           yaxis = list(title = "Kasus positif baru"),
           xaxis = list(title = "Tanggal"))

# Case another country
library(tidyr)
pivot_cases_by_country_death<- coronavirus %>%
  filter(type== 'death') %>%
  group_by (country, date) %>%
  summarise (total = sum(cases)) %>%
  pivot_wider (names_from = country,
               values_from = total)

pivot_cases_by_country_death %>%
  plot_ly( x = ~date, y= ~Indonesia,
           name = 'Indonesia', type= 'scatter',
           mode = 'lines+markers') %>%
  add_trace(y= ~Singapore,
            name = 'Singapore', type= 'scatter',
            mode = 'lines+markers') %>%
  add_trace(y= ~Vietnam,
            name = 'Vietnam', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Thailand,
            name = 'Thailand', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Malaysia,
            name = 'Malaysia', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Philippines,
            name = 'Philippines', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Brunei,
            name = 'Brunei', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Cambodia,
            name = 'Kamboja', type= 'scatter',
            mode = 'lines+markers')%>%
  layout ( title = "Kasus Kematian",
           legend = list (x = 0.1, y = 1),
           yaxis = list(title = "Kasus Kematian"),
           xaxis = list(title = "Tanggal"))

# Case another country
library(tidyr)
pivot_cases_by_country_recov<- coronavirus %>%
  filter(type== 'recovered') %>%
  group_by (country, date) %>%
  summarise (total = sum(cases)) %>%
  pivot_wider (names_from = country,
               values_from = total)

pivot_cases_by_country_recov %>%
  plot_ly( x = ~date, y= ~Indonesia,
           name = 'Indonesia', type= 'scatter',
           mode = 'lines+markers') %>%
  add_trace(y= ~Singapore,
            name = 'Singapore', type= 'scatter',
            mode = 'lines+markers') %>%
  add_trace(y= ~Vietnam,
            name = 'Vietnam', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Thailand,
            name = 'Thailand', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Malaysia,
            name = 'Malaysia', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Philippines,
            name = 'Philippines', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Brunei,
            name = 'Brunei', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~Cambodia,
            name = 'Kamboja', type= 'scatter',
            mode = 'lines+markers')%>%
  layout ( title = "Kasus Sembuh",
           legend = list (x = 0.1, y = 1),
           yaxis = list(title = "Kasus Sembuh"),
           xaxis = list(title = "Tanggal"))

# Confirmed
pivot_confirmed_by_country <- coronavirus %>%
  filter(type== 'confirmed') %>%
  group_by (country, date) %>%
  summarise (total = sum(cases)) %>%
  pivot_wider (names_from = country,
               values_from = total) %>%
  ungroup() %>%
  mutate( confirmed_indo = cumsum(Indonesia),
          confirmed_sg = cumsum(Singapore),
          confirmed_malay = cumsum(Malaysia),
          confirmed_viet = cumsum(Vietnam),
          confirmed_thai = cumsum(Thailand),
          confirmed_brunei = cumsum(Brunei),
          confirmed_kambj = cumsum(Cambodia),
          confirmed_filip = cumsum(Philippines)
          )
confirmed_indo <- pivot_confirmed_by_country$confirmed_indo
confirmed_sg <- pivot_confirmed_by_country$confirmed_sg
confirmed_malay <- pivot_confirmed_by_country$confirmed_malay
confirmed_viet <- pivot_confirmed_by_country$confirmed_viet
confirmed_thai <- pivot_confirmed_by_country$confirmed_thai
confirmed_brunei <- pivot_confirmed_by_country$confirmed_brunei
confirmed_kambj <- pivot_confirmed_by_country$confirmed_kambj
confirmed_filip <- pivot_confirmed_by_country$confirmed_filip

# Death
pivot_death_by_country <- coronavirus %>%
  filter(type== 'death') %>%
  group_by (country, date) %>%
  summarise (total = sum(cases)) %>%
  pivot_wider (names_from = country,
               values_from = total) %>%
  ungroup() %>%
  mutate( death_indo = cumsum(Indonesia),
          death_sg = cumsum(Singapore),
          death_malay = cumsum(Malaysia),
          death_viet = cumsum(Vietnam),
          death_thai = cumsum(Thailand),
          death_brunei = cumsum(Brunei),
          death_kambj = cumsum(Cambodia),
          death_filip = cumsum(Philippines)
          )

death_indo <- pivot_death_by_country$death_indo
death_sg <- pivot_death_by_country$death_sg
death_malay <- pivot_death_by_country$death_malay
death_viet <- pivot_death_by_country$death_viet
death_thai <- pivot_death_by_country$death_thai
death_brunei <- pivot_death_by_country$death_brunei
death_kambj <- pivot_death_by_country$death_kambj
death_filip <- pivot_death_by_country$death_filip

# Recovery
pivot_recov_by_country <- coronavirus %>%
  filter(type== 'recovered') %>%
  group_by (country, date) %>%
  summarise (total = sum(cases)) %>%
  pivot_wider (names_from = country,
               values_from = total) %>%
  ungroup() %>%
  mutate( recov_indo = cumsum(Indonesia),
          recov_sg = cumsum(Singapore),
          recov_malay = cumsum(Malaysia),
          recov_viet = cumsum(Vietnam),
          recov_thai = cumsum(Thailand),
          recov_brunei = cumsum(Brunei),
          recov_kambj = cumsum(Cambodia),
          recov_filip = cumsum(Philippines)
  )

recov_indo <- pivot_recov_by_country$recov_indo
recov_sg <- pivot_recov_by_country$recov_sg
recov_malay <- pivot_recov_by_country$recov_malay
recov_viet <- pivot_recov_by_country$recov_viet
recov_thai <- pivot_recov_by_country$recov_thai
recov_brunei <- pivot_recov_by_country$recov_brunei
recov_kambj <- pivot_recov_by_country$recov_kambj
recov_filip <- pivot_recov_by_country$recov_filip

date <- dfw$date
df_ratio <- data.frame(cbind(date,confirmed_indo,confirmed_malay,confirmed_sg,confirmed_thai,confirmed_viet,confirmed_brunei,confirmed_filip,confirmed_kambj,
                                death_indo,death_malay,death_sg,death_thai,death_viet,death_brunei,death_filip,death_kambj,
                                recov_indo,recov_malay,recov_sg,recov_thai,recov_viet,recov_brunei,recov_filip,recov_kambj))
df_ratio$date <- as.Date(date)
df_ratio

ratioo <- df_ratio %>%
  group_by(date) %>%
  mutate(recov_rate_indo = 100*(recov_indo/confirmed_indo))%>%
  mutate(recov_rate_sg = 100*(recov_sg/confirmed_sg))%>%
  mutate(recov_rate_malay = 100*(recov_malay/confirmed_malay))%>%
  mutate(recov_rate_viet = 100*(recov_viet/confirmed_viet))%>%
  mutate(recov_rate_thai = 100*(recov_thai/confirmed_thai))%>%
  mutate(recov_rate_brunei = 100*(recov_brunei/confirmed_brunei))%>%
  mutate(recov_rate_kambj = 100*(recov_kambj/confirmed_kambj))%>%
  mutate(recov_rate_filip = 100*(recov_filip/confirmed_filip))%>%
  mutate(death_rate_indo = 100*(death_indo/confirmed_indo))%>%
  mutate(death_rate_sg = 100*(death_sg/confirmed_sg))%>%
  mutate(death_rate_malay = 100*(death_malay/confirmed_malay))%>%
  mutate(death_rate_viet = 100*(death_viet/confirmed_viet))%>%
  mutate(death_rate_thai = 100*(death_thai/confirmed_thai))%>%
  mutate(death_rate_brunei = 100*(death_brunei/confirmed_brunei))%>%
  mutate(death_rate_kambj = 100*(death_kambj/confirmed_kambj))%>%
  mutate(death_rate_filip = 100*(death_filip/confirmed_filip))


#Terdapat beberapa data Na
ratioo <- na.omit(ratioo)

ratiooo <- as.data.frame(ratioo)

str(ratiooo)

ratiooo %>%
  plot_ly( x = ~date, y= ~recov_rate_indo,
           name = 'Indonesia', type= 'scatter',
           mode = 'lines+markers') %>%
  add_trace(y= ~recov_rate_sg,
            name = 'Singapore', type= 'scatter',
            mode = 'lines+markers') %>%
  add_trace(y= ~recov_rate_viet,
            name = 'Vietnam', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~recov_rate_thai,
            name = 'Thailand', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~recov_rate_malay,
            name = 'Malaysia', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~recov_rate_filip,
            name = 'Philippines', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~recov_rate_brunei,
            name = 'Brunei', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~recov_rate_kambj,
            name = 'Kamboja', type= 'scatter',
            mode = 'lines+markers')%>%
  layout ( title = "Ratio Recovery",
           legend = list (x = 0.11, y = 1),
           yaxis = list(title = "Ratio Recovery"),
           xaxis = list(title = "Date"))

ratiooo %>%
  plot_ly( x = ~date, y= ~death_rate_indo,
           name = 'Indonesia', type= 'scatter',
           mode = 'lines+markers') %>%
  add_trace(y= ~death_rate_sg,
            name = 'Singapore', type= 'scatter',
            mode = 'lines+markers') %>%
  add_trace(y= ~death_rate_viet,
            name = 'Vietnam', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~death_rate_thai,
            name = 'Thailand', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~death_rate_malay,
            name = 'Malaysia', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~death_rate_filip,
            name = 'Philippines', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~death_rate_brunei,
            name = 'Brunei', type= 'scatter',
            mode = 'lines+markers')%>%
  add_trace(y= ~death_rate_kambj,
            name = 'Kamboja', type= 'scatter',
            mode = 'lines+markers')%>%
  layout ( title = "Ratio Death",
           legend = list (x = 5, y = 1),
           yaxis = list(title = "Ratio Death"),
           xaxis = list(title = "Date"))

# Kumulatif asia tenggara
confirmed_indo <- pivot_confirmed_by_country$confirmed_indo
confirmed_sg <- pivot_confirmed_by_country$confirmed_sg
confirmed_malay <- pivot_confirmed_by_country$confirmed_malay
confirmed_viet <- pivot_confirmed_by_country$confirmed_viet
confirmed_thai <- pivot_confirmed_by_country$confirmed_thai
confirmed_brunei <- pivot_confirmed_by_country$confirmed_brunei
confirmed_kambj <- pivot_confirmed_by_country$confirmed_kambj
confirmed_filip <- pivot_confirmed_by_country$confirmed_filip

date <- dfw$date
Cum_cases <- as.data.frame(cbind(date,confirmed_brunei,confirmed_filip,confirmed_indo,confirmed_kambj,confirmed_malay,confirmed_sg,confirmed_viet,confirmed_thai))
Cum_cases$date <- as.Date(date)

Cum_cases %>%
  plot_ly( x = ~date, y= ~confirmed_indo,
           name = 'Indonesia', type= 'scatter',
           mode = 'lines+markers') %>%
  add_trace(y = ~confirmed_sg, name = 'Singapura',mode = 'lines+markers')%>%
  add_trace(y = ~confirmed_viet, name = 'Vietnam',mode = 'lines+markers')%>%
  add_trace(y = ~confirmed_thai, name = 'Thailand',mode = 'lines+markers')%>%
  add_trace(y = ~confirmed_malay, name = 'Malaysia',mode = 'lines+markers')%>%
  add_trace(y = ~confirmed_filip, name = 'Filipina',mode = 'lines+markers')%>%
  add_trace(y= ~confirmed_brunei,name = 'Brunei', mode = 'lines+markers') %>%
  add_trace(y = ~confirmed_kambj, name = 'Kamboja',mode = 'lines+markers')%>%
    layout ( title = "Total Cases Covid 19",
           legend = list (x = 0.1, y = 0.9),
           yaxis = list(title = "Total"),
           xaxis = list(title = "Date"))
