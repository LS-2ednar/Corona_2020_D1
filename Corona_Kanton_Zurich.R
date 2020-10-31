#T1.1 Download Data form https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/age_distribution_zh.csv

#T2.1 
data = read.delim('age_distribution_zh.csv', sep = ',', dec = '.', header = TRUE , na.strings=c("","NA"), stringsAsFactors=FALSE); data
data$region = 'Kanton Zuerich' # T4.1

#T2.2 - T2.4
str(data)
library(mice)
md.pattern(data) # Data is tidy

#T5.1
meanval   = mean(data$cases_pp)
medianval = median(data$cases_pp)
varval    = var(data$cases_pp)
minval    = min(data$cases_pp)
maxval    = max(data$cases_pp)
qartiles  = quantile(data$cases_pp, probs = seq(0,1,0.25), type = 3)

#T5.2
library(ggplot2)
#Calculate iqr, lower-, and upper-fence
iqr      = qartiles[4] - qartiles[2]; iqr 
lowfence = qartiles[2]-1.5*iqr
upfence  = qartiles[4]+1.5*iqr
#create boxplot
box = ggplot(data = data, aes(y = cases_pp))+ geom_boxplot()
#modify boxplot with errorbar and upper and lower fences
box = box + geom_boxplot(outlier.size = 1, outlier.colour = 'red') + stat_boxplot(geom = 'errorbar', ymax = upfence, ymin = lowfence, width  = 0.5)
box + theme(text = element_text(size =14), axis.title.x=element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank())
#determin skewness
#install.packages('moments')
library('moments')
skewness(data$cases_pp)

#T6.1 -6.6
library('gridExtra')
library('cowplot')
#Overall distribution of cases according to age with different colors
g1 = ggplot(data = data, aes(x = age, y = cases, color = sex )) + 
  geom_point() +
  theme(legend.position = 'none', panel.background = element_blank(), 
        panel.grid.major = element_line(colour = 'gray', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = 'lightgray', linetype = 'dashed'))
g1

#boxplot comparison of male and female
cases_qartile = quantile(data$cases, pprobs = seq(0,0.25,1), type = 3)
iqr      = cases_qartile[4] - cases_qartile[2]; iqr 
lowfence = cases_qartile[2]-1.5*iqr
upfence  = cases_qartile[4]+1.5*iqr

g2 = ggplot(data = data, aes(y = cases, color = sex )) + geom_boxplot(outlier.size = 1) + 
  stat_boxplot(geom = 'errorbar')  +
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.position = 'none', 
        panel.background = element_blank(),
        panel.grid.major = element_line(colour = 'gray', linetype = 'dashed')) + 
  coord_cartesian(ylim = c(-0, 50))
g2

#Look at cases per age and sex as stacked bars
g3 = ggplot(data = data, aes(y=cases, x = age, fill = sex)) +
  geom_bar(position = 'stack', stat = 'identity') + 
  theme(panel.background = element_blank(), 
        panel.grid.major = element_line(colour = 'gray', linetype = 'dashed'),
        panel.grid.minor = element_line(colour = 'lightgray', linetype = 'dashed'))
g3

#create pie chart from new dataframe
library('scales')
sex = c('Male','Female')
cases = c((sum(data$cases[which(data$sex == 'Male')])/sum(data$cases)),(sum(data$cases[which(data$sex == 'Female')]))/sum(data$cases))
newdataframe = cbind(sex, cases)
str(newdataframe) #no Dataframe
#make the a newtable called  newdataframe
newdataframe = as.data.frame(newdataframe)
newdataframe$cases = as.numeric(newdataframe$cases)
str(newdataframe) #now it is a dataframe

g4 = ggplot(newdataframe, aes(x = '', y = cases, fill = sex)) +
  geom_bar(width = 1, stat = 'identity') + coord_polar("y") + 
  theme_minimal() +
  theme(axis.text.x=element_blank(), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        panel.border = element_blank(), 
        panel.grid=element_blank(),
        axis.ticks = element_blank(), 
        plot.title = element_text(size = 10)) + 
  ggtitle('                Cases Distribution') + # spaces added for final plot
  geom_text(aes(y = cases/2 + c(0, cumsum(cases)[-length(cases)]), label = percent(cases)), size=4) 
g4

#T6.6 Create final plot and save it as pdf
library('grid')
plt= grid.arrange(g1,g2,g3+theme(legend.position = 'none'),g4, ncol = 2, nrow = 2, top=textGrob("Female vs Male Covid-19 cases is Kanton Zurich", gp=gpar(fontsize=15)))
plt
ggsave(plot = plt, 'Plot.pdf', units = c('mm'),width = 210, height = 148, dpi = 320)