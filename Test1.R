#DESCRIPTION:------------------------------------------------------------------------------------------
#Monthly data
#Step 1: fit all data to a cyclic regression model and calculate upper bound of 95%CI, values above upr
#are considered as epidemic period and excluded from data set
#Step 2: fit new data set (with NAs) to another cyclic regression model and calculate 95% upr. Values above
#this limit are considered as excess mortality
#Step 3: Where excess mortality > 0, and time period falls into the intervals between Oct of a year to March
#of the following year is our interest. Total excess mortality over an interval are summed together.
#Step 4: Calculate denominator (person-time), sum values corresponding to interval in Step 3
#(condition: where excess mortality > 0 & epi==1 & ((1<= nperiod <=3) | (10 <= nperiod <=12)))
#Step 5: Calculate excess mortality rate (per 100,000 persons-year), divide obtained result in Step 3 by Step 4
#Step 6: Visualize result by age-specific (young- 60&70; old- 80&90) and edu-specific

####
rm(list=ls())
# library("xlsx")
# library("gamlss")
library("ggplot2")
library("reshape2")
library("dplyr")


data1=read.csv("/Users/dinhngoclinh/Dropbox/For PhD research/Code/R/Flu&PI-Spain/Shared folder/alldeaths.month.csv")
data2=read.csv("/Users/dinhngoclinh/Dropbox/For PhD research/Code/R/Flu&PI-Spain/Shared folder/influenza.month.csv")
data3=read.csv("/Users/dinhngoclinh/Dropbox/For PhD research/Code/R/Flu&PI-Spain/Shared folder/persons.year.month.csv")

omega1=(2*3.14159265)/12
omega2=(4*3.14159265)/12

cos1 = cos(omega1*data2$nperiod)
sin1 = sin(omega1*data2$nperiod)
cos2 = cos(omega2*data2$nperiod)
sin2 = sin(omega2*data2$nperiod)
square = (data2$nperiod/100)^2

ily= data2$il60 + data2$il70
ilo= data2$il80 + data2$il90
bay= data2$basic60 + data2$basic70
bao= data2$basic80 + data2$basic90
pry= data2$pri60 + data2$pri70
pro= data2$pri80 + data2$pri90
sty= data2$st60 + data2$st70
sto= data2$st80 + data2$st90

mydata2 = as.data.frame(cbind(data2$nperiod, data2$month, ily, ilo, bay, bao, pry, pro, sty, sto, data2$all, cos1, sin1, cos2, sin2, square))
colnames(mydata2)[c(1,2,11)] = c("nperiod", "month", "all")


###traditional approach----------------------------------------------------------------------------------
mod0=lm(all~nperiod + square + cos1 + sin1 + cos2 +sin2, data=mydata2)
predicted = predict(mod0, interval="confidence", level=0.95)
mydata2$upr=predicted[,3]
mydata2$epi=ifelse(mydata2$all>mydata2$upr, 1, 0) # epi=1 if all>fitted, epi=0 if all<fitted


trans.data=mydata2

t(cbind(names(trans.data)))

#  [,1]      [,2]    [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9]  [,10] [,11]
#  "nperiod" "month" "ily" "ilo" "bay" "bao" "pry" "pro" "sty" "sto" "all"
#  [,12]  [,13]  [,14]  [,15]  [,16]    [,17] [,18]
#  "cos1" "sin1" "cos2" "sin2" "square" "upr" "epi"

## anterior 3:11 (error!)
results=matrix( nrow=12,  ncol=0)
for (j in 3:11){
  trans.data[,j]=ifelse(trans.data$epi==1, NA, trans.data[,j]) # epi=0, discard observed ILI (all); epi=1, keep!
  
  mod1=lm(trans.data[,j]~nperiod + square + cos1 + sin1 + cos2 +sin2, data=trans.data)
  predicted1 = predict(mod1, newdata=trans.data, interval="confidence", level=0.95)

  out=cbind(subset(mydata2, select=-c(upr)), predicted1[,c(1,3)])
  out$excess.raw=out[,j] - out$upr
  out$excess= ifelse(((out$month>=1 & out$month<=3)|(out$month>=10 & out$month <=12)) & out$excess.raw >0, out$excess.raw, 0)
  out$epi.fn= ifelse(out$excess>0, 1, 0)
  data3$epi=out$epi.fn      ### !!!!! Here is an error in the correspondence with the populations
                            ### !!!!! data3 does not have the same structure as trans.data.
                            ### !!!!! column number "j" does not match !!!! There is an error at least with my data
  data3$base= ifelse(data3$epi==1, data3[,j], 0)
  result.group=matrix(, nrow=0, , ncol=1)
  for (i in 1:12){
    excess.season= sum(out$excess[(12*i-2):(12*i+3)])
    base.season= sum(data3$base[(12*i-2):(12*i+3)])
    excess.rate= excess.season*100000/base.season
    result.group=rbind(result.group,excess.rate)
  }
  results=cbind(results, result.group)
}
dim(results)
rownames(results)=c(1:12)

colnames(results)=c("eily", "eilo", "ebay", "ebao", "epry", "epro", "esty", "esto", "eall")
season=1:12
results=as.data.frame(cbind(results, season))

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
results[is.nan(results)] = 0

###visualize data-----------------------------------------------------------------------------------

#visualize epidemic threshold
i=1:12
x= 12*i-2
y= 12*i+3
a=c(x,y)


pdf(file='graph/Test1_PossibleError.pdf', height =   8.3,   width =  11.7)
str(mydata2)
# 'data.frame':	149 obs. of  18 variables:
#  $ nperiod: num  1 2 3 4 5 6 7 8 9 10 ...
#     year     ??
#  $ month  : num  1 2 3 4 5 6 7 8 9 10 ... 
#     measure  ??
#  $ ily    : num  1 1 1 0 0 0 0 0 1 0 ...
#  $ ilo    : num  2 4 3 2 3 1 4 3 1 0 ...
#  $ bay    : num  0 1 0 0 1 0 1 3 0 3 ...
#  $ bao    : num  5 5 3 8 3 3 1 1 1 2 ...
#  $ pry    : num  2 0 0 0 1 0 1 1 1 1 ...
#  $ pro    : num  2 3 1 0 2 1 2 0 0 2 ...
#  $ sty    : num  1 0 0 0 0 0 0 0 0 0 ...
#  $ sto    : num  2 0 1 0 0 0 2 0 1 1 ...
#  $ all    : num  15 14 9 10 10 5 11 8 5 9 ...   <----------------- all
#  $ cos1   : num  8.66e-01 5.00e-01 1.79e-09 -5.00e-01 -8.66e-01 ...
#  $ sin1   : num  0.5 0.866 1 0.866 0.5 ...
#  $ cos2   : num  0.5 -0.5 -1 -0.5 0.5 ...
#  $ sin2   : num  8.66e-01 8.66e-01 3.59e-09 -8.66e-01 -8.66e-01 ...
#  $ square : num  0.0001 0.0004 0.0009 0.0016 0.0025 0.0036 0.0049 0.0064 0.0081 0.01 ...
#  $ upr    : num  20.13 19.56 15.3 10.69 8.76 ...
#  $ epi    : num  0 0 0 0 1 0 1 0 0 1 ...

mydata2 %>% select(nperiod, all, epi) %>% melt(id=c("nperiod", "epi")) %>% rename(Type = variable, Deaths = value) %>% 
  mutate(epi=factor(epi)) %>%
  ggplot(aes(x=nperiod, y=Deaths, group=Type)) +
  geom_line(aes(color=epi))+ geom_vline(xintercept=a, colour="grey", linetype = "dashed") +
  scale_color_manual(values=c("red","blue"))+
  scale_x_continuous(name="Month", breaks=a, labels=as.factor(a)) +
  theme(legend.position="top") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

str(out)
# 'data.frame':	149 obs. of  22 variables:
#  $ nperiod   : num  1 2 3 4 5 6 7 8 9 10 ...
#  $ month     : num  1 2 3 4 5 6 7 8 9 10 ...

# ...
#  $ all       : num  15 14 9 10 10 5 11 8 5 9 ...

#  $ epi       : num  0 0 0 0 1 0 1 0 0 1 ...
#  $ fit       : num  13.62 13.23 10.27 7.19 6.03 ...
#  $ upr       : num  15.26 14.79 11.76 8.7 7.56 ...
#  $ excess.raw: num  -0.255 -0.789 -2.764 1.298 2.444 ...
#  $ excess    : num  0 0 0 0 0 ...
#  $ epi.fn    : num  0 0 0 0 0 0 0 0 0 1 ...
 

#upperbound of expected, and observed
out %>% select(nperiod,all, upr,epi) %>% melt(id=c("nperiod", "epi")) %>% rename(Type=variable, Deaths=value) %>%
  ggplot(aes(x=nperiod, y=Deaths, group=Type)) +
  geom_line(aes(linetype=Type, color=Type))+
  geom_vline(xintercept=a, colour="grey", linetype = "dashed") +
  scale_linetype_manual(values=c("solid",  "longdash"))+
  scale_color_manual(values=c("red","black"))+
  scale_x_continuous(name="Month", breaks=a, labels=as.factor(a)) +
  theme(legend.position="top") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

##bar charts
#period-excess mortality for younger group (60+70)
results %>% select(season,eily, ebay, epry, esty ) %>% melt(id=c("season")) %>% 
  rename(Group=variable, Excess.Death.Rates=value) %>%
  ggplot(aes(x=season, y=Excess.Death.Rates, fill=Group)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_x_discrete(name="Season", limits= as.factor(c(2002:2013)))+
  theme_bw()

#period-excess mortality for older group (80+90)
results %>% select(season,eilo, ebao, epro, esto ) %>% melt(id=c("season")) %>% 
  rename(Group=variable, Excess.Death.Rates=value) %>%
  ggplot(aes(x=season, y=Excess.Death.Rates, fill=Group)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  scale_x_discrete(name="season", limits= as.factor(c(2002:2013)))+
  theme_bw()

dev.off()
