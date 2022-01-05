require(data.table) 
require(ggplot2)
require(cowplot)
require(gridExtra)

rm(list = ls())
load('data/month.data.Rdata')
rm(dft.month,dft.month2,dft.month.long)

## Quita totales ('all'), dan problemas 
dft.month2.long <-  dft.month2.long [(gage!='all') | (educa!='all') ]
##  convierte a factores
dft.month2.long$gage <- factor(dft.month2.long$gage)  
dft.month2.long$educa <- factor(dft.month2.long$educa, c( "il","ba", "pr", "st" ))
 
## reshape:
dft.month2.long[(gage!='all') | (educa!='all') ,
                 .(dealth.flu=sum(value*(measure=='influenza')),  
                   dealth.all =sum(value*(measure=='alldeaths')),  
                   persons.year=sum(value*(measure=='persons.year'))),
                  keyby=.(educa,gage,nperiod,year,month)] -> dtset

str(dtset)
# Classes ‘data.table’ and 'data.frame':	1192 obs. of  8 variables:
# $ educa       : Factor w/ 4 levels "il","ba","pr",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ gage        : Factor w/ 2 levels "o","y": 1 1 1 1 1 1 1 1 1 1 ...
# $ nperiod     : num  1 2 3 4 5 6 7 8 9 10 ...
# $ year        : int  2002 2002 2002 2002 2002 2002 2002 2002 2002 2002 ...
# $ month       : int  1 2 3 4 5 6 7 8 9 10 ...
# $ dealth.flu  : num  2 4 3 2 3 1 4 3 1 0 ...
# $ dealth.all  : num  661 603 577 492 478 428 464 415 358 401 ...
# $ persons.year: num  3825 3451 3819 3694 3820 ...

dtset
#       educa gage nperiod year month dealth.flu dealth.all persons.year
#    1:    il    o       1 2002     1          2        661     3824.613
#    2:    il    o       2 2002     2          4        603     3451.025
#    3:    il    o       3 2002     3          3        577     3819.114
#    4:    il    o       4 2002     4          2        492     3694.448
#    5:    il    o       5 2002     5          3        478     3820.287
# ---                                                                 
# 1188:    st    y     145 2014     1          0        538    38561.281
# 1189:    st    y     146 2014     2          1        484    35008.485
# 1190:    st    y     147 2014     3          0        497    38961.843
# 1191:    st    y     148 2014     4          0        468    37902.203
# 1192:    st    y     149 2014     5          0        429    39361.969

dtset[,':='(rate.flu= dealth.flu/persons.year,
            rate.all= dealth.all/persons.year)]

standard.pob  <- dtset[,.(standard.py=sum(persons.year)),keyby=.(gage)]

merge(dtset,standard.pob, by='gage') -> job
standard.rate <- job[,.(standard.rate.flu= sum(rate.flu*standard.py/sum(standard.py)),
                        standard.rate.all= sum(rate.all*standard.py/sum(standard.py))),
                        keyby=.(educa,nperiod,year,month)]

standard.rate[,month.brand:= min(year)+ (year-min(year)) +(month-1)/12 + 1/24 ]


standard.rate[standard.rate.flu==0, standard.rate.flu:=NA]

job[nperiod==10 & educa =='il']

pdf(file='graph/DesEducationMortalityGap.pdf', width = 8.3,  height =  11.7)

gg01 <-
ggplot(data=standard.rate, aes(x=month.brand, color=educa)) +
  geom_line(aes(y=10^5*standard.rate.flu)) +
  theme( axis.line = element_line(colour = "black"))+ 
  theme_bw() + theme(legend.position="left") +
  ylab('Deaths by 100.000 persons-year')+
  scale_x_continuous(name="Time", breaks = 2002:2015 )  +
  ggtitle('Inluenza Mortality: Standard Rate',
          subtitle = 'for over 60 by educational level and month')

gg02 <-
ggplot(data=standard.rate, aes(x=month.brand, color=educa)) +
  geom_line(aes(y=10^5*standard.rate.all)) +
  theme( axis.line = element_line(colour = "black"))+ 
  theme_bw() + theme(legend.position="left") +
  ylab('Deaths by 100.000 persons-year')+
  scale_x_continuous(name="Time", breaks = 2002:2015 )  +
  ggtitle('All causes Mortality: Standard Rate',
          subtitle = 'for over 60 by educational level and month')


grid.arrange(gg01,gg02, ncol = 1)

# plot_grid(gg01,gg02, labels = c("A", "B" ), ncol = 1)
  
dev.off()
