require(data.table)
require(ggplot2)
require(cowplot)
setwd("/Users/dinhngoclinh/Dropbox/For PhD research/Code/R/Flu&PI-Spain/Aflu2017-revise")

rm(list = ls())

load("data/month.data.Rdata")

rm(dft.month,dft.month.long,dft.month2)  # Other formats

dft.month2.long$educa<- factor(dft.month2.long$educa, c("il","ba", "pr", "st","all"))
dft.month2.long$gage<- factor(dft.month2.long$gage,  c("o","y","all" ) )

levels(dft.month2.long$educa)
levels(dft.month2.long$gage)

#-------------------------------------------------------------------------------
# Lineal Model by tempo=month and all ages:
#      Identifies epidemic periods of overmortality
#---------------------------------------------------------------------------------

## add data all ages
dft.month2.long[gage=='all' ,
                .(dealth.flu=sum(value*(measure=='influenza')),  
                  persons.year=sum(value*(measure=='persons.year'))),
                keyby=.(nperiod,year,month)] -> all.group


#Create auxiliary variable mark-monthly in the middle of the month to graph in decimal years
all.group[,month.brand:= min(year)+ (year-min(year)) +(month-1)/12 + 1/24 ]

## variables to store the seasonal effect (cycle)
omega1=(2*3.14159265)/12
omega2=(4*3.14159265)/12

all.group[,cos1:= cos(omega1*nperiod)]
all.group[,sin1:= sin(omega1*nperiod)]
all.group[,cos2:= cos(omega2*nperiod)]
all.group[,sin2:= sin(omega2*nperiod)]
all.group[,square:= (nperiod/100)^2]


## ------------------------------------------------------------------------------------------------------------
#Step 1: fit all data to a cyclic regression model and calculate upper bound of 95%CI, values above upr
#        vare considered as epidemic period and excluded from data set
##

## Model flu --
mod.flu1 =lm(dealth.flu ~  nperiod + square + cos1 + sin1 + cos2 +sin2,
             data = all.group)
summary(mod.flu1)

# Call:
#   lm(formula = dealth.flu ~ nperiod + square + cos1 + sin1 + cos2 + 
#        sin2, data = all.group)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  9.43668    1.18374   7.972 4.66e-13 ***
#   nperiod      0.10960    0.03645   3.007  0.00312 ** 
#   square      -7.04411    2.35402  -2.992  0.00327 ** 
#   cos1         3.56401    0.55114   6.467 1.51e-09 ***
#   sin1         3.55721    0.54931   6.476 1.44e-09 ***
#   cos2         0.58517    0.55100   1.062  0.29003    
#   sin2         3.13910    0.54762   5.732 5.72e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.738 on 142 degrees of freedom
# Multiple R-squared:  0.4709,	Adjusted R-squared:  0.4486 
# F-statistic: 21.06 on 6 and 142 DF,  p-value: < 2.2e-16

predicted <- as.data.table(predict(mod.flu1, interval="confidence", level=0.95))

all.group$expected.flu  <- predicted$fit
all.group$upr.flu       <-  predicted$upr


all.group[, winter.months:= ifelse(month %in% 4:9, F,T)]


## Identify epidemic periods:
all.group[,epi.flu:=ifelse(dealth.flu > upr.flu,TRUE,FALSE)]

brand <- as.vector( outer(2002:2014, c(-3/12,3/12), '+') )
brand <- sort(brand)

Epidemic.months <- all.group[,.(nperiod,year,month,month.brand, epi.flu)]
Epidemic.months[, winter.months:= ifelse(month %in% 4:9, F,T)] 

### check if it matches method 1
load('om1.Rdata')
dcast(Epidemic.months, year ~ month, value.var = 'epi.flu') -> print.epi2
print.epi -print.epi2   # 0 OK  


pdf(file='graph/Altenative_test1_lm_simple_OnlyFlu.pdf', width = 8.3,  height =  11.7)

geom_rect(data=Epidemic.months[epi.flu==T & winter.months,], 
          aes(xmin=year+(month-1)/12, xmax=year+(month)/12, 
              ymin=-Inf, ymax=+Inf),
          color=NA,
          fill="cyan",
          alpha=0.20,
          inherit.aes = FALSE) -> flu.bars


gp01 <- 
  qplot(month.brand,dealth.flu,data=all.group,  geom=c('line'), color=I('grey68') )  +
  geom_point(aes(y=dealth.flu,color=factor(epi.flu))) +
  geom_line(aes(y=upr.flu), color='orange' ) + 
  scale_color_manual(values=c('green','red'), name='Overmortality') +
  geom_vline(xintercept=brand, colour="grey", linetype = "dashed") +
  scale_x_continuous(name="Time: years and month", breaks = 2002:2015 )  +
  theme(legend.position="none") +
  theme( axis.line = element_line(colour = "black")) + 
  theme_bw() + theme(legend.position="bottom") + flu.bars +
  ylab('Observed Deaths by influenza & LRI\n and upper predictive threshold')+
  ggtitle('Epidemic threshold: Death for Influenza',
          subtitle = 'In cyan bars Epidemics periods\nidentified by monthy lineal regression (UP.IC 95%)')


gp01


  

rm(gp01)  # libera memoria

##---------------------------------------------------------------------------------------------
#Step 2: fit new data set (with NAs) to another cyclic regression model and calculate 95% upr.
# Values above this limit are considered as excess mortality
# + We introduce the educational level
# + We calculate two age and sex groupings 60 to 70 and 80y +. Shrinking by age
# possibly avoid confusion bias associated with different structures
# age of the different educational groups (which would prevent their comparison)
# + 1º We eliminate the data of the identified epidemic months to make a new prediction,
# replacing them with NA
# + Then we independently model each age group and educational level to define threshold
# of overmortality (upr).
## ---------------------------------------------------------------------------------------------

## Group data and 
## Remove totals: "all"
dft.month2.long[gage != 'all' |  educa != 'all',
                .(dealth.flu=sum(value*(measure=='influenza')),  
                  persons.year=sum(value*(measure=='persons.year'))),
                keyby=.(educa,gage,nperiod,year,month)] -> strat.group


# Remove "all" of levels list in "educa" y "gage"
strat.group$gage <- factor(strat.group$gage)  
strat.group$educa <- factor(strat.group$educa, c( "il","ba", "pr", "st" ))

str(strat.group)

## ---
strat.group[,cos1:= cos(omega1*nperiod)]
strat.group[,sin1:= sin(omega1*nperiod)]
strat.group[,cos2:= cos(omega2*nperiod)]
strat.group[,sin2:= sin(omega2*nperiod)]
strat.group[,square:= (nperiod/100)^2]

strat.group <- merge (Epidemic.months, strat.group, 
                      by= intersect(names(strat.group), names( Epidemic.months) )) 


strat.group$dealth.flu0        <- strat.group$dealth.flu

## Put to NA  over-mortality episodes
strat.group[epi.flu==TRUE, ':='(dealth.flu=NA)]  

## Model to make predictions including educational level and age group
## I see two alternatives:
## 1.- A single model introducing age group and educational level as predictors
## 2.- 12 models = 4 levels x 2 age groups (Solution so far tested)
##

##-----------------------------------------------
## Stratified resolution 8 models: 4 x 2
##-----------------------------------------------

### Expected by Influenza :::::::::::::::::::::::::::::::::::::::::::::::::
job  <- strat.group[,.(gage=as.character('-'),educa=as.character('-'),
                       expected=as.numeric(0), upr=as.numeric(0) ),
                    keyby=.(nperiod,square,cos1,sin1,cos2,sin2)]


store <- job[FALSE, c('nperiod', 'gage', 'educa', 'expected', 'upr'), with =F ] 
str(store)
i <- levels(strat.group$gage)[1] ;
j <- levels(strat.group$educa)[1] ;
paste(i,j)
strat.group[epi.flu==T, c(1:3,5:9,16), with=F]

for (i in levels(strat.group$gage)) {
  for (j in  levels(strat.group$educa) ){
    # only model with non-epidemic months 
    model <- lm(dealth.flu ~  nperiod + square + cos1 + sin1 + cos2 +sin2, 
                data = strat.group[gage==i & educa == j])
    predicted <- as.data.table(predict(model, newdata = job ,
                                       interval="confidence", level=0.95))
    job$expected <- predicted$fit
    job$upr      <- predicted$upr
    job$gage  <- i
    job$educa <- j
    store <- rbind(store, job [,c(1,7:10), with = F])
    # print(paste(i,j,' OK'))
  }
}

str(store)

setnames(store,c("expected","upr"),c("expected.flu","upr.flu") )
store[, educa:=factor(educa, levels = levels(strat.group$educa))]
store[, gage:=factor(gage)]
merge(strat.group, store, by = c("nperiod","gage","educa") )   -> strat.group


##--------------------------------------------------------------------------------------------------------------
#Step 3: Where excess mortality > 0, and time period falls into the intervals between Oct of a year to March
#        of the following year is our interest. Total excess mortality over an interval are summed together.
#
## Model to make predictions including educational level and age group
## I see two alternatives:
## 1.- A single model introducing age group and educational level as predictors
## 2.- 12 models = 4 levels x 2 age groups (Solution so far tested)
##
#
##--------------------------------------------------------------------------------------------------------------
strat.group[,excess.raw.flu:= dealth.flu0-upr.flu]

strat.group[,excess.flu:= ifelse(epi.flu       & 
                                   winter.months &  excess.raw.flu > 0
                                   , excess.raw.flu,0)]

strat.group[,py.base:= ifelse(excess.flu > 0, persons.year, 0)]  ## busco discrepancias  ..


## Gráfica Esperados y observados
melt(strat.group, 
     id.vars=1:6, 
     measure.vars= c("dealth.flu0",  
                     "expected.flu" ),
     variable.factor = F )  -> pp
pp[, variable:= gsub('0','',variable)]
pp[, c('type','event') :=  tstrsplit(pp$variable,'\\.')]

qplot(x=month.brand,y=value, color=type, data=pp[event=='flu'], geom = 'line') + 
  facet_grid(educa~gage, scales="free" ) + theme_bw() +
  labs(title='Influenza: Observed and predicted deaths',
       subtitle = 'Predicted deaths by lineal regresion model for each age and educational level')+
  geom_rect(data=Epidemic.months[epi.flu==T & winter.months,], 
            aes(xmin=year+(month-1)/12, xmax=year+(month)/12, 
                ymin=-Inf, ymax=+Inf),
            color=NA,
            fill="cyan",
            alpha=0.15,
            inherit.aes = FALSE)


## Add total Expected and Observed deaths for annual estimate of
## excess mortality associated with epidemic winter flu episodes
strat.group[winter.months==T & epi.flu & excess.flu > 0 ,
            .(O.flu=sum(dealth.flu0),
              E.flu=sum(expected.flu),
              excess.flu=sum(excess.flu),
              persons.year  = sum(persons.year) ,
              persons.year.exc.flu = sum(persons.year*(excess.flu>0))
            ),.(year, gage, educa)] -> anual.resu

# strat.group[,
#             .(excess.flu=sum(excess.flu),
#               persons.year  = sum(persons.year) ,
#               persons.year.exc.flu = sum(py.base)
#             ),.(year, gage, educa)] -> anual.resu


 # strat.group[(excess.flu > 0) & epi.flu & winter.months ,
 #             .(e1=sum(dealth.flu0) - sum(upr.flu),e2=sum(sum(excess.flu))),
 #             keyby=.(nperiod)]



anual.resu[, ':='(SMR.flu = O.flu/E.flu ,
                  AR.flu = 10^5*excess.flu / persons.year.exc.flu
                  )]

# anual.resu[, ':='(AR.flu = 10^5*excess.flu / persons.year.exc.flu )]


# anual.resu[, ':='(AR.flu =  ifelse(AR.flu==0,NA,AR.flu), 
#                   AR.all =  ifelse(AR.all==0,NA,AR.all))]



## Visualize results
ggplot(anual.resu, aes(x=year, y=AR.flu, fill=educa)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(~gage, nrow=2, scales = 'free') +
  theme(legend.position=c(0.35,.9),  legend.title = element_blank() )  +
  scale_x_continuous(name="years", breaks = 2002:2014 )  +
  labs(title='Influenza: epidemic overmortality', 
       y ='Excess deaths of more per 100,000 people and years', x = 'Years') +
  theme(axis.text.x = element_text(angle = 90))



anual.resu[,
           .(O.flu=sum(O.flu),
             E.flu=sum(E.flu),
             excess.flu=sum(excess.flu),
             persons.year  = sum(persons.year),
             persons.year.exc.flu = sum(persons.year.exc.flu)
           ),.( educa, gage)] ->  total 

total[, ':='(SMR.flu = O.flu/E.flu ,
             SMR.all = O.flu/E.flu,
             AR.flu = 10^5*excess.flu / persons.year.exc.flu
)]


gp01 <-
  ggplot(total, aes(x=gage,  y=AR.flu, fill=educa)) + 
  geom_bar(stat="identity", position=position_dodge())  +
  theme(legend.position=c(0.75,.9),  legend.title = element_blank() )  +
  labs(title='Influenza: overmortality \nin epidemic wave', 
       y ='Excess deaths of more per 100,000 people and years', 
       x = 'Age groups')

z <-  qnorm(0.975)
gp03 <-
  ggplot(total, aes(x=gage, y=SMR.flu, 
                    ymin=SMR.flu*exp(-z/sqrt(O.flu)),
                    ymax=SMR.flu*exp(+z/sqrt(O.flu)),
                    color=educa, fill=educa)) +  ylab('Ratio observed over expected') + 
  geom_pointrange(position = position_jitterdodge(0,0,.4) )  + 
  scale_y_continuous() + 
  theme(legend.position=c(0.2,.9),  legend.title = element_blank() )  +
  labs(title='Influenza Mortality Ratios \nin  epidemic wave')


plot_grid(gp01,gp03, labels = c("A", "B" ), ncol = 1,  align = "h")

dev.off()


### Tablas ---
anual.resu$gage <-  relevel(anual.resu$gage,'y')
(dcast(anual.resu, year ~  educa + gage , value.var = 'excess.flu', fun =sum) -> om.abs2 )
##    Absolute Overmortality 
#    year    il_y      il_o      ba_y      ba_o       pr_y       pr_o      st_y       st_o
# 1  2002 0.000000 0.0000000 2.1690873  0.000000 0.35951952 0.5557855 0.0000000 0.1184616
# 2  2003 1.396723 0.8888326 1.1250685  6.229950 0.06013920 1.3769462 0.9536151 0.0000000
# 3  2004 2.705790 3.5691921 2.1658951  5.377052 1.12680996 0.0000000 1.3742185 1.4791311
# 4  2005 1.145037 7.3905809 1.0518551 18.820877 1.06928112 2.4431825 2.9571992 2.4888252
# 5  2007 1.058783 1.2638495 2.6963474 15.083452 3.59434348 3.3592950 0.0000000 0.2612206
# 6  2008 3.665133 0.0000000 0.9559313 11.895179 0.33515608 1.1875156 1.3642449 0.0000000
# 7  2009 2.785788 3.6777370 0.8903395 14.661214 0.36956722 1.1088762 2.1876803 2.0891471
# 8  2010 0.000000 3.1065134 0.3079651  0.000000 0.01798471 2.8170665 0.0000000 1.2717498
# 9  2011 1.580412 0.0000000 0.0000000  0.000000 0.42673333 0.3708073 0.3766131 2.6059104
# 10 2012 1.455705 7.9250615 1.4683772 12.366574 0.00000000 7.9617870 0.9028907 7.4406516
# 11 2013 1.274199 0.0000000 0.0000000  1.987871 0.00000000 2.0227789 0.0000000 0.0000000
# 12 2014 0.000000 0.0000000 0.2203037  2.290481 0.00000000 2.0330111 0.5155118 0.5357874

anual.resu$excess.flu.rate  <-  100000 * anual.resu$excess.flu / anual.resu$persons.year.exc.flu
(dcast(anual.resu, year ~  educa + gage , value.var = 'excess.flu.rate', fun =sum) -> om.rate2)
##    Overmortality rate:
#    year      il_y       il_o      ba_y      ba_o      pr_y       pr_o     st_y      st_o
# 1  2002  0.000000   0.00000 5.2731829   0.00000 1.46972170 15.100959 0.000000   5.66413
# 2  2003 12.380402  11.73654 2.7686279  34.86015 0.23871413 34.309455 2.334087   0.00000
# 3  2004 12.620883  47.32912 2.6915593  19.31995 2.21383761  0.000000 2.140949  20.59028
# 4  2005  5.679628 103.66897 0.9159138  67.74491 2.15475583 19.067341 6.553328 100.72880
# 5  2007 12.378529  11.73084 3.6714940  76.29420 6.68117775 75.198934 0.000000  10.20548
# 6  2008 20.851756   0.00000 2.6198658 110.51188 1.19434217 22.827242 5.117871   0.00000
# 7  2009 11.749344  33.71697 2.7329677  33.06251 1.29193673 20.203888 2.618379  21.87785
# 8  2010  0.000000  81.29217 0.8857441   0.00000 0.06266793 47.715616 0.000000  35.99345
# 9  2011 24.123141   0.00000 0.0000000   0.00000 1.50879081  5.911273 1.153496  67.18714
# 10 2012 11.326294 104.59146 4.5805566  24.65046 0.00000000 41.385437 1.351930  61.82557
# 11 2013 20.910976   0.00000 0.0000000  15.06028 0.00000000 29.417480 0.000000   0.00000
# 12 2014  0.000000   0.00000 0.8271252  18.51184 0.00000000 30.897619 1.472534  12.71993

##  Compare with results method 1:
load('om1.Rdata')

om.abs1[,-c(9,10)] - om.abs2[-5,-1]
#          eily       eilo       ebay        ebao        epry         epro        esty        esto
# 1   0.00000000  0.0000000  0.00000000   0.0000000  0.00000000  0.0000000  0.000000000  0.0000000
# 2   0.00000000  1.0622763  0.09934626   2.3619122  0.77284316  0.0000000  0.461580029  0.5310055
# 3   1.14503735  6.3283046  0.94528318  15.4351273 -0.03817976  2.3169882  0.973686995  1.9578197
# 4  -1.14503735 -7.3905809 -1.04462944 -17.7970394 -0.73466340 -2.3169882 -1.435267023 -2.4888252
# 5   0.00000000 -0.9382929 -2.69634735  -5.7865136 -3.59434348  0.0000000  0.000000000  0.0000000
# 6  -0.51801041  0.9382929  1.74041602   5.7865136  3.25918740  0.0000000  0.000000000  0.0000000
# 7   0.00384755 -1.2442039  0.95593133  -4.6070157 -0.03441114  0.0000000 -0.345775992 -0.8018465
# 8   0.51416286  1.2442039  0.00000000   4.6070157  0.36956722  0.0000000  0.345775992  0.8018465
# 9  -1.58041169  0.0000000  0.00000000   0.0000000 -0.42673333 -0.3708073 -0.376613097 -2.6059104
# 10  1.58041169  0.0000000  0.00000000  -0.9038547  0.42673333  0.3708073 -0.007913982  0.1265517
# 11  0.00000000  0.0000000  0.00000000   0.9038547  0.00000000  0.0000000  0.384527079  2.4793586
# 12  0.00000000  0.0000000  0.00000000   0.0000000  0.00000000  0.0000000  0.000000000  0.0000000


