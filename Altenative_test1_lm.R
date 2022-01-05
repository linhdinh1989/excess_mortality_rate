require(data.table)
require(ggplot2)
require(cowplot)

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


## agrega datos todas las edad

dft.month2.long[gage=='all' ,
                  .(dealth.flu=sum(value*(measure=='influenza')),  
                    dealth.all =sum(value*(measure=='alldeaths')),  
                    persons.year=sum(value*(measure=='persons.year'))),
       keyby=.(nperiod,year,month)] -> all.group


# Crea varible auxiliar marca-mensual a mitad de mes para graficar en años decimales
all.group[,month.brand:= min(year)+ (year-min(year)) +(month-1)/12 + 1/24 ]

## variables para almacenar el efecto estacional (ciclo)
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

## Modela all death --
mod.all1 =lm(dealth.all ~  nperiod + square + cos1 + sin1 + cos2 +sin2,
             data = all.group)
# summary(mod.all1)
predicted <- as.data.table(predict(mod.all1, interval="confidence", level=0.95))

all.group$expected.all  <- predicted$fit
all.group$upr.all       <-  predicted$upr

all.group[, winter.months:= ifelse(month %in% 4:9, F,T)]



## Identifica periodo epidemicos:
all.group[,epi.flu:=ifelse(dealth.flu > upr.flu,TRUE,FALSE)]
all.group[,epi.all:=ifelse(dealth.all > upr.all,TRUE,FALSE)]

## Agreement ??
with(all.group, table(epi.flu,epi.all ,winter.months))

brand <- as.vector( outer(2002:2014, c(-3/12,3/12), '+') )
brand <- sort(brand)

Epidemic.months <- all.group[,.(nperiod,year,month,month.brand, epi.flu)]
Epidemic.months[, winter.months:= ifelse(month %in% 4:9, F,T)] 


pdf(file='graph/Altenative_test1_lm.pdf', width = 8.3,  height =  11.7)


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
  theme( axis.line = element_line(colour = "black"))+ 
  theme_bw() + theme(legend.position="bottom") + flu.bars +
  ylab('Observed Deaths by influenza & LRI\n and upper predictive threshold')+
  ggtitle('Epidemic threshold: Death for Influenza',
          subtitle = 'Epidemic threshold estimated by lineal regression (UP.IC 95%):\nTotal monthly deaths without other predictors')

gp02 <- 
  qplot(month.brand,dealth.all,data=all.group,  geom=c('line'), color=I('grey68') )  +
  geom_point(aes(y=dealth.all,color=factor(epi.all))) +
  geom_line(aes(y=upr.all), color='orange' ) + 
  scale_color_manual(values=c('green','red'), name='Overmortality') +
  geom_vline(xintercept=brand, colour="grey", linetype = "dashed") +
  scale_x_continuous(name="Time: years and month", breaks = 2002:2015 )  +
  theme(legend.position="none") +
  theme( axis.line = element_line(colour = "black"))+  
  theme_bw() + theme(legend.position="bottom") +  flu.bars + 
  ylab('Observed Deaths by all cause\n and upper predictive threshold')+
  ggtitle('Epidemic threshold: all Death',
          subtitle = 'Over-mortality threshold estimated by lineal regression (UP.IC 95%):\nTotal monthly deaths without other predictors')


plot_grid(gp01,gp02, labels = c("A", "B" ), ncol = 1)

rm(gp01,gp02)  # libera memoria

##---------------------------------------------------------------------------------------------
#Step 2: fit new data set (with NAs) to another cyclic regression model and calculate 95% upr.
# Values above this limit are considered as excess mortality
# + We introduce the educational level
# + We calculate two age and sex groupings 60 to 70 and 80y +. Shrinking by age
# possibly avoid confusion bias associated with different structures
# age of the different educational groups (which would prevent their comparison)
# + 1st We eliminate the data of the identified epidemic months to make a new prediction,
# replacing them with NA
# + Then we independently model each age group and educational level to define threshold
# of overmortality (upr).
##---------------------------------------------------------------------------------------------
# Epidemic.months <- all.group[,.(nperiod,year,month,month.brand,epi.flu,epi.all)]

## aggregate data
## filter out total: "all"
dft.month2.long[gage != 'all' |  educa != 'all',
                .(dealth.flu=sum(value*(measure=='influenza')),  
                  dealth.all =sum(value*(measure=='alldeaths')),  
                  persons.year=sum(value*(measure=='persons.year'))),
                keyby=.(educa,gage,nperiod,year,month)] -> strat.group


# Remove "all" from list of levels in "educa" and "gage"
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
strat.group$dealth.all0        <- strat.group$dealth.all

## Put the episodes of overmortality to NA: 
strat.group[epi.flu==TRUE, ':='(dealth.flu=NA, dealth.all=NA)]  

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
job

store <- job[FALSE, c(1,7:10), with =F ] ; str(store)
i <- levels(strat.group$gage)[1] ;
j <- levels(strat.group$educa)[1] ;
paste(i,j)
for (i in levels(strat.group$gage)) {
  for (j in  levels(strat.group$educa) ){
    # only model with non-epidemic months
    model <- lm(dealth.flu ~  nperiod + square + cos1 + sin1 + cos2 +sin2, 
                data = strat.group[gage==i & educa == j])
    predicted <- as.data.table(predict(model, newdata = job ,
                                       interval="confidence", level=0.975))
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

### Expected all causes :::::::::::::::::::::::::::::::::::::::::::::::::
store <- job[FALSE, c(1,7:10), with = F ]
for (i in levels(strat.group$gage)) {
  for (j in  levels(strat.group$educa) ){
    # only model with non-epidemic months
    model <- lm(dealth.all ~  nperiod + square + cos1 + sin1 + cos2 +sin2, 
                data = strat.group[gage==i & educa == j])
    predicted <- as.data.table(predict(model, newdata = job ,
                                       interval="confidence", level=0.975))
    job$expected <- predicted$fit
    job$upr      <- predicted$upr
    job$gage  <- i
    job$educa <- j
    store <- rbind(store, job [,c(1,7:10),  with = F])
  }
}

setnames(store,c("expected","upr"),c("expected.all","upr.all") )
store[, educa:=factor(educa, levels = levels(strat.group$educa))]
store[, gage:=factor(gage)]
merge(strat.group, store, by = c("nperiod","gage","educa") )   -> strat.group
strat.group[, winter.months:= ifelse(month %in% 4:9, F,T)]

str(strat.group)
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
strat.group[,excess.flu:= ifelse(epi.flu       & 
                                   winter.months & 
                                   dealth.flu0 > upr.flu, dealth.flu0-upr.flu,0)]

strat.group[,excess.all:= ifelse(epi.flu       & 
                                   winter.months &
                                   dealth.all0 > upr.all, dealth.all0-upr.all,0)]

## Graph predictions and observed
melt(strat.group, 
     id.vars=1:6, 
     measure.vars= c("dealth.flu0", "dealth.all0", 
                     "expected.flu", "expected.all" ),
     variable.factor = F )  -> pp
pp[, variable:= gsub('0','',variable)]

pp[, c('type','event') :=  tstrsplit(pp$variable,'\\.')]



qplot(x=month.brand,y=value, color=type, data=pp[event=='all'], geom = 'line') + 
  facet_grid(educa~gage, scales="free" ) + theme_bw() +  flu.bars +
  labs(title='All death: Observed and predicted deaths',
       subtitle = 'Predicted deaths by lineal regresion model for each age and educational level')

qplot(x=month.brand,y=value, color=type, data=pp[event=='flu'], geom = 'line') + 
  facet_grid(educa~gage, scales="free" ) + theme_bw() + flu.bars +
  labs(title='Influenza: Observed and predicted deaths',
       subtitle = 'Predicted deaths by lineal regresion model for each age and educational level')




## Add total Expected and Observed deaths for annual estimate of
## excess mortality associated with epidemic winter flu episodes
cat(names(strat.group)[])

strat.group[winter.months==T,
            .(O.flu=sum(dealth.flu0),
              O.all=sum(dealth.all0),
              E.flu=sum(expected.flu),
              E.all=sum(expected.all),
              excess.flu=sum(excess.flu),
              excess.all=sum(excess.all),
              persons.year  = sum(persons.year),
              persons.year.exc.flu = sum(persons.year*(sum(excess.flu)>0)),
              persons.year.exc.all = sum(persons.year*(sum(excess.all)>0))
            ),.(year, gage, educa)] -> anual.resu

anual.resu[, ':='(SMR.flu = O.flu/E.flu ,
                  SMR.all = O.flu/E.flu,
                  AR.flu = 10^5*excess.flu / persons.year.exc.flu,
                  AR.all = 10^5*excess.all / persons.year.exc.all
)]

# anual.resu[, ':='(AR.flu =  ifelse(AR.flu==0,NA,AR.flu), 
#                   AR.all =  ifelse(AR.all==0,NA,AR.all))]


anual.resu

## Graph results ...
gp01 <-
  ggplot(anual.resu, aes(x=year, y=AR.flu, fill=educa)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(~gage, nrow=2) +
  theme(legend.position=c(0.35,.9),  legend.title = element_blank() )  +
  scale_x_continuous(name="years", breaks = 2002:2014 )  +
  labs(title='Influenza: epidemic overmortality', 
       y ='Excess deaths of more per 100,000 people and years', x = 'Years') +
  theme(axis.text.x = element_text(angle = 90))

gp02 <-
  ggplot(anual.resu, aes(x=year, y=AR.all, fill=educa)) + 
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(~gage, nrow=2) +
  theme(legend.position=c(0.35,.9), legend.title = element_blank()) +
  scale_x_continuous(name="years", breaks = 2004:2014 )  +
  labs(title='All death: epidemic overmortality', 
       y ='Excess deaths of more per 100,000 people and years', x = 'Years') +
  theme(axis.text.x = element_text(angle = 90))

plot_grid(gp01,gp02, labels = c("A", "B" ), ncol = 2,  align = "h")

rm(gp01,gp02) 



anual.resu[,
           .(O.flu=sum(O.flu),
             O.all=sum(O.all),
             E.flu=sum(E.flu),
             E.all=sum(E.all),
             excess.flu=sum(excess.flu),
             excess.all=sum(excess.all),
             persons.year  = sum(persons.year),
             persons.year.exc.flu = sum(persons.year.exc.flu),
             persons.year.exc.all = sum(persons.year.exc.all)
           ),.( educa, gage)] ->  total 

total[, ':='(SMR.flu = O.flu/E.flu ,
             SMR.all = O.flu/E.flu,
             AR.flu = 10^5*excess.flu / persons.year.exc.flu,
             AR.all = 10^5*excess.all / persons.year.exc.all
)]


gp01 <-
  ggplot(total, aes(x=gage,  y=AR.flu, fill=educa)) + 
  geom_bar(stat="identity", position=position_dodge())  +
  theme(legend.position=c(0.35,.9),  legend.title = element_blank() )  +
  labs(title='Influenza: overmortality \nin epidemic wave', 
       y ='Excess deaths of more per 100,000 people and years', 
       x = 'Age groups')
gp02 <-
  ggplot(total, aes(x=gage,  y=AR.all, fill=educa)) + 
  geom_bar(stat="identity", position=position_dodge())  +
  theme(legend.position=c(0.35,.9),  legend.title = element_blank() )  +
  labs(title='All death: overmortality \nin epidemic wave', 
       y ='Excess deaths of more per 100,000 people and years', 
       x = 'Age groups')


z <-  qnorm(0.975)
gp03 <-
  ggplot(total, aes(x=gage, y=SMR.flu, 
                    ymin=SMR.flu*exp(-z/sqrt(O.flu)),
                    ymax=SMR.flu*exp(+z/sqrt(O.flu)),
                    color=educa, fill=educa)) +  ylab('Ratio observed over expected') + 
  geom_pointrange(position = 'jitter')  + scale_y_continuous() + 
  theme(legend.position=c(0.7,.9),  legend.title = element_blank() )  +
  labs(title='Influenza Mortality Ratios \nin  epidemic wave')


gp04 <-
  ggplot(total, aes(x=gage, y=SMR.all, 
                    ymin=SMR.all*exp(-z/sqrt(O.all)),
                    ymax=SMR.all*exp(+z/sqrt(O.all)),
                    color=educa, fill=educa)) + ylab('Ratio observed over expected') +
  geom_pointrange(position = 'jitter')  + scale_y_continuous() + 
  theme(legend.position=c(0.7,.9),  legend.title = element_blank() )  +
  labs(title='All causes Mortality Ratios \nin the epidemic wave')





plot_grid(gp01,gp02,gp03,gp04, labels = c("A", "B" ), ncol = 2,  align = "h")


dev.off()
