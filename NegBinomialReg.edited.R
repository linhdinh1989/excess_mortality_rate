require(data.table)
require(ggplot2)
require(cowplot)
library(MASS)

rm(list = ls())

load("month.data.edited.Rdata")

rm(dft.month,dft.month.long,dft.month2)  # Other formats

dft.month2.long$educa<- factor(dft.month2.long$educa, c("il","ba", "pr", "st","all"))
dft.month2.long$gage<- factor(dft.month2.long$gage,  c("o","y","all" ) )

levels(dft.month2.long$educa)
levels(dft.month2.long$gage)

## Quita totales ('all'), dan problemas 
dft.month2.long <-  dft.month2.long [(gage!='all') | (educa!='all') ]
##  convierte a factores
dft.month2.long$gage <- factor(dft.month2.long$gage)  
dft.month2.long$educa <- factor(dft.month2.long$educa, c( "il","ba", "pr", "st" ))

#-------------------------------------------------------------------------------
# GLM Model by tempo=month and pesons.year as predictor
#      Identifies epidemic periods of overmortality
#---------------------------------------------------------------------------------

## Selecciona sin totales si aun hay
dft.month2.long[(gage!='all') | (educa!='all') ,
                .(dealth.flu=sum(value*(measure=='influenza')),  
                  dealth.all =sum(value*(measure=='alldeaths')),  
                  persons.year=sum(value*(measure=='persons.year'))),
                keyby=.(educa,gage,nperiod,year,month)] -> dtset



# Create auxiliary variable mark-monthly in the middle of the month to graph in decimal years
dtset[,month.brand:= min(year)+ (year-min(year)) +(month-1)/12 + 1/24 ]

## variables to store the seasonal effect (cycle)
omega1=(2*3.14159265)/12
omega2=(4*3.14159265)/12

dtset[,cos1:= cos(omega1*nperiod)]
dtset[,sin1:= sin(omega1*nperiod)]
dtset[,cos2:= cos(omega2*nperiod)]
dtset[,sin2:= sin(omega2*nperiod)]
dtset[,square:= (nperiod/100)^2]


## ------------------------------------------------------------------------------------------------------------
#Step 1: fit all data to a cyclic regression model and calculate upper bound of 95%CI, values above upr
#        vare considered as epidemic period and excluded from data set
##
##        Using as predictors the variables derived from the time 
##        (stationary effect, tedency and cycle), age, and educational 
##        level and population at risk in each of the population categories
## ------------------------------------------------------------------------------------------------------------

## Model flu --
mod.flu1 =glm.nb(dealth.flu ~ educa + gage  + nperiod + square + cos1 + sin1 + cos2 +sin2 + offset(log(persons.year)),
                 data = dtset)

summary(mod.flu1)
predicted <- predict(mod.flu1, type = 'link', se.fit=T)
dtset$expected.flu <- exp(predicted$fit)
dtset$upr.flu <-  exp(predicted$fit+predicted$se.fit*qnorm(0.975))



## Modela all death --
mod.all1 =glm.nb(dealth.flu ~ educa + gage  + nperiod + square + cos1 + sin1 + cos2 +sin2 + offset(log(persons.year)),
                 data = dtset)

summary(mod.all1)
predicted <- predict(mod.all1, type = 'link', se.fit=T)
dtset$expected.all <- exp(predicted$fit)
dtset$upr.all <-  exp(predicted$fit+predicted$se.fit*qnorm(0.975))



### Prediciones realizadas por grupo de edad y nivel educativo
### hay que agregar para todos los grupo de edad y niveles educativos
### para definir umbrales epidemicos
cat(names(dtset))

dtset[,.(
  dealth.flu=sum(dealth.flu),
  dealth.all=sum(dealth.all),
  persons.year=sum(persons.year),
  expected.flu=sum(expected.flu),
  upr.flu=sum(upr.flu),
  expected.all=sum(expected.all),
  upr.all=sum(upr.all)
),
keyby=.(nperiod,year,month,month.brand)] -> dtset.all




##----------
dtset[, winter.months:= ifelse(month %in% 4:9, F,T)]
dtset.all[, winter.months:= ifelse(month %in% 4:9, F,T)]


## Identifica periodo epidemicos:
dtset.all[,epi.flu:=ifelse(dealth.flu > upr.flu,TRUE,FALSE)]
dtset.all[,epi.all:=ifelse(dealth.all > upr.all,TRUE,FALSE)]

## Agreement > in winter,month (ok)
with(dtset.all, table(epi.flu,epi.all ,winter.months))

brand <- as.vector( outer(2002:2014, c(-3/12,3/12), '+') )
brand <- sort(brand)

Epidemic.months <- dtset.all[,.(nperiod,year,month,month.brand, epi.flu)]
Epidemic.months[, winter.months:= ifelse(month %in% 4:9, F,T)] 


pdf(file='NegBinomial.edited.pdf', width = 8.3,  height =  11.7)


geom_rect(data=Epidemic.months[epi.flu==T & winter.months,], 
          aes(xmin=year+(month-1)/12, xmax=year+(month)/12, 
              ymin=-Inf, ymax=+Inf),
          color=NA,
          fill="cyan",
          alpha=0.20,
          inherit.aes = FALSE) -> flu.bars


gp01 <- 
  qplot(month.brand,dealth.flu,data=dtset.all,  geom=c('line'), color=I('grey68') )  +
  geom_point(aes(y=dealth.flu,color=factor(epi.flu))) +
  geom_line(aes(y=upr.flu), color='orange' ) + 
  scale_color_manual(values=c('green','red'), name='Overmortality') +
  geom_vline(xintercept=brand, colour="grey", linetype = "dashed") +
  scale_x_continuous(name="Time: years and month", breaks = 2002:2015 )  +
  theme( axis.line = element_line(colour = "black"))+ 
  theme_bw() + theme(legend.position="bottom") + flu.bars +
  ylab('Observed Deaths by influenza & LRI\n and upper predictive threshold')+
  ggtitle('Epidemic threshold: Death for Influenza',
          subtitle = 'Epidemic threshold estimated by poisson regression (UP.IC 95%):\nmonthly deaths predicted by age group, educational level and exposed population')

gp02 <- 
  qplot(month.brand,dealth.all,data=dtset.all,  geom=c('line'), color=I('grey68') )  +
  geom_point(aes(y=dealth.all,color=factor(epi.all))) +
  geom_line(aes(y=upr.all), color='orange' ) + 
  scale_color_manual(values=c('green','red'), name='Overmortality') +
  geom_vline(xintercept=brand, colour="grey", linetype = "dashed") +
  scale_x_continuous(name="Time: years and month", breaks = 2002:2015 )  +
  theme( axis.line = element_line(colour = "black"))+  
  theme_bw() + theme(legend.position="bottom") +  flu.bars + 
  ylab('Observed Deaths by all cause\n and upper predictive threshold')+
  ggtitle('Epidemic threshold: all Death',
          subtitle = 'Over-mortality threshold estimated by poisson regression (UP.IC 95%):\nmonthly deaths predicted by age group, educational level and exposed population')


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
# Epidemic.months <- dtset.all[,.(nperiod,year,month,month.brand,epi.flu,epi.all)]

dtset2 <- dtset  ## copia para manipular: introducir NA

dtset2 <- merge (Epidemic.months, dtset2, 
                 by= intersect(names(dtset2), names( Epidemic.months) )) 

## 1º hago copia de los valores de los fallecidos: !! cuidado con usar := (por referencia)
dtset2$dealth.flu0        <- dtset2$dealth.flu
dtset2$dealth.all0        <- dtset2$dealth.all

## Model to make predictions including educational level and age group
## I see two alternatives:
## 1.- A single model introducing age group and educational level as predictors
## 2.- 12 models = 4 levels x 2 age groups (Solution so far tested)
##

##-----------------------------------------------
## Stratified resolution 8 models: 4 x 2
##-----------------------------------------------

### Expected by Influenza :::::::::::::::::::::::::::::::::::::::::::::::::
job  <- dtset2[,.(gage=as.character('-'),educa=as.character('-'),
                  expected=as.numeric(0), upr=as.numeric(0) ),
               keyby=.(nperiod,square,cos1,sin1,cos2,sin2)]
store <- job[FALSE, c(1,7:10), with =F ]  
i <- levels(dtset2$gage)[1] ;
j <- levels(dtset2$educa)[1] ;
paste(i,j)
for (i in levels(dtset2$gage)) {
  for (j in  levels(dtset2$educa) ){
    # Model stratified by age group and cause
    # only with data from non-epidemic months
    ## Model flu -
    
    
    model =glm.nb(dealth.flu ~ nperiod + square + cos1 + sin1 + cos2 +sin2+offset(log(persons.year)),
               data = dtset2[gage==i & educa == j])
    
    predicted <- predict(model, newdata = dtset2[gage==i & educa == j],
                         type = 'link', se.fit=T)
    
    job$expected <- exp(predicted$fit)
    job$upr      <- exp(predicted$fit+predicted$se.fit*qnorm(0.975))
    job$gage  <- i
    job$educa <- j
    store <- rbind(store, job [,c(1,7:10), with = F])
    print(paste(i,j,' OK'))
  }
}

str(store)

setnames(store,c("expected","upr"),c("expected2.flu","upr2.flu") )
store[, educa:=factor(educa, levels = levels(dtset2$educa))]
store[, gage:=factor(gage)]
merge(dtset2, store, by = c("nperiod","gage","educa") )   -> dtset2

str(dtset2)
### Expected all causes :::::::::::::::::::::::::::::::::::::::::::::::::
store <- job[FALSE, c(1,7:10), with = F ]
str(store)

for (i in levels(dtset2$gage)) {
  for (j in  levels(dtset2$educa) ){
    print(paste(i,j))
    # solo modela con meses no epidemicos 
    model =glm(dealth.all ~ nperiod + square + cos1 + sin1 + cos2 +sin2+ log(persons.year),
               data = dtset2[gage==i & educa == j])
    
    predicted <- predict(model, newdata = dtset2[gage==i & educa == j],
                         type = 'link', se.fit=T)
    
    
    job$expected <- exp(predicted$fit)
    job$upr      <- exp(predicted$fit+predicted$se.fit*qnorm(0.975))
    job$gage  <- i
    job$educa <- j
    store <- rbind(store, job [,c(1,7:10), with = F])
  }
}

setnames(store,c("expected","upr"),c("expected2.all","upr2.all") )
store[, educa:=factor(educa, levels = levels(dtset2$educa))]
store[, gage:=factor(gage)]
merge(dtset2, store, by = c("nperiod","gage","educa") )   -> dtset2
dtset2[, winter.months:= ifelse(month %in% 4:9, F,T)]

str(dtset2)
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
dtset2[,excess.flu:= ifelse(#epi.flu              & 
  winter.months & 
    dealth.flu0 > upr.flu, dealth.flu0-expected.flu,0)]
dtset2[,excess2.flu:= ifelse(#epi.flu              & 
  winter.months & 
    dealth.flu0 > upr2.flu, dealth.flu0-expected2.flu,0)]
dtset2[,excess.all:= ifelse(#epi.flu              & 
  winter.months &
    dealth.all0 > upr.all, dealth.all0-expected.all,0)]
dtset2[,excess2.all:= ifelse(#epi.flu              & 
  winter.months &
    dealth.all0 > upr2.all, dealth.all0-expected2.all,0)]

## Gráfica esperado y observado
melt(dtset2, 
     id.vars=1:6, 
     measure.vars= c("dealth.flu0", "dealth.all0", 
                     "expected2.flu", "expected2.all"),
     variable.factor = F )  -> pp
pp[, variable:= gsub('0','',variable)]

pp[, c('type','event') :=  tstrsplit(pp$variable,'\\.')]


qplot(x=month.brand,y=value, color=type, data=pp[event=='all'], geom = 'line') + 
  facet_grid(educa~gage, scales="free" ) + theme_bw() +  flu.bars +
  labs(title='All death: Observed and predicted deaths',
       subtitle = 'Predicted deaths by lineal regresion model for each age and educational level')

qplot(x=month.brand,y=value, color=type, data=pp[event=='flu'], geom = 'line') + 
  facet_grid(educa~gage, scales="free" ) + theme_bw() +  flu.bars +
  labs(title='Influenza: Observed and predicted deaths',
       subtitle = 'Predicted deaths by lineal regresion model for each age and educational level')


## Add total Expected and Observed deaths for annual estimate of
## excess mortality associated with epidemic winter flu episodes

dtset2[winter.months==T,
       .(O.flu=sum(dealth.flu0),
         O.all=sum(dealth.all0),
         E.flu=sum(expected2.flu),
         E.all=sum(expected2.all),
         excess.flu=sum(excess2.flu),
         excess.all=sum(excess2.all),
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


# anual.resu

## Visualize
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

rm(gp01,gp02)  # libera memoria



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
