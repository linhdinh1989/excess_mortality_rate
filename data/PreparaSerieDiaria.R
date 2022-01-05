require (data.table)
require(lattice)
require(zoo)
require(car)
rm(list=ls())

load(file='../data/GripeAndaluciaCC.RData')

## -----------------------------------------------------
## Recódifica variables y crea varibles de agrupacion:
## -----------------------------------------------------
etSexo <-  c('Men','Women')

etEdu7 <-c(  'Iliterate'
           , 'Basic education'
           , 'Primary education'
           , 'Lower 2ª education'
           , 'Upper 2ª education'
           , 'First stage of 3ª education & post-secondary'
           , 'Second stage of 3ª education' )

etEdu4 <-c(  'Iliterate','Basic education'
             , 'Primary education'
             , 'Secondary & Tertiary Education' )

RecodeEdu <- "1='Iliterate';
              2='Basic education';
              3='Primary education';
              4:7='Secondary & Tertiary Education'"


pob$EDU4 <-  factor(recode(pob$EDU7EXH,RecodeEdu),levels = etEdu4)
def$EDU4 <-  factor(recode(def$EDU7EXH,RecodeEdu),levels = etEdu4)                   


pob$EDU7EXH <- factor(pob$EDU7EXH,labels = etEdu7)
def$EDU7EXH <- factor(def$EDU7EXH,labels = etEdu7)

def$SEXO <- factor(def$SEXO,levels = c(1,6), labels = etSexo )
pob$SEXO <- factor(pob$SEXO,levels = c(1,6), labels = etSexo )

names(def)[6] <- 'Causa.def'
def$Causa.def<- factor(def$Causa.def)
def$Causa.def<- recode(def$Causa.def,
                       "c('A')='Influenza.LRI';
               else = 'Other'")
def$FBAJA <- as.Date(as.character(def$FBAJA), '%Y%m%d')   ## Crea objecto tipo "date"
pob$FECHA <- as.Date(as.character(pob$FECHA), '%Y%m%d')

def$FRQ <- as.numeric(def$FRQ)


## Convierte en un objeto de tipo 'data.table' y filtra edaa
def <- as.data.table(def)  
def <- subset(def, FBAJA < as.Date('20140701', "%Y%m%d") ) ## Quita último mes (no completo)
pob <- as.data.table(pob)
rm(etEdu4,etEdu7,etSexo,RecodeEdu)

class(def)
def[,.(casos=sum(FRQ)),keyby=.(EDU4)] -> kk ; levels(kk$EDU4)

##--------------------------------
##  Simplifica:
##   + Agrega eventos y pesonas-año en 4 categorias educativas
##   + Filtra para mayores de 60
##   + Suprime categoria Provincia (CPRO)
##-------------------------------

a.def <- def[EDAD5G>=60,.(frq=sum(FRQ)),keyby=.(day=FBAJA,educa=EDU4,g.age=EDAD5G,sex=SEXO,cause=Causa.def)]
x.pob <- pob[EDAD5G>=60,.(frq=sum(FRQ)),keyby=.(day=FECHA,educa=EDU4,g.age=EDAD5G,sex=SEXO)]

##----------------------------------------------------------------------------
## Las poblaciones estan estimadas a primero de mes.
## Hay que interpolar (linealmente) a dia exacto: desde primero a primero del 
## mes siguiente ..
##----------------------------------------------------------------------------

x.pob[educa=='Iliterate' & g.age==60 & sex=='Men'] ->  pp   # interpolar linealmente 
                                                         # esta serie (nodos 1 mes)
# x.pob[,sum(frq) ,keyby=day]
range(x.pob$day) -> kk  # "2002-01-01" "2014-07-01"
seq(kk[1],kk[2], by=1) -> days 

with(pp, approx(x=day, y = frq, xout=days) ) -> kk ; str(kk)

x.pob[,{kk=approx(x=day, y = frq, xout=days);
         list(day=kk$x, persons.year=kk$y/365.25) },
         keyby=.(educa,g.age,sex)] -> a.pob   # Poblacion diaria / 365.25 = person-year

## borramos objetos de datos fuentes y temporales:
rm(def,pob,x.pob,pp,kk,days)

## Suprimimos por agregacion sexo, no demandado:
cat(names(a.def))

a.def[,.(deaths=sum(frq)),.(day,educa, g.age,cause) ] -> a.def
a.pob[,.(persons.year=sum(persons.year)),.(day,educa, g.age) ] -> a.pob


##--------------------------------------------------
## Ahora unimos defunciones por causa y personas-año
## en un único fichero por dia de ocurrencia ...
##--------------------------------------------------

## 1º desplegamos la variable causa de muerte, del fichero defunciones, en columnas:
dcast(a.def,   day+ educa +g.age ~ cause  , value.var = 'deaths') -> a.def2 ; 

## Integramos con población
comunes <-  intersect(names(a.pob),names(a.def2))

merge(a.pob, a.def2, by=comunes, all.x = T) -> a.pd

## Sustituye NA por 0 ...
a.pd[is.na(Influenza.LRI), Influenza.LRI:=0]
a.pd[is.na(Other), Other:=0]



##--------------------------------------------------------
## Por último desplegamos, segun estructuctura demandada
##--------------------------------------------------------

# Series diarias_ grupos de edad y nivel educativo: gripe, total y personas-año

a.pd[,all.deaths:= Influenza.LRI+Other]

# Agrupamos 90y+
a.pd[, g.age:= ifelse(g.age>90,90,g.age)]
# Agrupamos quiquenios en decenios 
a.pd[, g.age:=10* floor(g.age/10) ]


a.pd[ ,.(persons.year = sum(persons.year), 
         Influenza.LRI = sum(Influenza.LRI), 
         all.deaths= sum(all.deaths)),
        keyby=.(day, educa, g.age)] -> a.pd


### Desplegamos: 
dcast(a.pd, day~educa+g.age,  value.var = 'all.deaths') -> all.deaths
dcast(a.pd, day~educa+g.age,  value.var = 'Influenza.LRI') -> Influenza
dcast(a.pd, day~educa+g.age,  value.var = 'persons.year') -> persons.year

rm(a.def,a.def2,a.pob,comunes)

## Enpaqueta en objeto R
save(all.deaths,Influenza,persons.year, file = 'w20170213.RData')

## Pasa a texto delimitado y empaqueta en zip

## redondea a 3  cifras decimales las personas-tiempo, para reducir tamaño del fichero.

nn<- names(persons.year)[-1]
nn
within(persons.year, {
    for (i in nn) { assign(i,round(get(i),3)) }
        } )    -> persons.year2 

names(persons.year2)
persons.year2$i <- NULL
names(persons.year2)


write.csv(all.deaths,file = 'all.deaths.csv',row.names = F)
write.csv(Influenza,file = 'influenza.csv',row.names = F)
write.csv(persons.year2,file = 'persons.year.csv',row.names = F)

str(all.deaths)

