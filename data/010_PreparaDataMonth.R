require(data.table)
rm(list= ls())

read.csv('all.deaths.csv',  stringsAsFactors =F )   -> all.deaths
read.csv('influenza.csv',  stringsAsFactors =F )    -> influenza
read.csv('persons.year.csv',  stringsAsFactors =F ) -> persons.year

all.deaths$measure <- 'alldeaths'
influenza$measure <- 'influenza'
persons.year$measure <- 'persons.year'

rbind(all.deaths,influenza,persons.year) -> dft

dft$day <- as.Date(dft$day,'%Y-%m-%d')

dft <- as.data.table(dft)

dft[,year:= year(day)]
dft[,month:= month(day)]
dft[,nperiod:= (year-min(year))*12+month  ]
range(dft$nperiod)
dft[nperiod< (max(nperiod)-1) ] -> dft  ## quita ultimo mes datos incopletos
range(dft$nperiod)  # 1 - 149

# Agrega por meses
class(dft)
cbind(names(dft))


dft[ , lapply(.SD[,2:17, with=F],sum) ,keyby=.(nperiod,year,month,measure)] -> dft.month
dft.month[, all:=rowSums(.SD[,5:20]) ]
names(dft.month)[5:20] <- c("il60", "il70", "il80", "il90", "basic60", "basic70",
                             "basic80", "basic90", "pri60", "pri70", "pri80", "pri90",
                              "st60", "st70", "st80", "st90")

names(dft.month)

for (i in  unique(dft.month$measure)) {
    nfile <- paste0(i,'.month.csv') 
    write.csv(dft.month[measure==i,],file=nfile, row.names=F)
}    

## Agrega en solo en los dos grupos de edad usados en test-1

dft.month2 <- dft.month
dft.month2[,
           c("ily", "ilo", "bay", "bao", "pry", "pro", "sty", "sto"):=
             list (il60 + il70, il80 + il90, basic60 + basic70, basic80 +
                   basic90, pri60 + pri70, pri80 + pri90, st60 + st70, st80 + st90)]

dft.month2[,
           c("il60", "il70", "il80", "il90", "basic60", "basic70",
              "basic80", "basic90", "pri60", "pri70", "pri80", "pri90",
              "st60", "st70", "st80", "st90") :=
              list(NULL, NULL, NULL, NULL, NULL,NULL,
                   NULL, NULL, NULL, NULL, NULL, NULL,
                   NULL, NULL, NULL, NULL)  ] -> dft.month2

for (i in  unique(dft.month2$measure)) {
  nfile <- paste0(i,'.month2.csv') 
  write.csv(dft.month2[measure==i,],file=nfile, row.names=F)
}    

          
           

##  Pasa a formato largo
###---- grupos decenales de edad
melt(dft.month, 
     id.vars = c("nperiod", "year", "month",  "measure"), 
     measure.vars=c("il60","il70","il80","il90","basic60","basic70",
                    "basic80","basic90","pri60","pri70","pri80",
                    "pri90","st60","st70","st80","st90","all") ) -> dft.month.long

dft.month.long[,c('educa','gage'):=list(substr(variable,1,2),substr(variable,3,4))  ]
dft.month.long[variable=='all', c('educa','gage'):=list('all','all') ]

names(dft.month2)



###---- grupos 20 años
melt(dft.month2, 
     id.vars = c("nperiod", "year", "month",  "measure"), 
     measure.vars= c("all","ily","ilo","bay","bao","pry",
                     "pro","sty","sto")) -> dft.month2.long

dft.month2.long[,c('educa','gage'):=list(substr(variable,1,2),substr(variable,3,4))  ]
dft.month2.long[variable=='all', c('educa','gage'):=list('all','all') ]

save(dft.month,dft.month.long,
     dft.month2,dft.month2.long,
     file='month.data.Rdata')
