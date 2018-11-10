rm(list=ls())

library(stringr)
library(tidyr)
library(RSelenium)
cnames<-c("Total Shots",
          "Woodwork",   
          "Shots on target",
          "Shots off target",   
          "Shots blocked",
          "Pass Success%",
          "Total passes",
          "Accurate passes",   
          "Key passes",
          "Aerials Won",
          "Aerials Won%",
          "Offensive Aerials",
          "Defensive Aerials",
          "Successful Tackles",
          "Tackles Attempted",
          "Was Dribbled",
          "Tackle Success",
          "Clearances",
          "Interceptions")

##ESCOLHENDOO DRIVER (NAVEGADOR)
rdmd <- RSelenium::rsDriver(browser="firefox",port=4444L)

###########################
mainpage<-"https://www.whoscored.com/Teams/7334/Fixtures/Brazil-Ceara"
###########################3

##CARREGANDO O CLIENTE PAR AUSAR O NAVAGADOR
rd<-rdmd$client
##NAVEGANDO PARA A URL DESEJADA
rd$navigate(mainpage)

############ ESCLHER A PARTIDA ######## 
############ A LISTA MATCHES VAI DE 1 ATE 54 ###########
##escolha aqui a partida de acordo com a lista na página
matches <- rd$findElements(using ="xpath",value="//*[contains(@class, 'result-1')]")
Sys.sleep(2)

matches[[23]]$clickElement()

###### pegando os nomes dos times ##############
######@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#########
time=character(2)
rd$getCurrentUrl()[[1]]
year<-str_locate(rd$getCurrentUrl()[[1]],"2018-")[[1]]
Ce<-str_locate(rd$getCurrentUrl()[[1]],"Ceara")[[1]]
tra<- str_locate(rd$getCurrentUrl()[[1]],"ra-")
teste2<-str_sub(rd$getCurrentUrl()[[1]],Ce)
if(str_count(teste2)>5){
  time[1]<-str_sub(rd$getCurrentUrl()[[1]],Ce,tra[1]+1)
  time[2]<-str_sub(rd$getCurrentUrl()[[1]],tra[2]+1)
}else{
  time[2]<-str_sub(rd$getCurrentUrl()[[1]],Ce)
  time[1]<-str_sub(rd$getCurrentUrl()[[1]],year+5,Ce-2)
}



##shotsText <- rd$findElements(using ="xpath",value="//*[contains(@class, 'match-centre-stat-values')]")
#obtendos elementos desejados na pagina
allElements <- rd$findElements(using ="xpath",value="//*[contains(@class, 'match-centre-stat-values')]")
allelements2 <- rd$findElements(using ="xpath",value="//*[contains(@class, 'toggle-stat-details')]")

#pega do os dados desejados
ind<-c(2,4,6,7)
dados<-data.frame(x1=numeric(50),
                  x2=numeric(50),
                  x3=numeric(50),
                  x4=numeric(50))

d=2
for(k in 1:10){
  if(k==2){
    allelements2[[k]]$clickElement()
    Sys.sleep(1)
    for(i in 1:50){
      dados[i,1]=allElements[[i]]$getElementText()
    }
    Sys.sleep(1)
    allelements2[[k]]$clickElement()
    next()
  }
  if( k %in% ind){
    allelements2[[k]]$clickElement()
    Sys.sleep(1)
    if(d<=4){
      for(i in 1:50){
        dados[i,d]=allElements[[i]]$getElementText()
      }
      d=d+1
    }
    allelements2[[k]]$clickElement()
    Sys.sleep(1)
  }
  next()
}

dados <- dados[-c(1,2,8,11,16,21,26,33,36),]
dados1<-c(dados[,1],dados[,2],dados[,3],dados[,4])
dados1<-data.frame(cnames,dados1[dados1!=""])
colnames(dados1)[2]="value"
dados1<-separate(dados1,col = "value",into = c(time[1],time[2]))
dd<-tidyr::gather(dados1,Value,key =time ,-cnames)
dd <-(spread(dd,cnames,Value))
View(dd)
