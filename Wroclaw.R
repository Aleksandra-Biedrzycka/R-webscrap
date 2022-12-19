# Zestaw2:

#   1. Zebrać dane dotyczące ofert sprzedaży  miast, za pomocą web scrappingu.
# Miasta takie jak w linku:
#   https://www.bankier.pl/wiadomosc/Ceny-ofertowe-wynajmu-mieszkan-sierpien-2022-Raport-Bankier-pl-8394772.html

#1 Bydgoszcz
#2 Gdańsk
#3 Katowice
#4 Kraków
#5 Lublin
#6 Łódź
#7 Poznań
#8 Szczecin
#9 Warszawa
#10 Wrocław

# 2.Zapisać je do bazy danych, w tabelce o swoim nazwisku.

#10 Wrocław

install.packages("RSelenium")
install.packages("rvest")
install.packages("dplyr")
install.packages("gtools")

library(gtools)
library(dplyr)
library(RSelenium)
library(rvest)
#usuwa obiekty (rozwiazanie problemu z zajetym portem):
rm(rd)
rm(remDr)

remDr$open()
rd <- RSelenium::rsDriver(browser = "chrome",chromever = "108.0.5359.71" )
remDr <- rd[['client']]
remDr$navigate(url)
Sys.sleep(1)
pageFromSelenium <- remDr$getPageSource()[[1]] %>% rvest::read_html()
przyciski <- pageFromSelenium%>%html_elements(".eoupkm71.css-190hi89.e11e36i3")
ileStron <-as.numeric(przyciski[ (length(przyciski))-1 ]%>% html_text())
wektorLinkow<-c()

for ( i in 1:ileStron){
  urll<- paste0("https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/wroclaw?areaMax=38&page=",i)
  remDr$navigate(urll)
  Sys.sleep(1)
  webElement<- remDr$findElement("css","body")
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  pageFromSeleniumL <- remDr$getPageSource()[[1]] %>% rvest::read_html()
  
  linkiElements<-(pageFromSeleniumL%>%html_elements(".css-14cy79a.e3x1uf06")) %>%
    html_nodes("a")%>%html_attr("href")
  wektorLinkow<-c(wektorLinkow,linkiElements)
}
wektorLinkow<-unique(wektorLinkow)
w<-1
miasto<-"wroclaw"
data<-"19.12.2022"
zrobWiersz<- function(w,wektorLinkow,miasto,data,remDr){
  urll<- paste0("https://www.otodom.pl",wektorLinkow[w])
  remDr$navigate(urll)
  Sys.sleep(1)
  webElement<- remDr$findElement("css","body")
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  pageFromSeleniumL <- remDr$getPageSource()[[1]] %>% rvest::read_html()
  cena<-pageFromSeleniumL%>%html_element(".css-8qi9av.eu6swcv19")%>%html_text()
  
  v<-pageFromSeleniumL%>%html_elements(".css-1qzszy5.estckra8")%>%html_text()
  indexy<- seq(1,length(v))
  nazwyKolumn <- v[indexy%%2==1]
  wartosci <-v[indexy%%2==0]
  
  df1<-data.frame( t(wartosci) )
  names(df1)<-nazwyKolumn
  if( !any(is.na(names(df1) )) ) {
    df1<- cbind(df1,miasto)
    df1<- cbind(df1,data=data)
    df1<-cbind(cena=cena,df1)
  }
  df1
}

WroclawDF<-NULL
liczbaLinkow<-length(wektorLinkow)
for( l in 1:liczbaLinkow ){
  skip<-FALSE
  tryCatch(
    temp<-zrobWiersz(l,wektorLinkow,"Wroclaw",data,remDr=remDr),
    error=function(e){
      print(e)
      skip<<-TRUE
    }
  )
  if(skip){next}
  print(names(temp))
  if ( !any(is.na(names(temp))) ){
    if( is.null(WroclawDF) )
      WroclawDF<-temp
    else{
      WroclawDF<-smartbind(WroclawDF,temp )
    }
  }
}
# "!r23_pjatK_23!"
install.packages(c("DBI","RMySQL","rstudioapi"))
library(DBI)
library(RMySQL)
library(rstudioapi)
View(WroclawDF)
con <- DBI::dbConnect(RMySQL::MySQL(),
                      encoding ="UTF-8",
                      host = "51.83.185.240",
                      user = "student",
                      dbname = "rzajecia23",
                      password ="!r23_pjatK_23!"#rstudioapi::askForPassword("Database password")
)

dbGetQuery(con,'SET NAMES utf8')
dbGetQuery(con,'set character set "utf8"')
dbWriteTable(con, "biedrzycka_Wroclaw", WroclawDF, append = FALSE,overwrite=TRUE)

dbListTables(con)
biedrzycka<- tbl(con,"biedrzycka_Wroclaw")
biedrzycka%>%select(cena)
dbDisconnect(con)
