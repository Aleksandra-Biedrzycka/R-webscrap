library(RSelenium)
library(rvest)
#usuwa obiekty (rozwiazanie problemu z zajetym portem):
rm(rd)
rm(remDr)

url<-"https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/lublin?areaMax=38&page=1"
read_html(url)%>%html_nodes(".eoupkm71.css-1lc8b1f.e11e36i3")
#read_html("./stronka.html")%>%html_node(".eoupkm71.css-1lc8b1f.e11e36i3")

remDr$open()
rd <- RSelenium::rsDriver(browser = "chrome",chromever = "108.0.5359.71" )
remDr <- rd[['client']]
remDr$navigate(url)
Sys.sleep(1)
pageFromSelenium <- remDr$getPageSource()[[1]] %>% rvest::read_html()
przyciski <- pageFromSelenium%>%html_elements(".eoupkm71.css-1lc8b1f.e11e36i3")
ileStron <-as.numeric(przyciski[ (length(przyciski))-1 ]%>% html_text())
wektorLinkow<-c()
for ( i in 1:ileStron){​​
  urll<- paste0("https://www.otodom.pl/pl/oferty/sprzedaz/mieszkanie/lublin?areaMax=38&page=",i)
  remDr$navigate(urll)
  Sys.sleep(1)
  webElement<- remDr$findElement("css","body")
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  webElement$sendKeysToElement(list(key="end"))
  Sys.sleep(1)
  pageFromSeleniumL <- remDr$getPageSource()[[1]] %>% rvest::read_html()
  linki<- (pageFromSeleniumL%>%html_elements(".css-14cy79a.e3x1uf06") ) %>%
    html_elements(".css-p74l73.es62z2j19")%>%html_node("a")%>%html_attr("href")
  wektorLinkow<-c(wektorLinkow,linki)
  #linkiElements<- pageFromSeleniumL%>%html_nodes(".css-b2mfz3.es62z2j16")
  #linkiElements%>%html_attr("href")
}​​
wektorLinkow<-unique(wektorLinkow)
w<-1
miasto<-"lublin"
data<-"04.12.2022"
zrobWiersz<- function(w,wektorLinkow,miasto,data,remDr){​​
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
  
  #css-1qzszy5 estckra8
  v<-pageFromSeleniumL%>%html_elements(".css-1qzszy5.estckra8")%>%html_text()
  indexy<- seq(1,length(v))
  nazwyKolumn <- v[indexy%%2==1]
  wartosci <-v[indexy%%2==0]
  
  df1<-data.frame( t(wartosci) )
  names(df1)<-nazwyKolumn
  if( !any(is.na(names(df1) )) ) {​​
    df1<- cbind(df1,miasto)
    df1<- cbind(df1,data=data)
    df1<-cbind(cena=cena,df1)
  }​​
  df1
}​​
install.packages("gtools")
library(gtools)
miastaDF<-NULL
liczbaLinkow<-length(wektorLinkow)
for( l in 1:4 ){​​
  skip<-FALSE
  tryCatch(
    temp<-zrobWiersz(l,wektorLinkow,"Lublin",data,remDr=remDr),
    error=function(e){​​
      print(e)
      skip<<-TRUE
    }​​ 
  )
  if(skip){​​next}​​
  print(names(temp))
  if ( !any(is.na(names(temp))) ){​​
    if( is.null(miastaDF) )
      miastaDF<-temp
    else{​​
      miastaDF<-smartbind(miastaDF,temp )
    }​​
  }​​
}​​
# "!r23_pjatK_23!"
install.packages(c("DBI","RMySQL","rstudioapi"))
library(DBI)
library(RMySQL)
library(rstudioapi)
View(miastaDF)
con <- DBI::dbConnect(RMySQL::MySQL(),
                      encoding ="UTF-8",
                      host = "51.83.185.240",
                      user = "student",
                      dbname = "rzajecia23",
                      password ="!r23_pjatK_23!"#rstudioapi::askForPassword("Database password")
)

dbGetQuery(con,'SET NAMES utf8')
dbGetQuery(con,'set character set "utf8"')
dbWriteTable(con, "kwasniewicz_miasta", miastaDF, append = FALSE,overwrite=TRUE)
install.packages("dplyr")
library(dplyr)
dbListTables(con)
kwasniewicz<- tbl(con,"kwasniewicz_miasta")
kwasniewicz%>%select(cena)
dbDisconnect(con)
Mieszkania na sprzedaż w Lublin - www.otodom.plZobacz 741 ogłoszeń o mieszkania na sprzedaż w Lublin. Znajdź te i wiele innych ofert z rynku nieruchomości w kategorii mieszkania do wynajęcia, ale także sprzedaży i wynajmu domów, działek, pokojó...www.otodom.pl<https://teams.microsoft.com/l/message/19:RhtNiHrPlyBYWxbArST2OOH0yK_zC_nfqqgvS8LzDUk1@thread.tacv2/1670172987416?tenantId=ae65f568-0ceb-42c2-9dda-731b9c16e6b4&amp;groupId=5ddbb05f-e85b-4b72-ba9f-0cf50a0161db&amp;parentMessageId=1670156115998&amp;teamName=r2023z&amp;channelName=Ogólny&amp;createdTime=1670172987416&amp;allowXTenantAccess=false>