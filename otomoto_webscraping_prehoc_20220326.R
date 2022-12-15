#Go to otomoto
#Find your dreamcar 
#Go to second page of results
#Copy link

library(rvest)
library(stringr)
library(progress)
library(ggplot2)
library(dplyr)


link = 'https://www.otomoto.pl/osobowe/toyota/land-cruiser?page=1'

download.file(link, destfile = "scrapedpage.html", quiet=TRUE)
page <- read_html('scrapedpage.html')

result <- page %>% html_nodes(xpath = '//span[@class="ooa-epvm6 e1b25f6f8"]') %>% html_text()
eur_index <- str_detect(result, "EUR")
result <- as.integer(str_replace_all(result, " |PLN|EUR", ""))
result[eur_index] <- result[eur_index]*4.7
summary(result)

page %>% html_nodes(xpath = '//article/div/div/ul/li[1]') %>% html_text() %>% str_replace(., " ", "") %>% as.integer()


#Exercise 1: Downloading prices from first 5 pages
N = 7
link = 'https://www.otomoto.pl/osobowe/toyota/land-cruiser?page='
results <- data.frame('price'=integer())
pb <- progress_bar$new(total=N)
for(i in 1:N){
  #download.file(paste0(link, i), destfile = paste0("scrapedpage-", i,".html"), quiet=TRUE)
  page <- read_html(paste0("scrapedpage-", i,".html"))
  
  price <- page %>% html_nodes(xpath = '//span[@class="ooa-epvm6 e1b25f6f8"]') %>% html_text()
  eur_index <- str_detect(price, "EUR")
  price <- as.integer(str_replace_all(price, " |PLN|EUR", ""))
  price[eur_index] <- price[eur_index]*4.7
  print(price)
  
  results <- rbind(results, data.frame('price'=price))
  pb$tick()
}
summary(results)
hist(results$price)


#Exercise 2: Downloading prices and year from all pages
N = 7
link = 'https://www.otomoto.pl/osobowe/toyota/land-cruiser?page='
results <- data.frame('price'=integer(), 'year'=integer())
pb <- progress_bar$new(total=N)
for(i in 1:N){
  #download.file(paste0(link, i), destfile = paste0("scrapedpage-", i,".html"), quiet=TRUE)
  page <- read_html(paste0("scrapedpage-", i,".html"))
  
  price <- page %>% html_nodes(xpath = '//span[@class="ooa-epvm6 e1b25f6f8"]') %>% html_text()
  eur_index <- str_detect(price, "EUR")
  price <- as.integer(str_replace_all(price, " |PLN|EUR", ""))
  price[eur_index] <- price[eur_index]*4.7
  #print(price)
  
  year <- page %>% html_nodes(xpath = '//article/div/div/ul[1]/li[1]') %>% html_text() %>% str_replace(., " ", "") %>% as.integer()
  
  results <- rbind(results, data.frame('price'=price, 'year'=year))
  print(dim(results))
  pb$tick()
}
summary(results)
hist(results$year)

plot(x=results$year, y=results$price, pch=19)
summary(lm(price ~ year, data=results))
abline(lm(price ~ year, data=results))

results$year <- 2022-results$year

lm(price ~ year, data=results)
plot(x=results$year, y=results$price, pch=19, xlab="Car age", ylab="Price")
abline(lm(price ~ year, data=results), lwd=2, col="dark red")


fit_p2 <- lm(price ~ year + I(year^2), data=results)
summary(fit_p2)
points(x = 0:50, y=predict(fit_p2, data.frame('year'=0:50)), lwd=2, col="pink", type='l')

fit_p3 <- lm(price ~ year + I(year^2) + I(year^3), data=results)
AIC(fit_p3)
summary(fit_p3)
#poly(results$year, 3)
points(x = 0:50, y=predict(fit_p3, data.frame('year'=0:50)), lwd=2, col="navy blue", type='l')

fit_log <- lm(price ~ year + I(log(year+1)), data=results)
AIC(fit_log)
summary(fit_log)
points(x = 0:50, y=predict(fit_log, data.frame('year'=0:50)), lwd=2, col="darkgoldenrod1", type='l')



#Exercise 3: Get also mileage, engine capacity, fuel type
N = 7
link = 'https://www.otomoto.pl/osobowe/toyota/land-cruiser?page='
results <- data.frame('price'=integer(), 'year'=integer(), 'mileage'=integer(), 'engine'=integer(), 'engine_type'=character())
pb <- progress_bar$new(total=N)
for(i in 1:N){
  #download.file(paste0(link, i), destfile = paste0("scrapedpage-", i,".html"), quiet=TRUE)
  page <- read_html(paste0("scrapedpage-", i,".html"))
  
  price <- page %>% html_nodes(xpath = '//span[@class="ooa-epvm6 e1b25f6f8"]') %>% html_text()
  eur_index <- str_detect(price, "EUR")
  price <- as.integer(str_replace_all(price, " |PLN|EUR", ""))
  price[eur_index] <- price[eur_index]*4.7
  
  year <- page %>% html_nodes(xpath = '//article/div/div/ul[1]/li[1]') %>% html_text() %>% str_replace(., " ", "") %>% as.integer()
  
  mileage <- page %>% html_nodes(xpath = '//article/div/div/ul[1]/li[2]') %>% html_text() %>% str_replace_all(., " |km", "") %>% as.integer()
  
  engine <- page %>% html_nodes(xpath = '//article/div/div/ul[1]/li[3]') %>% html_text() %>% str_replace_all(., " |cm3", "") %>% as.integer()
    
  engine_type <- page %>% html_nodes(xpath = '//article/div/div/ul[1]/li[4]') %>% html_text()
  
  results <- rbind(results, data.frame('price'=price, 'year'=year, 'mileage'=mileage, 'engine'=engine, 'engine_type'=engine_type))
  pb$tick()
}
summary(results)
plot(results)

#Exercise 3B: What about missing data?
N = 7
link = 'https://www.otomoto.pl/osobowe/toyota/land-cruiser?page='
results <- data.frame('price'=integer(), 'year'=integer(), 'mileage'=integer(), 'engine'=integer(), 'fuel'=character())
pb <- progress_bar$new(total=N)
for(i in 1:N){
  #download.file(paste0(link, i), destfile = paste0("scrapedpage-", i,".html"), quiet=TRUE)
  page_nodes <- read_html(paste0("scrapedpage-", i, ".html")) %>% html_nodes(xpath = '//article[@data-testid="listing-ad"]')

  for(node in page_nodes){
    price <- node %>% html_nodes(xpath = './/span[@class="ooa-epvm6 e1b25f6f8"]') %>% html_text()
    eur_index <- str_detect(price, "EUR")
    price <- as.integer(str_replace_all(price, " |PLN|EUR", ""))
    price[eur_index] <- price[eur_index]*4.7

    year <- node %>% html_nodes(xpath = './div/div/ul[1]/li[1]') %>% html_text() %>% str_replace(., " ", "") %>% as.integer()

    mileage <- node %>% html_nodes(xpath = './div/div/ul[1]/li[2]') %>% html_text() %>% str_replace_all(., " |km", "") %>% as.integer()
    
    engine <- node %>% html_nodes(xpath = './div/div/ul[1]/li[3]') %>% html_text() %>% str_replace_all(., " |cm3", "") %>% as.integer()
    
    fuel <- node %>% html_nodes(xpath = './div/div/ul[1]/li[4]') %>% html_text()

    if(length(price) == 0) price <- NA
    if(length(year) == 0) year <- NA
    if(length(mileage) == 0) mileage <- NA
    if(length(fuel) == 0) fuel <- NA
    if(length(engine) == 0) engine <- NA
    
    results <- rbind(results, data.frame('price'=price, 'year'=year, 'mileage'=mileage, 'engine'=engine, 'fuel'=fuel))
  }
  pb$tick()
}
summary(results)
plot(results)

results$car_age <- 2022-results$year
results$year <- NULL

#Exercise 4: Analyze what impacts the price
fit_all <- lm(price ~ ., data=results)
summary(fit_all)
plot(results)

write.csv(results, "toyota.csv")

#Exercise 5: (Metaphysical-Political-ToBeDiscussedWithUncleDuringChristmasEve): When to buy a car?

