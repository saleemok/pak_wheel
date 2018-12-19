library(rvest)
library(purrr)
library(pbapply)
library(stringr)
library(data.table)
library(lubridate)


############### extract maximum page information ################

url_base <- "https://www.pakwheels.com/used-cars/search/-/"
webpage <- read_html(url_base)
webpage <- html_nodes(webpage,".pagination a")%>% html_attr('href')
webpage1 <- str_extract(webpage[8],"\\-*\\d+\\.*\\d*")
webpage1 <- as.numeric(webpage1)


page <- c(1:webpage1)
pages <- page[1:500]


####### classified description  ############



classified_data <- function(urls) {
  
  
  
  
  out <-  tryCatch(
    {
      
      
      data_html <- read_html(urls)
      
      heading <- html_text(html_nodes(data_html,".col-md-8 h1"))
      heading <- heading[1]
      address <-html_text(html_nodes(data_html,".detail-sub-heading a"))
      address <- address[1]
      price <-html_text(html_nodes(data_html,".generic-blue strong"))
      
      price <- price[1]
      specification <-html_text(html_nodes(data_html,".table td"))
      yom <- specification[1]
      yom<- trimws(yom)
      km <- specification[2]
      km<- trimws(km)
      fueltype <- specification[3]
      fueltype <- trimws(fueltype)
      transmission <- specification[4]
      transmission<- trimws(transmission)
      reg_city <-html_nodes(data_html,xpath = '//*[@id="scroll_car_info"]/ul[1]/li[2]')%>% html_text()
      reg_city <- reg_city[1]
       color <-html_nodes(data_html,xpath = '//*[@id="scroll_car_info"]/ul[1]/li[4]')%>% html_text()
      color <- color[1]
       assembly <-html_nodes(data_html,xpath = '//*[@id="scroll_car_info"]/ul[1]/li[6]')%>% html_text()
      assembly<- assembly[1]
       eng_capacity <-html_nodes(data_html,xpath = '//*[@id="scroll_car_info"]/ul[1]/li[8]')%>% html_text()
      eng_capacity <- eng_capacity[1]
       body_type <-html_nodes(data_html,xpath = '//*[@id="scroll_car_info"]/ul[1]/li[12]')%>% html_text()
      body_type <- body_type[1]
      
      
      
      details <- c(vehicle = heading,reg_city = reg_city,color=color,assembly=assembly,eng_capacity=eng_capacity,body_type=body_type,location = address,price = price,year=yom,km=km,fuel_type=fueltype,transmission=transmission,link=urls)

      },
      error=function(cond) {

        
        message(cond)
        # Choose a return value in case of error
        details <- c(vehicle = NA,reg_city=NA,color=NA,assembly=NA,eng_capacity=NA,body_type=NA,location = NA,price=NA,year=NA,km=NA,fuel_type=NA,transmission=NA,link = urls)

    },
    warning=function(cond) {
     
      message(cond)
     
      details <- c(vehicle = NA,reg_city=NA,color=NA,assembly=NA,eng_capacity=NA,body_type=NA,location = NA,price=NA,year=NA,km=NA,fuel_type=NA,transmission=NA,link = urls)
      
       },
    finally={
     
      
    }
  )    
  
  return(out)
  Sys.sleep(5)
  
}




####### fetch each post link  ######

  nodesdata <- function(j) {
   
      
    out <-  tryCatch(
      {
    
    
      urls<- paste0('https://www.pakwheels.com/used-cars/search/-/?page=',j)
      nodes <- read_html(urls)
      internal_page <- paste0(".page-",j," a")
      heading <-html_nodes(nodes,internal_page)%>% html_attr('href')
      heading1 <- paste0("https://www.pakwheels.com/",heading)
      },
   
     error=function(cond) {
      
      
      message(cond)
      return(NA)
    
      
    },
    warning=function(cond) {
      message(cond)
      return(NA)
      
      
    },
    finally={
      
    }
    )    
    
    return(out)
  }
  
###### call nodesdata function#####

data <- pblapply(pages,nodesdata)
data1 <- unlist(data)


##### call classified_data function #####
data2 <- data1[1:10]
data3 <- pblapply(data2,classified_data)

mylist <- data.frame(data3)
mylist <- do.call('rbind',data3)








# mylist <- do.call('rbind',data)
# 
# data1 <- pblapply(mylist, classified_data)

#convert to vector



#generate CSV file

write.csv(mylist,file = 'pak_wheel.csv')


###### cleansing ############

options(scipen = 999)

dataset <- fread('~/pakwheels_501_600_detail.csv',header = TRUE)


# remove row contain 'NA'
dataset_clean <- dataset[complete.cases(dataset[,2:3]),]


#rename column name
names(dataset_clean)[1]<-"sr.no"
names(dataset_clean)[7]<-"post_date"

#column name to lower
colnames(dataset_clean)<-tolower(names(dataset_clean))

#cleaning price column

dataset_clean$price <- ifelse(grepl('lacs',dataset_clean$price),as.numeric(gsub("[^0-9.]", "",dataset_clean$price))*100000,dataset_clean$price)
dataset_clean$price <- ifelse(grepl('crore',dataset_clean$price),as.numeric(gsub("[^0-9.]", "",dataset_clean$price))*10000000,dataset_clean$price)
dataset_clean$price <- ifelse(grepl('PKR',dataset_clean$price),as.numeric(gsub("[^0-9.]", "",dataset_clean$price)),dataset_clean$price)
dataset_clean$price <- ifelse(grepl('Call',dataset_clean$price),as.numeric(0),dataset_clean$price)
dataset_clean$price <- as.numeric(dataset_clean$price)

 
# cleaning engine capacity
dataset_clean$eng_capacity<- gsub('cc','',dataset_clean$eng_capacity)
dataset_clean$eng_capacity  <- trimws(dataset_clean$eng_capacity,'b')
dataset_clean$eng_capacity  <- as.numeric(dataset_clean$eng_capacity)


# post_date format
dataset_clean$post_date <- mdy(dataset_clean$post_date)

# year format
dataset_clean$year  <- trimws(dataset_clean$year,'b')
dataset_clean$year <- as.numeric(dataset_clean$year)

# km reading format
dataset_clean$km<- gsub('km','',dataset_clean$km)
dataset_clean$km<- gsub(',','',dataset_clean$km)
dataset_clean$km  <- trimws(dataset_clean$km,'b')
dataset_clean$km <- as.numeric(dataset_clean$km)
# fuel and transmission format

dataset_clean$fuel_type  <- trimws(dataset_clean$fuel_type,'b')
dataset_clean$transmission  <- trimws(dataset_clean$transmission,'b')


write.csv(dataset_clean,file = 'cleaned_file1.csv')




# plot

library(tidyverse)
ggplot(data = dataset_clean) + 
  geom_point(mapping = aes(x = year, y = price, color = assembly))











