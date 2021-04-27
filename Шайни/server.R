library(shiny)
library(COVID19)
library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tibble)
library(tseries)
library(lattice)

download <- function(country, window_size)
{
    country_data<-covid19(country,level =1,start=today() - days(window_size + 1))
    country_data <- country_data[,2:6]
    country$date <- as.Date(country_data$date,"%m/%d/%y")
    return (country_data)
}


server<-shinyServer(function(input, output) {
    output$death_plot <- renderPlot({
        len_date <- input$past
        len_for <- input$forecast
        Russia <- download("Russia", len_date)
        Italy <- download("Italy", len_date)
        USA <- download("USA", len_date)
        France <- download("France", len_date)
        min_date = min(length(Russia$deaths),length(USA$deaths),
                       length(Italy$deaths),length(France$deaths))
        date <- NULL
        if (length(Russia$deaths) == min_date){
            date <- as.Date(Russia$date[-1],"%m/%d/%y")
        } else if (length(USA$deaths) == min_date){
            date <- as.Date(USA$date[-1],"%m/%d/%y")
        } else if (length(Italy$deaths) == min){
            date <- as.Date(Italy$date[-1],"%m/%d/%y")
        } else if (length(France$deaths) == min){
            date <- as.Date(France$date[-1],"%m/%d/%y")
        }
        
        countries <- data.frame(id = date)
        
        if ("Russia" %in% input$Country) {
            dailyRussia <-list()
            dailyRussia$deaths <- diff(Russia$deaths)[1:length(date)]
            countries$Russia <- dailyRussia$deaths
            
        } 
        
        if ("Italy" %in% input$Country) {
            dailyItaly <-list()
            dailyItaly$deaths <- diff(Italy$deaths)[1:length(date)]
            countries$Italy <- dailyItaly$deaths
            
        }
        
        if ("USA" %in% input$Country) {
            dailyUSA <-list()
            dailyUSA$deaths <- diff(USA$deaths)[1:length(date)]
            countries$USA <- dailyUSA$deaths
            
        }
        
        
        if ("France" %in% input$Country) {
            dailyFrance <-list()
            dailyFrance$deaths <- diff(France$deaths)[1:length(date)]
            countries$France <- dailyFrance$deaths
            
        }
        
        
        if (length(countries) == 1) {
        } else {
            
            if (input$real_for == "Data") {
                y_lab <- "Deaths"
                df <- melt(countries ,  id.vars = 'id')
                
                options(scipen = 999)
                
                
                ggplot(data = df, aes(x = id, y = value,  color = variable)) +
                    geom_line() +
                    labs(color = "Country")+
                    xlab("Date")+
                    ylab(y_lab)+
                    theme_light()+
                    theme(legend.title = element_text(size = 20),
                          legend.text = element_text(size = 16),
                          axis.title = element_text(size = 20),
                          axis.text = element_text(size = 20))
                
            } else{
                
                
                HWmodel <- "additive"
                if (input$model != "Additive") {
                    HWmodel <- "mult"
                } 
                
                future <- future <- tail(date + days(len_for + 1), len_for+1)
                pred_countries <- future
                
                if ("Russia" %in% input$Country) {
                    past_Russia <- ts(data.frame(date, dailyRussia), frequency = 7)
                    past_Russia[!is.finite(past_Russia)] <- 0
                    model_Russia <- HoltWinters(past_Russia,seasonal = HWmodel)
                    pred_Russia <- predict(model_Russia,n.ahead = len_for)
                    pred_Russia <- as.integer(round(pred_Russia))
                    pred_countries <- cbind(pred_countries, pred_Russia)
                    colnames(pred_countries)[length(colnames(pred_countries))] <- "Russia"
                } 
                
                if ("Italy" %in% input$Country) {
                    past_Italy <- ts(data.frame(dailyItaly), frequency = 7)
                    past_Italy[!is.finite(past_Italy)] <- 0
                    model_Italy <- HoltWinters(past_Italy,seasonal = HWmodel)
                    pred_Italy <- predict(model_Italy,n.ahead = len_for)
                    pred_Italy <- as.integer(round(pred_Italy))
                    pred_countries <- cbind(pred_countries, pred_Italy)
                    colnames(pred_countries)[length(colnames(pred_countries))] <- "Italy"
                }
                
                if ("USA" %in% input$Country) {
                    past_USA <- ts(data.frame(dailyUSA), frequency = 7)
                    past_USA[!is.finite(past_USA)] <- 0
                    model_USA <- HoltWinters(past_USA,seasonal = HWmodel)
                    pred_USA <- predict(model_USA,n.ahead = len_for)
                    pred_USA <- as.integer(round(pred_USA))
                    pred_countries <- cbind(pred_countries, pred_USA)
                    colnames(pred_countries)[length(colnames(pred_countries))] <- "USA"
                }
                
                
                if ("France" %in% input$Country) {
                    past_France <- ts(data.frame(dailyFrance), frequency = 7)
                    past_France[!is.finite(past_France)] <- 0
                    model_France <- HoltWinters(past_France,seasonal = HWmodel)
                    pred_France <- predict(model_France,n.ahead = len_for)
                    pred_France <- as.integer(round(pred_France))
                    pred_countries <- cbind(pred_countries, pred_France)
                    colnames(pred_countries)[length(colnames(pred_countries))] <- "France"
                }
                
                colnames(pred_countries)[1] <- "id"
                pred_countries <- data.frame(pred_countries)
                pred_countries$id <- as.Date("1970-01-01", format = "%Y-%m-%d")+pred_countries[,1] 
                countries <- rbind(countries, pred_countries[1,])
                #pred_countries <- join(pred_countries, countries)
                #countries <- join(countries,pred_countries)
                #countries <- rbind(countries, pred_countries)
                countries$pred <- FALSE
                pred_countries$pred <- TRUE
                
                countries <- rbind(countries, pred_countries)
                
                y_lab <- "Deaths"
                df <- melt(countries,  id.vars = c('id','pred') )
                
                
                options(scipen = 999)
                ggplot(data = df, aes(x = id, y = value, color = variable )) +
                    geom_line(aes(linetype = pred)) +
                    geom_vline(xintercept = today()-1, 
                               color = "red",
                               lty = 7) +
                    labs(color = "Country")+
                    guides(fill=FALSE, linetype = FALSE) +
                    xlab("Date")+
                    ylab(y_lab)+
                    theme_light()+
                    theme(legend.title = element_text(size = 20),
                          legend.text = element_text(size = 16),
                          axis.title = element_text(size = 20),
                          axis.text = element_text(size = 20))
                
                
            }
        }
        
    })
    
})
