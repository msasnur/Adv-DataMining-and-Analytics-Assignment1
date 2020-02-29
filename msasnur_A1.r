library(ISLR)
library(dplyr)
library(ggplot2)
library(gridExtra)
safeBabies <- Carseats %>% select("Sales", "Price", "ShelveLoc")

good <- filter(safeBabies,ShelveLoc == 'Good')
medium <- filter(safeBabies,ShelveLoc == 'Medium')
bad <- filter(safeBabies,ShelveLoc == 'Bad')

#Question 1

good_order <- good[order(good$Price),]
model <- lm(good_order$Sales ~ good_order$Price, data = good_order)
summary(model)

good_optimal_price = (-0.065785 *55 - 17.968864)/(2 * -0.065785)
print(good_optimal_price)

result <- vector("numeric", 40)
for(cost in 40:86) {
  good_optimized_result <- (-0.065785 *cost - 17.968864)/(2 * -0.065785)
  result[cost - 40] <- good_optimized_result
}

price<- c(40:85)
good_optimized_price<-cbind.data.frame(result,price)
names(good_optimized_price)<-c('Optimized_Price','Change_in_Cost')

#Question 2

bad_order<-bad[order(bad$Price),]
model_2 <- lm(bad_order$Sales ~ bad_order$Price, data = bad_order)
summary(model_2)

bad_optimal_price = (-0.05522 * 55 - 11.832984) / (2 * -0.05522)
print(bad_optimal_price)

result_1 <- vector("numeric", 40)
for(cost in 40:86) {
  bad_optimized_result <- (-0.05522 *cost - 11.832984)/(2 * -0.05522)
  result_1[cost - 40] <- bad_optimized_result
}

bad_optimized_price<-cbind.data.frame(result_1,price)
names(bad_optimized_price)<-c('Optimized_Price','Change_in_Cost')

plot_good <- ggplot(good_optimized_price, aes(Optimized_Price, Change_in_Cost, colour='Good location')) +
             labs(title = 'Optimized Price varying with cost',x='Optimized Price',y='Cost') +
             geom_line() +
             scale_color_manual("", values = ("Good Price" = "red")) +
             geom_point(colour='black')

plot_bad <- ggplot(bad_optimized_price, aes(Optimized_Price, Change_in_Cost, colour='Bad location')) +
            labs(x='Optimized price',y='Cost') +
            geom_line() +
            scale_color_manual("", values = ("Bad Price" = "blue")) +
            geom_point(colour='black')

grid.arrange(plot_good, plot_bad, ncol=1)