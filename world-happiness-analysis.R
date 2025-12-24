data=read.csv("C:/Users/Pc/OneDrive/Desktop/2019.csv")

View(data)
colnames(data)
head(data)
str(data)
summary(data)

plot(data)

plot(data$Perceptions.of.corruption,data$Score)

top_10 <- data[1:10, ]  # Get the first 10 rows

barplot(top_10$Score, 
        names.arg = top_10$Country.or.region, 
        col = "thistle1", 
        las = 2,  # Rotate country names for readability
        main = "Top 10 Countries (First 10 Rows)",
        xlab = "Country",
        ylab = "Happiness Score")

plot(data$GDP.per.capita,data$Score,
     col="#cc0000",
     pch=19,
     main="GDP vs Score",
     xlab="GDP",
     ylab= "Score"
     )

boxplot(data$Score, 
        main = "Distribution of Happiness Scores",
        ylab = "Happiness Score"
        )

top_10 <- data[1:10, ]  # Get the first 10 rows
boxplot(data[, c("Score", "GDP.per.capita", "Perceptions.of.corruption")], 
        main = "Comparison",
        col = c("lightblue", "lightgreen", "lightpink"))

mean_score <- mean(data$Score, na.rm = TRUE)
print(mean_score)
install.packages("pacman")
library(pacman)
pacman::p_load(pacman,dplyr,GGally,ggplot2,ggthemes,ggvis,httr,lubridate,plotly,rio,rmarkdown,shiny,stringr,tidyr)

glimpse(data)

great_support <- filter(data,Social.support>=1.5)
View(great_support)


# for (i in 1:nrow(data)) {
#   if (data$Score[i] > mean_score) {
#     data$Happiness_Level[i] <- "High"
#   } else if(data$Score[i] == mean_score){
#     data$Happiness_Level[i] <- "Medium"
#   }
#   else {
#     data$Happiness_Level[i] <- "Low"
#   }
#   
# }

data$Happiness_level <- ifelse(data$Score > 6, "High",
                               ifelse(data$Score > 4, "Medium", "Low"))

head(data)
table(data$Happiness_level)

data %>% 
  group_by(Happiness_level)
  


ggplot(data,aes(x=Healthy.life.expectancy,
                y=Score,
                ))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(data, aes(x = Happiness_level, fill = Happiness_level)) +
  geom_bar() +
  labs(x = "Happiness Level",
       y = "Count") +
  theme_minimal()+
  scale_fill_brewer(palette = "Dark2")

top_happy <- data %>%
  arrange(desc(Score)) %>%
  slice(1:10)
top_happy

write.csv(top_happy, "C:/Users/Pc/OneDrive/Desktop/top_10_happy.csv", row.names = FALSE)




