#Download and Import Data
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
df <- read_delim("household_power_consumption.txt", ";", col_names = TRUE, na = c("?"))
str(df)

#Convert Date and Time
df$Time_date <- paste(df$Date,df$Time)
df$Time_date <- strptime(df$Time_date, format = "%d/%m/%Y %H:%M:%S")

#New df3 for plotting 2007-02-01 to 2007-02-02
df2 <- select(df, Global_active_power:Time_date)
df2 <- df2[, c(8, 1:7)]
df3 <- subset(df2, Time_date >= "2007-02-01"& Time_date < "2007-02-03")
df3$Time_date <- as.POSIXct(df3$Time_date)


#Plot 1
hist(df3$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
dev.copy(png,"plot1.png", width=480, height=480)
dev.off()

#Plot 2
plot2 <- ggplot(df3, mapping = aes(Time_date, Global_active_power)) + geom_path() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA)) + 
  scale_x_datetime(date_labels = "%a", date_breaks = 'day') +
  ylab("Global Active Power (kilowatts)") +
  xlab("Time_date") +
  title()

dev.copy(png,"plot2.png", width=480, height=480)
dev.off()