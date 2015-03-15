######################           Obttain data      ##################

# Read data Activity dataset        
        path <- getwd()
        name <- "activity.csv"
        dt_activity <- read.csv(paste(path, name, sep="/"), sep = ",")
       sum(dt_activity_1$steps)

#####################           Proces Date      ##########################        
# Part 1 - Plot and mead/Medain off step by day      
        # Copy data.frame in new data.frame
                dt_activity_1 <- dt_activity        
        # Subset, exclude NA rows. 
                dt_activity_1 <- dt_activity_1[complete.cases(dt_activity_1),]
        # aggrate by day
                dt_steps_by_day <- as.data.frame(xtabs(steps ~ date  , dt_activity_1))      
        # plot Histogram        
                hist(dt_steps_by_day$Freq )
        # Calcualete mean and Median
                mean_activity_1 <- mean (dt_steps_by_day$Freq)
                median_activity_1 <- median(dt_steps_by_day$Freq)


# Part 2 - plot average 5 Min interval
        # Copy data.frame in new data.frame
                dt_activity_2 <- dt_activity        
        # Subset, exclude NA rows. 
                dt_activity_2 <- dt_activity_2[complete.cases(dt_activity_2),]
        # aggrate by day
                dt_steps_by_day2 <- as.data.frame(aggregate(dt_activity_2$steps, by=list(dt_activity_2$interval), FUN=mean, data = dt_activity_2 ))
        # Max Value
                maxvalue <- max(dt_steps_by_day2$x)
                dt_steps_by_day2_max <- dt_steps_by_day2[dt_steps_by_day2$x ==  maxvalue, ]
        # plot
                plot_part2 <- plot(x=dt_steps_by_day2$Group.1 , y= dt_steps_by_day2$x , type = "l")


# part 3 - missing Value's

        # report number of missing NA
                dt_activity_na <- dt_activity[is.na(dt_activity[,1]),]
                missing_na <- length(dt_activity_na[,1])
        # filing in missing Value's
                # value for missing values
                        mean_not_missing <- mean(dt_activity_1[complete.cases(dt_activity_1),1])
sum(dt_replace_na[complete.cases(dt_replace_na),1])
                # replece na with mean
                        dt_replace_na <- dt_activity
                        dt_replace_na[is.na(dt_replace_na[,1]),1] <- mean_not_missing
dt_steps_by_day_replaced_na <- as.data.frame(xtabs(steps ~ date  , dt_replace_na))
                       
        # plot average 3 min interval
                # aggrate by day
                        dt_replace_na_agg <- as.data.frame(aggregate(dt_replace_na$steps, by=list(dt_replace_na$interval), FUN=mean, data = dt_replace_na))
                # plot
                        plot(x=dt_replace_na_agg$Group.1 , y= dt_replace_na_agg$x , type = "l")



# part 4 - week /  weekend days

        # Add day name to data.frame
                dt_replace_na$day <- weekdays(as.Date(dt_replace_na$date), abbreviate = FALSE)
        # split dataset into weekday and weekenddays
                weekend  <- dt_replace_na[dt_replace_na$day %in% c("zaterdag","zondag"),]
                week     <- dt_replace_na[!(dt_replace_na$day %in% c("zaterdag","zondag")),]
        # aggrate by day
                dt_replace_na1_weekend <- as.data.frame(aggregate(weekend$steps, by=list(weekend$interval), FUN=mean, data = weekend ))
                dt_replace_na1_week <- as.data.frame(aggregate(week$steps, by=list(week$interval), FUN=mean, data = week ))
        # plot
                par(mfrow = c(1,2))
                plot(x=dt_replace_na1_weekend$Group.1 , y = dt_replace_na1_weekend$x , type = "l")
                plot(x=dt_replace_na1_week$Group.1 , y = dt_replace_na1_week$x , type = "l")

