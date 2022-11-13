# Deliverable 1

library(dplyr)

mechacar_data <- read.csv('MechaCar_mpg.csv')

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, mechacar_data)
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, mechacar_data))

# Deliverable 2

suspensioncoil_data <- read.csv('Suspension_Coil.csv')

total_summary <- suspensioncoil_data %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

lot_summary <- suspensioncoil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))

# Deliverable 3
t.test(suspensioncoil_data$PSI,mu=mean(suspensioncoil_data$PSI))

lot1 <- subset(suspensioncoil_data, Manufacturing_Lot == "Lot1")
lot2 <- subset(suspensioncoil_data, Manufacturing_Lot == "Lot2")
lot3 <- subset(suspensioncoil_data, Manufacturing_Lot == "Lot3")

t.test(lot1$PSI, mu=mean(suspensioncoil_data$PSI))
t.test(lot2$PSI, mu=mean(suspensioncoil_data$PSI))
t.test(lot3$PSI, mu=mean(suspensioncoil_data$PSI))



