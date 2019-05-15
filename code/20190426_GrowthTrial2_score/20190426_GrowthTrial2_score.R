library(easysorter)
library(ggridges)
library(tidyverse)


setwd("/Users/grad/Documents/GitHub/WormGrowth-CQuB")

## Define a vector of your experiment directories
## Read data
dirs <- "raw_data/20190426_GrowthTrial2_score"
raw <- read_data(dirs)
score <- raw[[1]]

#### Data processing

### Reading all data

# Dataframe holding info for each worm in each well
plate <- score %>%
  filter(call50 == "object" & !condition =="Wash") %>%
  filter(TOF<1000 & TOF > 90) %>%
  separate(condition, into = c("timepoint","replicate"), sep="_")
write.csv(plate, "processed_data/20190426_GrowthTrial2_score/20190426_GrowthTrial2_Processed.csv", row.names = F, quote = FALSE)


### Summarizing data by well

summedraw <- sumplate(raw, directories = FALSE, quantiles = TRUE) %>%
  filter(!condition %in% c("Wash",NA)) %>%
  filter(!col %in% c("5","11")) %>%
  separate(condition, into = c("timepoint","replicate"), sep="_")
write.csv(summedraw, "processed_data/20190426_GrowthTrial2_score/20190426_GrowthTrial2_Summarized.csv", row.names = F, quote = FALSE)


#### Plots

### Using all data (plate)

# Density plot of all objects at each timepoint
plt1 <- ggplot(plate, aes(x = TOF, y = timepoint)) +
  stat_density_ridges(alpha=0.8, fill="blue") + 
  theme(text = element_text(size=20)) + 
  labs(x= expression(paste("Length (", mu, "m)")),y="Density")
ggsave(plt1, filename = paste("plots_tables/20190426_GrowthTrial2_score/AllData_Density.png",sep=""), height = 8, width = 12)

plt1

# Scatter plot of all objects at each timepoint
plt2 <- ggplot(plate, aes(x = timepoint, y = TOF))+geom_jitter() + 
  labs(x="Hours",y=expression(paste("Length (", mu, "m)"))) +
  theme(text = element_text(size=20))
ggsave(plt2, filename = paste("plots_tables/20190426_GrowthTrial2_score/AllData_Scatter.png",sep=""), height = 8, width = 12)

plt2

# Box plot of all objects at each timepoint
plt3 <- ggplot(plate, aes(x = timepoint, y = TOF))+geom_boxplot() + 
  labs(x="Hours",y=expression(paste("Length (", mu, "m)"))) +
  theme(text = element_text(size=20))
ggsave(plt3, filename = paste("plots_tables/20190426_GrowthTrial2_score/AllData_Box.png",sep=""), height = 8, width = 12)

plt3

# Faceted box plot showing variation between biological replicates
plt4 <- ggplot(plate, aes(y = TOF, color = replicate))+
  geom_boxplot() + facet_wrap(~timepoint) + 
  labs(x="Hours",y=expression(paste("Length (", mu, "m)"))) +
  theme(text = element_text(size=16))
ggsave(plt4, filename = paste("plots_tables/20190426_GrowthTrial2_score/AllData_Boxfaceted.png",sep=""), height = 8, width = 12)

plt4


### Using summarized data

# Mean TOF values per well
plt5 <- ggplot(summedraw, aes(x=timepoint, y=mean.TOF))+geom_boxplot()+geom_jitter()+
  theme(text=element_text(size=18)) + labs(title="Averaged by well",x="Hours",y=expression(paste("Mean Length (", mu, "m)")))
ggsave(plt5, filename = paste("plots_tables/20190426_GrowthTrial2_score/Summarized_meanTOFbywell.png",sep=""), height = 8, width = 12)

plt5

## Averaging technical reps within each timepoint
summedall <- summedraw %>%
  group_by(timepoint, replicate, control, strain) %>%
  summarize(mean.TOF = mean(mean.TOF, na.rm = TRUE))

# Plot
plt6 <- ggplot(summedall, aes(x=timepoint, y=mean.TOF))+geom_boxplot()+geom_jitter()+
  theme(text = element_text(size=18))+
  labs(title ="Averaged by technical replicate",x="Hours",y=expression(paste("Mean Length (", mu, "m)")))
ggsave(plt6, filename = paste("plots_tables/20190426_GrowthTrial2_score/Summarized_meanTOF_by_Technicalrep.png",sep=""), height = 8, width = 12)

plt6 

## Average biological replicates within each timepoint
summedall2 <- summedraw %>%
  group_by(timepoint, control, strain) %>%
  summarize(mean.TOF = mean(mean.TOF, na.rm = TRUE))

# Plot
plt7 <- ggplot(summedall2, aes(x=timepoint, y=mean.TOF))+geom_point()+
  theme(text = element_text(size=18))+
  labs(title ="Mean length over time", x="Hours",y=expression(paste("Mean Length (", mu, "m)")))
ggsave(plt7, filename = paste("plots_tables/20190426_GrowthTrial2_score/Summarized_meanTOF_by_Biologicalrep.png",sep=""), height = 8, width = 12)

plt7

# Plot number of replicates measured per timepoint
plt8 <- ggplot(summedraw, aes(x=timepoint, y=n))+geom_boxplot()+geom_jitter()+ 
  theme(text=element_text(size=18)) + labs(x = "Hours", y=expression(paste("Number of objects")))
ggsave(plt8, filename = paste("plots_tables/20190426_GrowthTrial2_score/Number_of_Objects.png",sep=""), height = 8, width = 12)

plt8



