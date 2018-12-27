
library(easysorter)
library(ggridges)
library(tidyverse)


#### data processing

###Read all data

raw_data <- read_data("~/WormGrowth-CQuB/raw_data/20181115_NaAzide_score/score/p01_grow_NaN3_None.txt") 

plate <- raw_data %>%
  filter(call50 == "object" & !condition =="Wash") 

write.csv(plate, "~/WormGrowth-CQuB/processed_data/20181115_NaAzide_score/20181115_NaAzide_processed.csv", row.names = F, quote = FALSE)



### Summarize all data by each well

## Define a vector of your experiement directories
dirs <- "~/WormGrowth-CQuB/raw_data/20181115_NaAzide_score"

# Read in the data
raw <- read_data(dirs)

# Summarize the data
summedraw <- sumplate(raw, directories = FALSE, quantiles = TRUE) %>%
  filter(!condition %in% c("Wash",NA))

write.csv(summedraw, "~/WormGrowth-CQuB/processed_data/20181115_NaAzide_score/20181115_NaAzide_Summarized.csv", row.names = F, quote = FALSE)




####Plots


###All data distribution

plate <- plate %>%
  separate(condition,into = c('Time','Measure'),sep = "_") %>%
  filter(TOF<750)


plate$SamplingTime = factor(plate$Time,levels = c("2.5","3.5","4.5","5.5","6.5","7.5","24","25","26","27","29","48"))

plate$Measure[is.na(plate$Measure)] <- "m0"



dist <- ggplot(plate, aes(x=TOF,color=Measure,fill=Measure)) + 
  geom_histogram(position="identity", alpha=0.8,binwidth = 30) + 
  facet_wrap(.~SamplingTime)



ggsave(dist, filename = paste("~/WormGrowth-CQuB/plots_tables/20181115_NaAzide_score/AllData_histogram.png",sep=""), height = 8, width = 12)


ridge <- ggplot(plate, aes(x = TOF, y = SamplingTime,fill=Measure)) +
  stat_density_ridges(alpha=0.8) + 
  theme(text = element_text(size=18), strip.text.x = element_text(size = 15))

ggsave(ridge, filename = paste("~/WormGrowth-CQuB/plots_tables/20181115_NaAzide_score/AllData_ridge.png",sep=""), height = 8, width = 12)




###summarized 
# Time points : "2.5","2.5_m5", "24","24_m5","48","48_m5"
df1 <- summedraw %>%
  filter(condition %in% c("2.5","2.5_m5", "24","24_m5","48","48_m5"))%>%
  mutate(Time=condition)


plt<- ggplot(df1, aes(x=Time, y=mean.TOF))+geom_line()+geom_point()+
  theme(axis.text = element_text(size=15),axis.title=element_text(size=15)) + scale_x_discrete(name="SamplingTimePoint_MeasureAfter~Hours")


ggsave(plt, filename = paste("~/WormGrowth-CQuB/plots_tables/20181115_NaAzide_score/Summarized_3TimePoints_meanTOF.png",sep=""), height = 8, width = 12)



# All time points
df2 <- summedraw 

df2$Time = factor(df2$condition,levels = c("2.5","2.5_m5","3.5", "3.5_m4","4.5","4.5_m3","5.5","5.5_m2","6.5","6.5_m1","7.5","24","24_m5","25","25_m4","26","26_m3","27","27_m2","29","48","48_m5"))


plt1 <- ggplot(df2, aes(x=Time, y=mean.TOF)) + 
  geom_line() + 
  geom_point()+
  theme(axis.text = element_text(size=15),axis.title=element_text(size=15)) + 
  scale_x_discrete(name="SamplingTimePoint_MeasureAfter~Hours")


ggsave(plt1, filename = paste("~/WormGrowth-CQuB/plots_tables/20181115_NaAzide_score/Summarized_AllTimePoints_meanTOF.png",sep=""), height = 8, width = 16)



