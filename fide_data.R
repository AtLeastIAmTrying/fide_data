#Libraries
#library("robustbase")
library("ggplot2")
library("ggforce")
library("plyr")
library("agricolae")
library("tidyverse")
library("RColorBrewer")
library("Cairo")
library("pals")
library("ggrepel")
library("gridExtra")
library("varhandle")
library("gtools")
library("bbplot")
library("readxl")
library("XML")
library("methods")
library("data.table")

pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png',
               'grid', 'ggpubr', 'scales',
               'bbplot')

# write.table(data, file = "batch_31.07.2022.txt", sep = "\t",
#             row.names = FALSE)

#fix width file read
# # l2015_04 <- c(15, 61, 4, 4, 5, 5, 15, 6, 4, 3, 5, 4)
# l2016_10 <- c(15, 61, 4, 4, 5, 5, 15, 4, 6, 4, 3, 6, 4)
# 
# raw.data <- read.fwf("standard_jun22frl.txt", skip = 1, widths = l2016_10)
# #data_2022_06 <- raw.data
# #raw.data <- xmlToDataFrame("standard_rating_list.xml")
# 
# column.names <- c("FIDE ID", "Name","Federation","Sex","Title","WomanTitle","OtherTitles",
#                    "FideOnlineArenaTitle","Ranking","GamesPlayed","Coefficient","B-Year","Activity")
# # column.names <- c("FIDE ID", "Name","Federation","Sex","Title","WomanTitle","OtherTitles",
# #                   "Ranking","GamesPlayed","Coefficient","B-Year","Activity")
# 
# col.names <- colnames(data_2022_06)
# col.names <- column.names
# colnames(data_2022_06) <- col.names
# rm(col.names, column.names)
# 
# data_2022_06$Month <- "2022-06-01"
# data_2022_06$`B-Year` <-as.numeric(as.character(data_2022_06[,12]))
# 
# data_2022_06$Age <- ifelse (data_2022_06$`B-Year` != 0, 2022-data_2022_06$`B-Year`, "")
# data_2022_06 <- data_2022_06[,-8]

# bind data
# data <- rbind(data_2015_04,data_2015_07, data_2015_10, data_2016_01, data_2016_04, data_2016_07,
#               data_2016_10, data_2017_01, data_2017_04, data_2017_07, data_2017_10, data_2018_01,
#               data_2018_04, data_2018_07, data_2018_10, data_2019_01, data_2019_04, data_2019_07,
#               data_2019_10, data_2020_01, data_2020_04, data_2020_07, data_2020_10, data_2021_01,
#               data_2021_04, data_2021_07, data_2021_10, data_2022_01, data_2022_04, data_2022_06)

# data = data.copy
# data_UKR <- filter(data, Federation == "UKR ")
# data_RUS <- filter(data, Federation == "RUS ")


# rm(data_2015_04,data_2015_07, data_2015_10, data_2016_01, data_2016_04, data_2016_07,
#    data_2016_10, data_2017_01, data_2017_04, data_2017_07, data_2017_10, data_2018_01,
#    data_2018_04, data_2018_07, data_2018_10, data_2019_01, data_2019_04, data_2019_07,
#    data_2019_10, data_2020_01, data_2020_04, data_2020_07, data_2020_10, data_2021_01,
#    data_2021_04, data_2021_07, data_2021_10, data_2022_01, data_2022_04, data_2022_06, l2015_04, l2016_10, raw.data)

# data_1 = filter(data_1, Federation %in% c("USA ", "CHN ", "IND ", "UKR ", "POL "))
# write.table(data_1, file="fide_data.tsv", sep = "\t")




#read data


#Parameters for ggplot
axis.text.font.size = 18
axis.title.font.size = 24
group.labels.font.size = 8
legend.font.size = 18
dodge.value <- 0.5
chibrikov.colors <- c("#7570B3", "#E7298A", "#66A61E", "#D95F02", "#8B8378", "#5F9EA0")


































############################PLOT_AVG_RANKING########################################

data.copy <- data_1
data <- data.copy

#data$Month = as.character(data$Month)
data$Ranking <-as.numeric(as.character(data[,8]))

#data = data.copy
data_UKR <- filter(data, Federation == "UKR ")
data_POL <- filter(data, Federation == "POL ")

data$Ranking <-as.numeric(as.character(data[,8]))
data_UKR$Ranking <-as.numeric(as.character(data_UKR[,8]))
data_POL$Ranking <-as.numeric(as.character(data_POL[,8]))

##ggplot

data %>% group_by(Month) %>% 
  summarise(mean = mean(Ranking, na.rm = T),
            sd = sd(Ranking, na.rm = T)) %>% 
  ungroup() -> data

data_UKR %>% 
  group_by(Month) %>% 
  summarise(mean = mean(Ranking, na.rm = TRUE),
            sd = sd(Ranking, na.rm = TRUE)) %>% 
  ungroup() -> data_UKR

data_POL %>% 
  group_by(Month) %>% 
  summarise(mean = mean(Ranking, na.rm = TRUE),
            sd = sd(Ranking, na.rm = TRUE)) %>% 
  ungroup() -> data_POL

data <- as.data.frame(data)
data$Type = "World"
data_UKR <- as.data.frame(data_UKR)
data_UKR$Type = "Ukraine"
data_POL <- as.data.frame(data_POL)
data_POL$Type = "Poland"


#ggplot

# total <- rbind(data, data_UKR, data_POL)
#total = total[,-1]

# total$mean <-as.numeric(as.character(total[,2]))

# world = total$mean
# world = world[1:30]

plot <- ggplot(NULL, mapping = aes(x = Month, y = mean, color = Type, group = 1))+
  geom_point(data = data, mapping = aes(x = Month, y = mean),
             #color = "black",
             size = 2) +
  geom_point(data = data_UKR, mapping = aes(x = Month, y = mean),
             #color = "black",
             size = 2) +
  geom_point(data = data_POL, mapping = aes(x = Month, y = mean),
             #color = "black",
             size = 2) +
  geom_line(data = data, mapping = aes(x = Month, y = mean),
            linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_line(data = data_UKR, mapping = aes(x = Month, y = mean),
            linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_line(data = data_POL, mapping = aes(x = Month, y = mean),
            linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_hline(yintercept = 1550,
             size = 1,
             colour="#333333") +
  labs(x = "",
       y = "Elo ranking") +
  scale_color_manual(values=chibrikov.colors) + 
  ylim(1550, 2100) +
  #scale_x_continuous(breaks=c(0,0.25,0.5,1.0)) +
  bbc_style() +
  #theme_classic2()
  theme(aspect.ratio=0.5) +
  # guides(color = guide_legend(nrow = 1,
  # byrow = TRUE)) +
  theme(axis.title = ggplot2::element_text(
    size = axis.title.font.size-5,
    face = "bold",
    color = "#222222")) +
  theme(axis.text.y = ggplot2::element_text(size= axis.text.font.size)) +
  theme(axis.text.x = ggplot2::element_text(size= axis.text.font.size, angle = 90)) + 
  theme(plot.margin = margin(0.0, 1.0, 1.0, 1.0, "cm")) +
  theme(legend.text=element_text(size=legend.font.size)) +
  labs(tag = "Authorship: Vadym Chibrikov") +
  theme(legend.position = "top") +
  labs(title = "Average ranking in classical chess")+
theme(plot.tag.position = c(0.18, 0.02))
# theme(plot.tag = element_text(size = 32)) 

ggsave("AVG_RANK_WORLD_UKR_POL.png", width = 30, height = 20, units = "cm", dpi = 600)

rm(data, data_POL, data_UKR, plot)



































#######FEMALE_PLAYERS#####################################

data = data.copy

data = data[,-c(1,2,5,6,7,8,9,10,11,12,14)]
data_UKR <- filter(data, Federation == "UKR ")
data_POL <- filter(data, Federation == "POL ")

#female percentage world
total = ddply(data,.(Sex, Month),nrow)
column.names <- c("Sex", "Month","Frequency")
col.names <- colnames(total)
col.names <- column.names
colnames(total) <- col.names
rm(col.names, column.names)

data = data.copy
total.1. = count(data$Month)
column.names <- c("Month", "Overall")
col.names <- colnames(total.1.)
col.names <- column.names
colnames(total.1.) <- col.names
rm(col.names, column.names)

total.1. = rbind(total.1., total.1.)
total = cbind(total, total.1.)
total = total[,-2]
total <- filter(total, Sex == "F   ")
total$percentage = (total$Frequency/total$Overall)*100
total$Type = "World"
rm(total.1.)

#female percentage Ukraine
total_UKR = ddply(data_UKR,.(Sex, Month),nrow)
column.names <- c("Sex", "Month","Frequency")
col.names <- colnames(total_UKR)
col.names <- column.names
colnames(total_UKR) <- col.names
rm(col.names, column.names)

total.1._UKR = count(data_UKR$Month)
column.names <- c("Month", "Overall")
col.names <- colnames(total.1._UKR)
col.names <- column.names
colnames(total.1._UKR) <- col.names
rm(col.names, column.names)

total.1._UKR = rbind(total.1._UKR, total.1._UKR)
total_UKR = cbind(total_UKR, total.1._UKR)
total_UKR = total_UKR[,-2]
total_UKR <- filter(total_UKR, Sex == "F   ")
total_UKR$percentage = (total_UKR$Frequency/total_UKR$Overall)*100
total_UKR$Type = "Ukraine"
rm(total.1._UKR)

#female percentage Poland
total_POL = ddply(data_POL,.(Sex, Month),nrow)
column.names <- c("Sex", "Month","Frequency")
col.names <- colnames(total_POL)
col.names <- column.names
colnames(total_POL) <- col.names
rm(col.names, column.names)

total.1._POL = count(data_POL$Month)
column.names <- c("Month", "Overall")
col.names <- colnames(total.1._POL)
col.names <- column.names
colnames(total.1._POL) <- col.names
rm(col.names, column.names)

total.1._POL = rbind(total.1._POL, total.1._POL)
total_POL = cbind(total_POL, total.1._POL)
total_POL = total_POL[,-2]
total_POL <- filter(total_POL, Sex == "F   ")
total_POL$percentage = (total_POL$Frequency/total_POL$Overall)*100
total_POL$Type = "Poland"
rm(total.1._POL)

#plot

plot <- ggplot(NULL, mapping = aes(x = Month, y = percentage, colour = Type, group = 1))+
  geom_point(data = total, mapping = aes(x = Month, y = percentage),
             #color = "black",
             size = 2) +
  geom_point(data = total_UKR, mapping = aes(x = Month, y = percentage),
             #color = "black",
             size = 2) +
  geom_point(data = total_POL, mapping = aes(x = Month, y = percentage),
             #color = "black",
             size = 2) +
  geom_line(data = total, mapping = aes(x = Month, y = percentage),
            linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_line(data = total_UKR, mapping = aes(x = Month, y = percentage),
            linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_line(data = total_POL, mapping = aes(x = Month, y = percentage),
            linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_hline(yintercept = 9,
             size = 1,
             colour="#333333") +
  labs(x = "",
       y = "Female players, %") +
  scale_color_manual(values=chibrikov.colors) + 
  ylim(9, 15) +
  #scale_x_continuous(breaks=c(0,0.25,0.5,1.0)) +
  bbc_style() +
  #theme_classic2()
  theme(aspect.ratio=0.5) +
  # guides(color = guide_legend(nrow = 1,
  # byrow = TRUE)) +
  theme(axis.title = ggplot2::element_text(
    size = axis.title.font.size-5,
    face = "bold",
    color = "#222222")) +
  theme(axis.text.y = ggplot2::element_text(size= axis.text.font.size)) +
  theme(axis.text.x = ggplot2::element_text(size= axis.text.font.size, angle = 90)) + 
  theme(plot.margin = margin(0.0, 1.0, 1.0, 1.0, "cm")) +
  theme(legend.text=element_text(size=legend.font.size)) +
  labs(tag = "Authorship: Vadym Chibrikov") +
  theme(legend.position = "top") +
  labs(title = "Percentage of female players in classical chess")+
  theme(plot.tag.position = c(0.18, 0.02))
# theme(plot.tag = element_text(size = 32)) 

ggsave("PERC_FEM_PLAYERS_WORLD_UKR_POL.png", width = 30, height = 20, units = "cm", dpi = 600)

rm(data, data_POL, data_UKR, plot, total, total_POL, total_UKR)
































#### number of players

data = data.copy

data = data[,-c(1,2,5,6,7,8,9,10,11,12,14)]
data_UKR <- filter(data, Federation == "UKR ")
data_POL <- filter(data, Federation == "POL ")

data = ddply(data,.(Month),nrow)
column.names <- c("Month", "Overall")
col.names <- colnames(data)
col.names <- column.names
colnames(data) <- col.names
rm(col.names, column.names)
data$Type = "World"
#data$Overall = log10(data$Overall)
data$percentage = ((data$Overall/data[1,2])*100)-100


data_UKR = ddply(data_UKR,.(Month),nrow)
column.names <- c("Month", "Overall")
col.names <- colnames(data_UKR)
col.names <- column.names
colnames(data_UKR) <- col.names
rm(col.names, column.names)
data_UKR$Type = "Ukraine"
#data_UKR$Overall = log10(data_UKR$Overall)
data_UKR$percentage = ((data_UKR$Overall/data_UKR[1,2])*100)-100



data_POL = ddply(data_POL,.(Month),nrow)
column.names <- c("Month", "Overall")
col.names <- colnames(data_POL)
col.names <- column.names
colnames(data_POL) <- col.names
rm(col.names, column.names)
data_POL$Type = "Poland"
#data_POL$Overall = log10(data_POL$Overall)
data_POL$percentage = ((data_POL$Overall/data_POL[1,2])*100)-100








#plot

plot <- ggplot(NULL, mapping = aes(x = Month, y = percentage, colour = Type, group = 1))+
  geom_point(data = data, mapping = aes(x = Month, y = percentage),
             #color = "black",
             size = 2) +
  geom_point(data = data_UKR, mapping = aes(x = Month, y = percentage),
             #color = "black",
             size = 2) +
  geom_point(data = data_POL, mapping = aes(x = Month, y = percentage),
             #color = "black",
             size = 2) +
  geom_line(data = data, mapping = aes(x = Month, y = percentage),
            linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_line(data = data_UKR, mapping = aes(x = Month, y = percentage),
            linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_line(data = data_POL, mapping = aes(x = Month, y = percentage),
            linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_hline(yintercept = 0,
             size = 1,
             colour="#333333") +
  labs(x = "",
       y = "Increase, %") +
  scale_color_manual(values=chibrikov.colors) + 
  ylim(0, 100) +
  #scale_x_continuous(breaks=c(0,0.25,0.5,1.0)) +
  bbc_style() +
  #theme_classic2()
  theme(aspect.ratio=0.5) +
  # guides(color = guide_legend(nrow = 1,
  # byrow = TRUE)) +
  theme(axis.title = ggplot2::element_text(
    size = axis.title.font.size-5,
    face = "bold",
    color = "#222222")) +
  theme(axis.text.y = ggplot2::element_text(size= axis.text.font.size)) +
  theme(axis.text.x = ggplot2::element_text(size= axis.text.font.size, angle = 90)) + 
  theme(plot.margin = margin(0.0, 1.0, 1.0, 1.0, "cm")) +
  theme(legend.text=element_text(size=legend.font.size)) +
  labs(tag = "Authorship: Vadym Chibrikov") +
  theme(legend.position = "top") +
  labs(title = "Percentage increase of players number by federation")+
  theme(plot.tag.position = c(0.18, 0.02))
# theme(plot.tag = element_text(size = 32)) 

ggsave("PERC_PLAYERS_WORLD_UKR_POL.png", width = 30, height = 20, units = "cm", dpi = 600)

rm(data, data_POL, data_UKR, plot)



























#TOP 100,1000,10000,100000, all
data.copy = data
data = data.copy
data$Ranking <-as.numeric(as.character(data[,8]))


data = data[,-c(1,2,3,4,5,6,7,9,10,11,12,14)]

## Top N highest values by group

#top10
data_10 <- data %>%                
  arrange(desc(Ranking)) %>% 
  group_by(Month) %>%
  slice(1:10)
data_10 <- as.data.frame(data_10)
data_10$Ranking <-as.numeric(as.character(data_10[,1]))
data_10 %>% group_by(Month) %>% 
  summarise(m = mean(Ranking, na.rm = T),
            sd = sd(Ranking, na.rm = T)) %>% 
  ungroup() -> data_10
data_10$Type = "10"

#top100
data_100 <- data %>%                
  arrange(desc(Ranking)) %>% 
  group_by(Month) %>%
  slice(1:100)
data_100 <- as.data.frame(data_100)
data_100$Ranking <-as.numeric(as.character(data_100[,1]))
data_100 %>% group_by(Month) %>% 
  summarise(m = mean(Ranking, na.rm = T),
            sd = sd(Ranking, na.rm = T)) %>% 
  ungroup() -> data_100
data_100$Type = "100"

#top1000
data_1000 <- data %>%                
  arrange(desc(Ranking)) %>% 
  group_by(Month) %>%
  slice(1:1000)
data_1000 <- as.data.frame(data_1000)
data_1000$Ranking <-as.numeric(as.character(data_1000[,1]))
data_1000 %>% group_by(Month) %>% 
  summarise(m = mean(Ranking, na.rm = T),
            sd = sd(Ranking, na.rm = T)) %>% 
  ungroup() -> data_1000
data_1000$Type = "1000"


#top10000
data_10000 <- data %>%                
  arrange(desc(Ranking)) %>% 
  group_by(Month) %>%
  slice(1:10000)
data_10000 <- as.data.frame(data_10000)
data_10000$Ranking <-as.numeric(as.character(data_10000[,1]))
data_10000 %>% group_by(Month) %>% 
  summarise(m = mean(Ranking, na.rm = T),
            sd = sd(Ranking, na.rm = T)) %>% 
  ungroup() -> data_10000
data_10000$Type = "10000"


#top100000
data_100000 <- data %>%                
  arrange(desc(Ranking)) %>% 
  group_by(Month) %>%
  slice(1:100000)
data_100000 <- as.data.frame(data_100000)
data_100000$Ranking <-as.numeric(as.character(data_100000[,1]))
data_100000 %>% group_by(Month) %>% 
  summarise(m = mean(Ranking, na.rm = T),
            sd = sd(Ranking, na.rm = T)) %>% 
  ungroup() -> data_100000 
data_100000$Type = "100000"


#top_all
data %>% group_by(Month) %>% 
  summarise(m = mean(Ranking, na.rm = T),
            sd = sd(Ranking, na.rm = T)) %>% 
  ungroup() -> data_all
data_all$Type = "All"

data_10 <- as.data.frame(data_10)
data_100 <- as.data.frame(data_100)
data_1000 <- as.data.frame(data_1000)
data_10000 <- as.data.frame(data_10000)
data_100000 <- as.data.frame(data_100000)
data_all <- as.data.frame(data_all)

#plot

plot <- ggplot(NULL, mapping = aes(x = Month, y = m, colour = Type, group = 1)) +
  # geom_point(data = data_10, mapping = aes(x = Month, y = m),
  #            #color = "black",
  #            size = 2) +
  # geom_line(data = data_10, mapping = aes(x = Month, y = m),
  #           linetype = "dashed", size = 0.5, color = "#bab8b8") +
  # geom_point(data = data_100, mapping = aes(x = Month, y = m),
  #            #color = "black",
  #            size = 2) +
  # geom_line(data = data_100, mapping = aes(x = Month, y = m),
  #           linetype = "dashed", size = 0.5, color = "#bab8b8") +
  # geom_point(data = data_1000, mapping = aes(x = Month, y = m),
  #            #color = "black",
  #            size = 2) +
  # geom_line(data = data_1000, mapping = aes(x = Month, y = m),
  #           linetype = "dashed", size = 0.5, color = "#bab8b8") +
  # geom_point(data = data_10000, mapping = aes(x = Month, y = m),
  #            #color = "black",
  #            size = 2) +
  # geom_line(data = data_10000, mapping = aes(x = Month, y = m),
  #           linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_point(data = data_100000, mapping = aes(x = Month, y = m),
             #color = "black",
             size = 2) +
  geom_line(data = data_100000, mapping = aes(x = Month, y = m),
            linetype = "dashed", size = 0.5, color = "#bab8b8") +
  # geom_point(data = data_all, mapping = aes(x = Month, y = m),
  #            #color = "black",
  #            size = 2) +
  # geom_line(data = data_all, mapping = aes(x = Month, y = m),
  #           linetype = "dashed", size = 0.5, color = "#bab8b8") +
  geom_hline(yintercept = 2085,
             size = 1,
             colour="#333333") +
  labs(x = "",
       y = "Elo ranking") +
  scale_color_manual(values=chibrikov.colors) + 
  ylim(2085, 2095) +
  #scale_x_continuous(breaks=c(0,0.25,0.5,1.0)) +
  bbc_style() +
  #theme_classic2()
  theme(aspect.ratio=0.5) +
  # guides(color = guide_legend(nrow = 1,
  # byrow = TRUE)) +
  theme(axis.title = ggplot2::element_text(
    size = axis.title.font.size-5,
    face = "bold",
    color = "#222222")) +
  theme(axis.text.y = ggplot2::element_text(size= axis.text.font.size)) +
  theme(axis.text.x = ggplot2::element_text(size= axis.text.font.size, angle = 90)) + 
  theme(plot.margin = margin(0.0, 1.0, 1.0, 1.0, "cm")) +
  theme(legend.text=element_text(size=legend.font.size)) +
  labs(tag = "Authorship: Vadym Chibrikov") +
  theme(legend.position = "none") +
  labs(title = "Average rankings of top 100000 players")+
  theme(plot.tag.position = c(0.18, 0.05))
# theme(plot.tag = element_text(size = 32)) 

ggsave("TOP_100000.png", width = 30, height = 20, units = "cm", dpi = 600)

rm(data, data_POL, data_UKR, plot)










########RANKING_DISTRIBUTION_HISTOGRAM
data.copy = data
data <- data.copy
data <- filter(data, Month == "2022-06-01")
data <- data[,c(8,13)]
data[,1] <-as.numeric(as.character(data[,1]))

#plot
plot <- ggplot(NULL, mapping = aes(x = Ranking, group = 1))+
  geom_histogram(data = data, mapping = aes(x = Ranking), binwidth = 20,
                 na.rm = T, color = "black", fill = "transparent") +
  geom_density(data = data, mapping = aes(y = 20* ..count..), #fill = "blue", alpha = .2,
               stat = "density", position = "identity", adjust = 1, inherit.aes = T) +
  # bbc_style()+
  # geom_hline(yintercept = 0,
  #            size = 1,
  #            colour="#333333") +
  labs(x = "Elo ranking",
       y = "Number of players") +
  scale_color_manual(values=chibrikov.colors) +
  scale_x_continuous(breaks=c(1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,
                              2200,2300,2400,2500,2600,2700,2800,2900,3000)) +
  scale_y_continuous(breaks = c(1000,2000,3000,4000,5000,6000,7000,8000,10000,12000,14000,16000,18000)) +
  theme_classic2() +
  theme(aspect.ratio=0.5) +
  # guides(color = guide_legend(nrow = 1,
  # byrow = TRUE)) +
  theme(axis.title = ggplot2::element_text(
    size = axis.title.font.size-10,
    #face = "bold",
    color = "#222222")) +
  theme(axis.text.y = ggplot2::element_text(size= axis.text.font.size-5)) +
  theme(axis.text.x = ggplot2::element_text(size= axis.text.font.size-5, angle = 0)) +
  theme(plot.margin = margin(0.0, 1.0, 1.0, 1.0, "cm")) +
  theme(panel.background = element_rect(fill='transparent')) +
  # theme(legend.text=element_text(size=legend.font.size)) +
  labs(tag = "Authorship: Vadym Chibrikov") +
  theme(legend.position = "none") +
  labs(title = "Distribution of rankings in classical chess")+
  theme(plot.tag.position = c(0.9, 0.02))
# theme(plot.tag = element_text(size = 32))

plot2 = ggplot(data = NULL, aes(x = Ranking, group = 1)) +
  geom_histogram(data = data, mapping = aes(x = Ranking), binwidth = 10,
                 na.rm = T, color = "black", fill = "transparent") +
  geom_density(data = data, mapping = aes(y = 10* ..count..), #fill = "blue", alpha = .2,
               stat = "density", position = "identity", adjust = 1, inherit.aes = T) +
  xlim (2600, 2900) +
  ylim (0, 40) +
  theme_pubr( base_size = 8,border = TRUE) +
  labs(x = "",
       y = "") +
  theme_classic2() +
  theme(aspect.ratio=0.5) +
  theme(panel.background = element_rect(fill='transparent')) +
  # guides(color = guide_legend(nrow = 1,
  # byrow = TRUE)) +
  theme(axis.title = ggplot2::element_text(
    size = axis.title.font.size-10,
    #face = "bold",
    color = "#222222")) +
  theme(axis.text.y = ggplot2::element_text(size= axis.text.font.size-5)) +
  theme(axis.text.x = ggplot2::element_text(size= axis.text.font.size-5, angle = 0)) +
  theme(plot.margin = margin(0.0, 1.0, 1.0, 1.0, "cm"))



#CORRECT
# plot3 = plot +
#   annotation_custom(ggplotGrob(plot2)), xmin = 2100, xmax = 3000, ymin = 4000, ymax = 7000)
#   # geom_rect(data = data, mapping = aes(xmin = 2300, xmax = 3000, ymin = 2000, ymax = 5000),
#   #           color='black', linetype='dashed', alpha=0) +
# ggsave("plot3.png", width = 30, height = 20, units = "cm", dpi = 600)

































#DENSITY_M_F

data.copy = data
data <- data.copy
data <- filter(data, Month == "2022-06-01")
data <- data[,c(4,8,13)]

data_M <- filter(data, Sex == "M   ")
data_F <- filter(data, Sex == "F   ")
data_M[,2] <-as.numeric(as.character(data_M[,2]))
data_F[,2] <-as.numeric(as.character(data_F[,2]))


plot1 <- ggplot(NULL, mapping = aes(x = Ranking, group = 1))+
  geom_histogram(data = data_M, mapping = aes(x = Ranking), binwidth = 10,
                 na.rm = T, color = "black", fill = "lightskyblue", alpha = 1) +
  geom_histogram(data = data_F, mapping = aes(x = Ranking), binwidth = 10,
                 na.rm = T, color = "black", fill = "lightpink3", alpha = 1) +
  geom_density(data = data_F, mapping = aes(y = 10 * ..count..), #fill = "pink", #alpha = .05,
               stat = "density", position = "identity", adjust = 1, inherit.aes = T) +
  geom_density(data = data_M, mapping = aes(y = 10 * ..count..), #fill = "blue", #alpha = .05,
               stat = "density", position = "identity", adjust = 1, inherit.aes = T) +
  # bbc_style()+
  # geom_hline(yintercept = 0,
  #            size = 1,
  #            colour="#333333") +
  labs(x = "Elo ranking",
       y = "Number of players") +
  scale_color_manual(values=chibrikov.colors) +
  scale_x_continuous(breaks=c(1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,
                              2200,2300,2400,2500,2600,2700,2800,2900,3000)) +
  scale_y_continuous(breaks = c(0,200,400,600,800,1000,1200,1400,1600,1800,2000,2200,2400,2600,2800,3000,3200,3400,3600,3800)) +
  theme_classic2() +
  theme(aspect.ratio=0.5) +
  # guides(color = guide_legend(nrow = 1,
  # byrow = TRUE)) +
  theme(axis.title = ggplot2::element_text(
    size = axis.title.font.size-10,
    #face = "bold",
    color = "#222222")) +
  theme(axis.text.y = ggplot2::element_text(size= axis.text.font.size-5)) +
  theme(axis.text.x = ggplot2::element_text(size= axis.text.font.size-5, angle = 0)) +
  theme(plot.margin = margin(0.0, 1.0, 1.0, 1.0, "cm")) +
  theme(panel.background = element_rect(fill='transparent')) +
  # theme(legend.text=element_text(size=legend.font.size)) +
  labs(tag = "Authorship: Vadym Chibrikov") +
  theme(legend.position = "none") +
  labs(title = "Distribution of rankings in classical chess") +
  theme(plot.tag.position = c(0.9, 0.02))
# theme(plot.tag = element_text(size = 32))
ggsave("plot4.png", width = 30, height = 20, units = "cm", dpi = 600)


plot3 = ggplot(NULL, mapping = aes(x = Ranking, group = 1))+
  geom_histogram(data = data_M, mapping = aes(x = Ranking), binwidth = 5,
                 na.rm = T, color = "black", fill = "lightskyblue", alpha = 1) +
  geom_histogram(data = data_F, mapping = aes(x = Ranking), binwidth = 5,
                 na.rm = T, color = "black", fill = "lightpink3", alpha = 1) +
  geom_density(data = data_F, mapping = aes(y = 5 * ..count..), #fill = "pink", #alpha = .05,
               stat = "density", position = "identity", adjust = 1, inherit.aes = T) +
  geom_density(data = data_M, mapping = aes(y = 5 * ..count..), #fill = "blue", #alpha = .05,
               stat = "density", position = "identity", adjust = 1, inherit.aes = T) +
  # bbc_style()+
  # geom_hline(yintercept = 0,
  #            size = 1,
  #            colour="#333333") +
  scale_color_manual(values=chibrikov.colors) +
  xlim (2400, 2900) +
  ylim (0, 220) +
  theme_classic2() +
  theme(aspect.ratio=0.5) +
  # guides(color = guide_legend(nrow = 1,
  # byrow = TRUE)) +
  labs(x = "",
       y = "") +
  theme(axis.title = ggplot2::element_text(
    size = axis.title.font.size-10,
    #face = "bold",
    color = "#222222")) +
  theme(axis.text.y = ggplot2::element_text(size= axis.text.font.size-5)) +
  theme(axis.text.x = ggplot2::element_text(size= axis.text.font.size-5, angle = 0)) +
  theme(plot.margin = margin(0.0, 1.0, 1.0, 1.0, "cm")) +
  theme(panel.background = element_rect(fill='transparent')) +
  # theme(legend.text=element_text(size=legend.font.size)) +
  theme(legend.position = "none") +
  theme(plot.tag.position = c(0.9, 0.02))
# theme(plot.tag = element_text(size = 32))

plot4 = plot +
  annotation_custom(ggplotGrob(plot3), xmin = 2100, xmax = 2900, ymin = 1000, ymax = 3000)
# geom_rect(data = data, mapping = aes(xmin = 2300, xmax = 3000, ymin = 2000, ymax = 5000),
#           color='black', linetype='dashed', alpha=0) +


ggsave("plot.png", width = 30, height = 20, units = "cm", dpi = 600)

