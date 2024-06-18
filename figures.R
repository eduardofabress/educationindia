setwd("C:/Users/Eduardo/Documents/datafiles")
library(dplyr)
library(ggplot2)

#Figure 1a
library(ggrepel)
edgdp <- read.csv("C:/Users/Eduardo/Documents/datafiles/edgdp.csv")
ggplot(data = edgdp, aes(x = Year, y = Education_Spending, label = ifelse(duplicated(Education_Spending), "", Education_Spending))) +
  geom_line(color = "brown3") +
  geom_point(color = "brown4", size = 3) +
  scale_x_continuous(breaks = seq(2013, 2022, by = 1)) +
  scale_y_continuous(limits = c(2.7, 3.2), breaks = seq(2.7, 3.2, by = 0.5)) +
  labs(title = "Education/GDP ratio",
       x = "",
       y = "% of GDP") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text_repel(segment.size = 0.5, segment.color = "grey50", segment.alpha = 0.5)


#Figure1b
pisadata <- read.csv("C:/Users/Eduardo/Documents/datafiles/pisatidy.csv")
colors_brown <- c("brown4", "brown", "pink")
pisadata <- pisadata %>%
  mutate(Performance = factor(Performance, levels = c("Bottom Performers", "Other", "Top Performers"), ordered = TRUE))
pisadata <- pisadata %>%
  arrange(Performance) %>%
  mutate(Participant = factor(Participant, levels = unique(Participant)))
positions <- pisadata %>%
  group_by(Performance) %>%
  summarise(center = mean(as.numeric(Participant)))

ggplot(pisadata, aes(x = Participant, y = Score, fill = Subject)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Performance in PISA 2009: Reading, Mathematics, and Science",
       x = "", y = "Score") +
  scale_fill_manual(values = colors_brown,
                    labels = c("Reading", "Mathematics", "Science"),
                    name = "Subject") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "top") +
  annotate("text", x = positions$center[positions$Performance == "Bottom Performers"], y = 515, label = "Bottom Performers",
           vjust = -2.3, hjust = 0.5, size = 5, color = "black", fontface = "bold") +
  annotate("text", x = positions$center[positions$Performance == "Top Performers"], y = 515, label = "Top Performers",
           vjust = -2.3, hjust = 0.5, size = 5, color = "black", fontface = "bold") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 600)) +
  guides(fill = guide_legend(title = "Subject"))
rm(colors_brown, positions)

#Figure2a
library(rnaturalearth)
library(rmapshaper)

india <- ne_states(country = "India", returnclass = "sf")
india_simplified <- ms_simplify(india)
stutvar <- read.csv("C:/Users/Eduardo/Documents/datafiles/stutvar.csv")
india_data <- india_simplified %>%
  left_join(stutvar, by = c("name" = "stut"))

rm(india_simplified,india)

ggplot(data = india_data) +
  geom_sf(aes(fill = change), color = "black", size = 0.3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0, na.value = "black") +
  theme_void() +
  labs(
    fill = "% Change",
    title = "Spending to Revenue ratio per State/UT") +
  theme(legend.position = "right",
        legend.text = element_text(size = 15), 
        legend.title = element_text(size = 14))

rm(india_data)
  
##Figure 2b
socialspending <- read.csv("C:/Users/Eduardo/Documents/datafiles/socialspendingshare.csv")
line_color <- "brown4"
regression_color <- "brown2"
ggplot(data = socialspending, aes(x = year, y = educationshare)) +
  geom_line(color = line_color, size = 1, linetype = "solid") +  # solid blue line
  geom_smooth(method = "loess", color = regression_color, se = FALSE, linetype = "dashed") +  # dashed smooth regression line in green
  labs(x = "Year", y = "% Share", title = "Education Share in Social Spending") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, margin = margin(t = 10))) +
  scale_y_continuous(limits = c(30, 45), breaks = seq(30, 45, by = 2)) +
  scale_x_continuous(breaks = seq(2004, 2023, by = 2))
rm(line_color, regression_color)
