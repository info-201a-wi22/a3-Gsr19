setwd("/Users/garrettruth/Desktop/a3-Gsr19")
IncaData <- read.csv("/Users/garrettruth/Desktop/a3-Gsr19/source/incarceration_trends.csv")
library(dplyr)
# Summary Info
Workable <- select(IncaData, yfips, fips, year, state, county_name, total_pop, urbanicity, region, total_jail_pop, female_jail_pop, male_jail_pop, black_jail_pop, white_jail_pop, white_pop_15to64, black_pop_15to64)
based <- group_by(Workable, region)
MaxBlackPopU <- IncaData 
  MaxBlackPopU <- select(MaxBlackPopU,black_pop_15to64, region) 
  MaxBlackPopU <- filter(MaxBlackPopU,region == "West") 
  MaxBlackPopU <- filter(MaxBlackPopU,TRUE != is.na(black_pop_15to64)) 
  MaxBlackPopU <- filter(MaxBlackPopU,black_pop_15to64 == max(black_pop_15to64))
MaxWhitePopU <- IncaData 
  MaxWhitePopU <- select(MaxWhitePopU,white_pop_15to64, region) 
  MaxWhitePopU <- filter(MaxWhitePopU,region == "West") 
  MaxWhitePopU <- filter(MaxWhitePopU,TRUE != is.na(white_pop_15to64)) 
  MaxWhitePopU <- filter(MaxWhitePopU,white_pop_15to64 == max(white_pop_15to64))
based <- select(based, white_jail_pop, black_jail_pop, white_pop_15to64, black_pop_15to64)
based <- left_join(based, summarize(filter(group_by(Workable, region), TRUE != is.na(white_jail_pop)), HighestWhite = max(white_jail_pop)))
based <- left_join(based, summarize(filter(group_by(Workable, region), TRUE != is.na(black_jail_pop)), HighestBlack = max(black_jail_pop)))
based <- filter(based, black_jail_pop == HighestBlack | white_jail_pop == HighestWhite)
Jailprop <- 13143.92/7036.59
Actualprop <- 623040/2317550
library(ggplot2)
# Plot 1
plot1 <- rename(Workable, Year = year, "Black_Jail_Population" = black_jail_pop, "White_Jail_Population" = white_jail_pop, County = county_name)
plot1 <- filter(plot1, County == "Los Angeles County" | County == "New York County" | County == "Cook County" | County == "Harris County")
plot1 <- filter(plot1, FALSE == is.na(Black_Jail_Population) & FALSE == is.na(White_Jail_Population))
Graph1 <- ggplot( data = plot1) +
  geom_smooth(mapping = aes(x = Year, y = Black_Jail_Population, group = region, color = region))+
  scale_y_continuous(expand = c(0,0), limits = c(0, NA))+
  ggtitle("Black Jail Population Over Time in Largest Cities per Region")
Graph1.5 <- ggplot(data = plot1) +
  geom_smooth(mapping = aes(x = Year, y = White_Jail_Population , group = region, color = region)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, NA)) +
  ggtitle("White Jail Population Over Time in Largest Cities per Region")
# Plot 2
Plot2 <- summarize(group_by(filter(Workable, FALSE == is.na(black_jail_pop), FALSE == is.na(white_jail_pop)), year), Total_Black_Jail_Population = sum(black_jail_pop), Total_White_Jail_Population = sum(white_jail_pop))
Graph2 <- ggplot(data = Plot2) +
  geom_line(mapping = aes(x= Total_White_Jail_Population, y = Total_Black_Jail_Population)) +
  ggtitle("White Jailed Population Compared to Black Jailed Population")
# Map 
MapWork <- select(Workable, fips, year, county_name, state, black_jail_pop, white_jail_pop) %>%
  filter(FALSE == is.na(black_jail_pop), FALSE == is.na(white_jail_pop), 2016 == year) %>%
  mutate(Proportion = black_jail_pop/white_jail_pop)
library("tidyverse")
library(maps)
library(mapproj)
library(patchwork)
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")
mapData <- left_join(county_shapes, MapWork, by = "fips") %>%
  filter("CA" == state)
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )
ColorMap <- ggplot(mapData) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = Proportion ), color = "gray", size = 0.3) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max( mapData$Proportion )), na.value = "white", low = "yellow", high = "red") +
  blank_theme +
  ggtitle("Proportion of Black Jail Population to White Jail Population")












