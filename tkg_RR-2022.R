library(dplyr)
library(tidyr)

indoor = read.csv(file.choose())
indoor2 = indoor %>%
  filter(SessionID >126) %>% 
  select(SessionID, Observer, DateTime, Year, Month, Hour, Focal.Name,
         Interval.Channel.1.Name, Interval.Channel.1.Value, Interval.Channel.2.Name, Interval.Channel.2.Value,
         Space.Use.Grid.X, Space.Use.Grid.Y, Space.Use.Coordinate.X, Space.Use.Coordinate.Y)
indoor2$DateTime = as.POSIXct(indoor2$DateTime)
indoor2$Interval.Channel.1.Name = replace(indoor2$Interval.Channel.1.Name, indoor2$Interval.Channel.1.Name == "", NA)
indoor3 = indoor2 %>%
  drop_na(Interval.Channel.1.Name)

indoor4 = indoor3 %>%
  # group_by(Interval.Channel.1.Value) %>%
  group_by(Interval.Channel.1.Value) %>%
  summarise(Freq = n()) %>%
  spread(Interval.Channel.1.Value, Freq) %>%
  replace(is.na(.), 0) %>%
  rename(ClimbHzn = "Climbing - Horizontal",
         ClimbVer = "Climbing - Vertical, Up",
         ClimbDwn = "Climbing - Vertical, Down",
         Asleep = "Rest - asleep",
         Awake = "Rest - awake",
         LickArm = "Forearm licking",
         TailSwish = "Tail Swishing",
         Drink = "Drinking",
         Feed = "Feeding",
         Groom = "Grooming",
         Leap = "Leaping",
         Sniff = "Sniffing",
         Yawn = "Yawning",
        Excrete = "Excretion",
        Vigil = "Vigilance")

indoor4 = indoor4 %>% # 
  dplyr::summarise(Total = sum(Bipedal, ClimbHzn,ClimbDwn,ClimbVer,Drink,
                               Excrete,Feed,LickArm,Groom,Leap,OOS,
                               Others,Quadrupedal,Asleep,Awake,
                               Sniff, TailSwish, Vigil, Yawn),
                   Bipedal = Bipedal / Total,
                   ClimbHzn = sum(ClimbHzn) / Total,
                   ClimbDwn = sum(ClimbDwn)/Total,
                   ClimbVer = sum(ClimbVer)/Total,
                   Drink = sum(Drink)/Total,
                   Excrete = sum(Excrete)/Total,
                   Feed = sum(Feed)/Total,
                   LickArm = sum(LickArm)/Total,
                   Groom = sum(Groom)/Total,
                   Leap = sum(Leap)/Total,
                   OOS = sum(OOS)/Total,
                   Others = sum(Others)/Total,
                   Asleep = sum(Asleep)/Total,
                   Awake = sum(Awake)/Total,
                   Sniff = sum(Sniff)/Total,
                   TailSwish = sum(TailSwish)/Total,
                   Vigil = sum(Vigil)/Total,
                   Yawn = sum(Yawn)/Total,
                   Quadrupedal = sum(Quadrupedal)/Total) %>%
  mutate_if(is.numeric, ~ . * 100)



outdoor = read.csv(file.choose())
outdoor2 = outdoor %>% 
  filter(SessionID > 129) %>% 
  select(SessionID, Observer, DateTime, Year, Month, Hour, Focal.Name,
         Interval.Channel.1.Name, Interval.Channel.1.Value, Interval.Channel.2.Name, Interval.Channel.2.Value,
         Interval.Channel.3.Name, Interval.Channel.3.Value, 
         Space.Use.Grid.X, Space.Use.Grid.Y, Space.Use.Coordinate.X, Space.Use.Coordinate.Y)
outdoor2$DateTime = as.POSIXct(outdoor2$DateTime)
outdoor2$Interval.Channel.1.Name = replace(outdoor2$Interval.Channel.1.Name, outdoor2$Interval.Channel.1.Name == "", NA)
outdoor3 = outdoor2 %>%  drop_na(Interval.Channel.1.Name)
  
outdoor4 = outdoor3 %>%
  #group_by(SessionID, Interval.Channel.1.Value) %>%
  group_by(Interval.Channel.1.Value) %>%
  summarise(Freq = n()) %>%
  spread(Interval.Channel.1.Value, Freq) %>%
  replace(is.na(.), 0) %>%
  rename(ClimbHzn = "Climbing - Horizontal",
         ClimbVer = "Climbing - Vertical, Up",
         ClimbDwn = "Climbing - Vertical, Down",
         Asleep = "Rest - sleeping",
         Awake = "Rest - awake",
         LickArm = "Forearm licking",
         TailSwish = "Tail Swishing",
         Drink = "Drinking",
         Feed = "Feeding",
         Groom = "Grooming",
         Leap = "Leaping",
         Sniff = "Sniffing",
         Yawn = "Yawning",
         Excrete = "Excretion",
         Vigil = "Vigilance")

outdoor4 = outdoor4 %>% # 
  dplyr::summarise(Total = sum(ClimbHzn,ClimbDwn,ClimbVer,Drink,
                               Excrete,Feed,LickArm,Groom,Leap,OOS,
                               Others,Quadrupedal,Asleep,Awake,
                               Sniff, TailSwish, Vigil, Yawn),
                   ClimbHzn = sum(ClimbHzn) / Total,
                   ClimbDwn = sum(ClimbDwn)/Total,
                   ClimbVer = sum(ClimbVer)/Total,
                   Drink = sum(Drink)/Total,
                   Excrete = sum(Excrete)/Total,
                   Feed = sum(Feed)/Total,
                   LickArm = sum(LickArm)/Total,
                   Groom = sum(Groom)/Total,
                   Leap = sum(Leap)/Total,
                   OOS = sum(OOS)/Total,
                   Others = sum(Others)/Total,
                   Asleep = sum(Asleep)/Total,
                   Awake = sum(Awake)/Total,
                   Sniff = sum(Sniff)/Total,
                   TailSwish = sum(TailSwish)/Total,
                   Vigil = sum(Vigil)/Total,
                   Yawn = sum(Yawn)/Total,
                   Quadrupedal = sum(Quadrupedal)/Total) %>%
  mutate_if(is.numeric, ~ . * 100)


library(reshape2)
outdoor.plot = melt(outdoor4, id.vars = "Total")
outdoor.plot$site = "outdoor"
indoor.plot = melt(indoor4, id.vars = "Total")
indoor.plot$site = "indoor"
both.plot = rbind(outdoor.plot, indoor.plot)

library(ggplot2)
ggplot(data = both.plot) +
  geom_point(aes(x = variable, y = value, colour = site), size = 4.0, position = position_dodge(width = 0.90)) +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("Behaviours") +
  ylab("Proportion (%)")


#### 1/2 session ####

outdoor2 = outdoor %>% 
  filter(SessionID > 139) %>% 
  filter(!SessionID %in% c(194, 566, 568)) %>%
  select(SessionID, Observer, DateTime, Year, Month, Hour, Focal.Name,
         Interval.Channel.1.Name, Interval.Channel.1.Value, Interval.Channel.2.Name, Interval.Channel.2.Value,
         Interval.Channel.3.Name, Interval.Channel.3.Value, 
         Space.Use.Grid.X, Space.Use.Grid.Y, Space.Use.Coordinate.X, Space.Use.Coordinate.Y)
outdoor2$DateTime = as.POSIXct(outdoor2$DateTime)
outdoor2$Interval.Channel.1.Name = replace(outdoor2$Interval.Channel.1.Name, outdoor2$Interval.Channel.1.Name == "", NA)
outdoor2.5 = outdoor2 %>%
  group_by(SessionID) %>%
  mutate(Index = rep(1:80))

outdoor3 = outdoor2 %>%  drop_na(Interval.Channel.1.Name)



outdoor4 = outdoor3 %>%
  #group_by(SessionID, Interval.Channel.1.Value) %>%
  group_by(Interval.Channel.1.Value) %>%
  summarise(Freq = n()) %>%
  spread(Interval.Channel.1.Value, Freq) %>%
  replace(is.na(.), 0) %>%
  rename(ClimbHzn = "Climbing - Horizontal",
         ClimbVer = "Climbing - Vertical, Up",
         ClimbDwn = "Climbing - Vertical, Down",
         Asleep = "Rest - sleeping",
         Awake = "Rest - awake",
         LickArm = "Forearm licking",
         TailSwish = "Tail Swishing",
         Drink = "Drinking",
         Feed = "Feeding",
         Groom = "Grooming",
         Leap = "Leaping",
         Sniff = "Sniffing",
         Yawn = "Yawning",
         Excrete = "Excretion",
         Vigil = "Vigilance")

outdoor4 = outdoor4 %>% # 
  dplyr::summarise(Total = sum(ClimbHzn,ClimbDwn,ClimbVer,Drink,
                               Excrete,Feed,LickArm,Groom,Leap,OOS,
                               Others,Quadrupedal,Asleep,Awake,
                               Sniff, TailSwish, Vigil, Yawn),
                   ClimbHzn = sum(ClimbHzn) / Total,
                   ClimbDwn = sum(ClimbDwn)/Total,
                   ClimbVer = sum(ClimbVer)/Total,
                   Drink = sum(Drink)/Total,
                   Excrete = sum(Excrete)/Total,
                   Feed = sum(Feed)/Total,
                   LickArm = sum(LickArm)/Total,
                   Groom = sum(Groom)/Total,
                   Leap = sum(Leap)/Total,
                   OOS = sum(OOS)/Total,
                   Others = sum(Others)/Total,
                   Asleep = sum(Asleep)/Total,
                   Awake = sum(Awake)/Total,
                   Sniff = sum(Sniff)/Total,
                   TailSwish = sum(TailSwish)/Total,
                   Vigil = sum(Vigil)/Total,
                   Yawn = sum(Yawn)/Total,
                   Quadrupedal = sum(Quadrupedal)/Total) %>%
  mutate_if(is.numeric, ~ . * 100)