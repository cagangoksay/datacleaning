my_data <- read.csv("ASSIST_DATA.csv")
library(tidyverse)
md <- my_data
head(md)


head(md)
tail(md)
View(md)
md$Team
md[ ,3]
install.packages("tidyverse")
library("tidyverse")
require("tidyverse")

md |> 
  select(Player, Team, Country) |> 
  filter(Country == "GER" & Team == "Borussia Dortmund") |> 
  arrange(Player)
#This code selects Player, team and Country names. 
#Then filter country as a German and team as a Borussia Dortmund and order player your output for player names.


glimpse(md)
#This code shows all datas from our data set with the categories.
class(md$Country)
#This code shows cassification(integer or character) of the column which I wrote.
unique(md$Country)
#This code shows all options at the Country category. 
mean(md$Actual.Assists, na.rm = TRUE)
#This code shows average value of the Actual Assist datas.

md |> 
  select(Rank, Team, Expected.Assists, Actual.Assists) |> 
  na.omit()
#This code deletes all NA rows from our output.

md %>%
  select(Rank, Player, Team, Expected.Assists, Actual.Assists) %>%
  filter(!complete.cases(.))
#This code shows eliminated row. But we have to use "%>%" this pipe operator. (Hocaya sor!)

md |> 
  select(Rank, Player, Team, Expected.Assists, Actual.Assists) %>%
  filter(!complete.cases(.)) %>%
  mutate(Team = ifelse(is.na(Team) | Team == "" | Team == " ", "none", Team)) %>%
  mutate(Expected.Assists = ifelse(is.na(Expected.Assists) | Expected.Assists == "" | Expected.Assists == " ", 0, Expected.Assists)) |> 
  mutate(Actual.Assists = ifelse(is.na(Actual.Assists) | Actual.Assists == "" | Actual.Assists == " ", 0, Actual.Assists))
#This code is designed to fill empty rows.
#If an integer row is empty, it will write 0; if a character row is empty, it will write "None".


md_duplicates <- md %>%
  filter(duplicated(Player) | duplicated(Player, fromLast = TRUE)) %>%
  arrange(Player)
  view(md_duplicates)
#This code finds duplicated rows.



# To see the duplicated rows.

