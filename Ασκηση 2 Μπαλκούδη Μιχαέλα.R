######Ασκηση 2η###########
###Όνομα:Μπαλκούδη Μιχαέλα
###ΑΕΜ:15994
#Ερώτημα 1
install.packages("RSQLite")
library(DBI)
con <- DBI::dbConnect(RSQLite::SQLite(), "/Users/user/Desktop/database.sqlite")
con
dbListTables(con)
##Ερώτημα2:
dbReadTable(con, "Country")
str(dbReadTable(con,"Country"))
dbReadTable(con, "League")
str(dbReadTable(con,"League"))
dbReadTable(con, "Match")
str(dbReadTable(con,"Match"))
dbReadTable(con, "Player")
str(dbReadTable(con,"Player"))
dbReadTable(con, "Player_Attributes")
str(dbReadTable(con,"Player_Attributes"))
dbReadTable(con, "Team")
str(dbReadTable(con,"Team"))
dbReadTable(con, "Team_Attributes")
str(dbReadTable(con,"Team_Attributes"))
dbReadTable(con, "sqlite_sequence")
str(dbReadTable(con,"sqlite_sequence"))
install.packages("tidyverse")
library(tidyverse)
##Ερώτημα 3, δημιουργία δεικτών και υπολογισμός διαστάσεων
country<-tbl(con,"Country")
head(country)
dim(country)
match<-tbl(con,"Match")
head(match)
dim(match)
league<-tbl(con,"League")
head(league)
dim(league)
player<-tbl(con,"Player")
head(player)
dim(player)
player_attributes<-tbl(con,"Player_Attributes")
head(player_attributes)
dim(player_attributes)
team<-tbl(con,"Team")
head(team)
dim(team)
team_attributes<-tbl(con,"Team_Attributes")
head(team_attributes)
dim(team_attributes)
###Ερώτημα 4, 
#ερώτημα1 ασκσησης 1
erwtima1<-dbGetQuery(con,"SELECT player_name,height,weight FROM Player WHERE (height>=160 AND height<=180) order by weight DESC")
erwtima1
erwtima2<-dbGetQuery(con,"SELECT date,home_team_api_id,home_team_goal FROM Match WHERE match_api_id=492473")
erwtima2
erwtima4<-dbGetQuery(con,"SELECT date,home_team_api_id,home_team_goal,away_team_api_id,away_team_goal FROM Match WHERE match_api_id=492473")
erwtima4
erwtima3<-dbGetQuery(con,"SELECT DISTINCT Team.Team_long_name,Country.name FROM Team JOIN Match JOIN Country ON (Match.Home_team_api_id=Team.Team_api_id) AND (Match.Country_id=Country.id)")
erwtima3
###Ερώτημα 5a
player_by_preferred_foot<-player_attributes %>%
group_by(preferred_foot) %>% 
summarise(n=n())
player_by_preferred_foot
barplot(player_by_preferred_foot)
class(player_by_preferred_foot)
library(ggplot2)
##Ερώτημα 5β
player_attributes %>%
  group_by(preferred_foot) %>%
  collect() %>%
  ggplot(aes(x=preferred_foot))+geom_bar()
##Ερώτημα 6α
player_by_weight_height<-player %>%
  group_by(height,weight) %>%
  collect() %>%
  ggplot(aes(x=height,y =weight))+geom_point()
player_by_weight_height
##Ερώτημα 6β
player_by_weight_height1<-player %>%
  group_by(height,weight) %>%
  collect() %>%
  ggplot(aes(x=height,y =weight))+geom_jitter(alpha=0.1)
player_by_weight_height1
match_points <- match %>%
  mutate(home_team_points = if_else((home_team_goal > away_team_goal), 3,
                                    if_else((home_team_goal == away_team_goal), 1, 0))) %>%
  mutate(away_team_points = if_else((home_team_goal > away_team_goal), 0,
                                    if_else((home_team_goal == away_team_goal), 1, 3)))
match_points
##Ερώτημα 7α
home_points<-match_points %>%
  group_by(league_id,home_team_api_id) %>%
  summarise(home_mesi_timi=mean(home_team_points, na.rm = TRUE))
home_points
##Ερώτημα 7β
away_points<-match_points %>%
  group_by(league_id,away_team_api_id) %>%
  summarise(away_mesi_timi=mean(away_team_points, na.rm = TRUE))
away_points

##Ερώτημα 7γ
enwnw<-home_points %>%
  left_join(away_points) %>%
  group_by(c1=home_team_api_id==away_team_api_id)
enwnw
enwnw1<-filter(enwnw,c1==1)
enwnw1
enwnw2<-enwnw1 %>%
  arrange(-c1)
enwnw2
###Ερώτημα 7δ
diagramma<-enwnw2 %>%
  ggplot(aes(x=home_mesi_timi,y =away_mesi_timi))+geom_point()+geom_smooth(method = lm)
diagramma
##Από το διάγραμμα βλέπουμε ότι δεν υπάρχει γραμμική σχέση μεταξύ των 2 μέσων τιμών
##γιατί τα περισσότερα σημεία έχουν μεγάλη απόσταση απο την ευθεία και βρίσκονται
##εκτός του διαστήματος εμπιστοσύνης.
dbDisconnect(con)
con
