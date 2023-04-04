
# setup -------------------------------------------------------------------

library(tidyverse)
library(here)
raw_df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/raw_anime.csv")

anime <- raw_df %>% 
  # Producers
  mutate(producers = str_remove(producers, "\\["),
         producers = str_remove(producers, "\\]")) %>% 
  separate_rows(producers, sep = ",") %>% 
  mutate(producers = str_remove(producers, "\\'"),
         producers = str_remove(producers, "\\'"),
         producers = str_trim(producers)) %>% 
  # Genre
  mutate(genre = str_remove(genre, "\\["),
         genre = str_remove(genre, "\\]")) %>% 
  separate_rows(genre, sep = ",") %>% 
  mutate(genre = str_remove(genre, "\\'"),
         genre = str_remove(genre, "\\'"),
         genre = str_trim(genre)) %>% 
  # Studio
  mutate(studio = str_remove(studio, "\\["),
         studio = str_remove(studio, "\\]")) %>% 
  separate_rows(studio, sep = ",") %>% 
  mutate(studio = str_remove(studio, "\\'"),
         studio = str_remove(studio, "\\'"),
         studio = str_trim(studio)) %>% 
  # Aired
  mutate(aired = str_remove(aired, "\\{"),
         aired = str_remove(aired, "\\}"),
         aired = str_remove(aired, "'from': "),
         aired = str_remove(aired, "'to': "),
         aired = word(aired, start = 1, 2, sep = ",")) %>% 
  separate(aired, into = c("start_date", "end_date"), sep = ",") %>% 
  mutate(start_date = str_remove_all(start_date, "'"),
         start_date = str_sub(start_date, 1, 10),
         end_date = str_remove_all(start_date, "'"),
         end_date = str_sub(end_date, 1, 10)) %>%
  mutate(start_date = lubridate::ymd(start_date),
         end_date = lubridate::ymd(end_date)) %>% 
  filter(rank != 0,
         popularity != 0)

write_csv(anime,"anime.csv")



# Start -------------------------------------------------------------------


file<-here("data","anime.csv")
anime<-read_csv(file)

ggplot(anime,aes(x=score))+
  geom_histogram(bins=30,color="red",fill="darkblue")+
  labs(x="Score",y="Count",title="Distribution of Anime Ratings")+
  theme_classic()
mean(anime$score,na.rm=TRUE)#6.872

t.test(anime$score)#6.865-6.879 95% confident interval that the true mean rating of anime is  
# Genre -------------------------------------------------------------------

A
#MAYBE reference the data having duplication genres because fore each studio and developer 
genre<-anime %>% 
  group_by(genre) %>% 
  summarise(mean=mean(score,na.rm = TRUE),
            sd=sd(score,na.rm = TRUE),
            count=n()) %>%
  mutate(rank_mean=min_rank(-mean),
         rank_count=min_rank(-count))

scored_well<-genre %>% 
  arrange(-mean) %>% 
  filter(rank_mean<=5)

most_used<-genre %>% 
  arrange(-count) %>% 
  filter(rank_count<=5)

#Plots
 # Top 10 well scoerd genres usage 
ggplot(scored_well,aes(x=fct_reorder(genre,-count),y=count,fill=genre))+
  geom_col()+
  ylim(0,10000)+
  labs(x="Genre",y="Count",title="Top 5 Well-Scored Genres")+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  scale_fill_discrete(name="Genre")+
  geom_text(aes(label=count),vjust=-.4)+
  geom_text(aes(label=signif(mean,digits = 3)),vjust=1)+
  theme_minimal()



 #Most used genres in the anime industry 
ggplot(most_used,aes(x=fct_reorder(genre,-count),y=count,fill=genre))+
  geom_col()+
  labs(x="Genre",y="Count",title="Top 5 Most common used Genre")+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  scale_fill_discrete(name="Genre")+
  geom_text(aes(label=count),vjust=-1)+
  geom_text(aes(label=signif(mean,digits = 3)),vjust=1)+
  theme_minimal()

#USE THIS AS THE R MARK DOWN DOCUMANTATION !!!!!!!
# reevalutation of genre counts -------------------------------------------
#The new start to clean ( Since there is a lot of ducplciate and this set of code removes the duplicates)
genre_list<-anime %>% 
  group_by(genre,name) %>% 
  summarise(mean=mean(score,na.rm = TRUE)) %>%
  mutate(rank_mean=min_rank(-mean))

genre_count<-genre_list %>% 
  count() %>%
  ungroup() %>%
  mutate(Usage=min_rank(-n)) %>% 
  arrange(Usage)

genre_mean<-genre_list %>%
  ungroup() %>% 
  group_by(genre) %>% 
  summarise(score=mean(mean,na.rm=TRUE)) %>% 
  mutate(Score=min_rank(-score)) %>% 
  arrange(Score)

genre_anime<-genre_mean %>% 
  left_join(genre_count,by="genre")

well_recieved_genre<-genre_anime %>% 
  filter(Score<=5)

most_used_genre<-genre_anime %>% 
  filter(Usage<=5) %>% 
  arrange(Usage)

ggplot(most_used_genre,aes(x=fct_reorder(genre,-n),y=n,fill=genre))+
  geom_col()+
  labs(x="Genre",y="Count",title="Top 5 Most common used Genre")+
  ylim(0,5500)+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  scale_fill_discrete(name="Genre")+
  geom_text(aes(label=n),vjust=-1)+
  geom_text(aes(label=signif(score,digits = 3)),vjust=1)+
  theme_minimal()

ggplot(well_recieved_genre,aes(x=fct_reorder(genre,-score),y=n,fill=genre))+
  geom_col()+
  labs(x="Genre",y="Count",title="Top 5 Well-Scored Genres")+
  ylim(0,5500)+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  scale_fill_discrete(name="Genre")+
  geom_text(aes(label=n,vjust=-.4))+
  geom_text(aes(label=signif(score,digits = 3),vjust=1),vjust=1.5)+
  theme_minimal()

# Looking deeper (Most used genre v the highest average) -----------------
#This is fine for the project !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

comedy_list<- genre_list %>% 
  filter(genre=="Comedy")
t.test(comedy_list$mean) # 6.48-6.54 95% conf interval

comedy_list_sm<- genre_list %>% 
  filter(genre=="Comedy",
         rank_mean<=5) %>% 
  arrange(rank_mean)



mystery_list<- genre_list %>% 
  filter(genre=="Mystery")
t.test(mystery_list$mean) #7.06-7.20 95% conf interval 

mystery_list_sm<- genre_list %>% 
  filter(genre=="Mystery",
         rank_mean<=5) %>% 
  arrange(rank_mean)

# plots
ggplot(mystery_list,aes(x=mean))+
  geom_histogram(bins=30,color="red",fill="darkblue")+
  labs(x="Score",y="Count",title="Distribution of Thiller Score")+
  theme_classic()
#Anime Thiller
#1st:Steins;Gate
#2nd: Monster
#3rd: Death Note
#4th: Kara no kyoukai 5: Mujun Rasen
#5th: Steins;Gate 0

ggplot(comedy_list,aes(x=mean,color=mean))+
  geom_histogram(bins=30,color="red",fill="darkblue")+
  labs(x="Score",y="Count",title="Distribution of Comedy Anime Score")+
  theme_classic()
#Comedy:
 #1st: Violence Voyager
#2nd:Tatakae! Dokan-kun: Robolympic-he
#3rd: Fullmetal Alchemist:Brotherhood
#4th: Gintama*
#5th: Gintama'

#Statistical testing;

two_genre<-genre_list %>% 
  filter(genre==c("Mystery","Comedy"))
#Null hypothesis: Mean scores difference of the Thriller and Comedy isn't different
#Alternative hypothesis: The difference of mean scores are different 
#a=0.05
t.test(mean~genre,data=two_genre)
#p-value=0.02909 

#Because of the pvalue being less than 0.05, there is statistical significance that the 
#mean difference of scores between Thiller and Comedy are not the same 