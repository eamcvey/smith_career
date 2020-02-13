
# Setup -------------------------------------------------------------------

library(tidyverse)
library(babynames)
library(dtwclust)


# Get data ----------------------------------------------------------------

data(babynames)


# Look at top names by year -----------------------------------------------

babynames %>%
  filter(year %in% c(1880, 1979, 2008, 2015)) %>%
  group_by(year) %>%
  arrange(desc(prop)) %>%
  mutate(rank = 1:n()) %>%
  slice(1:10) %>%
  ungroup() %>%
  ggplot(aes(x = reorder_within(name, prop, year), y = 100 * prop)) +
  geom_col(aes(fill = sex)) +
  theme_bw() +
  coord_flip() + 
  scale_x_reordered() +
  facet_wrap( ~ year, scales = 'free_y') +
  scale_fill_discrete('') +
  labs(y = '% of babies by gender',
       x = '',
       title = 'Top 10 baby names by year',
       subtitle = 'From the US Social Security Administration')



# Variety of names by year and gender -------------------------------------

babynames %>%
  filter(year %in% c('1880', '2008')) %>%
  group_by(year, sex) %>%
  arrange(year, sex, desc(prop)) %>%
  mutate(cume_prop = cumsum(prop)) %>%
  mutate(rank = 1:n()) %>%
  ungroup() %>%
  filter(cume_prop <= .5) %>% 
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = 100 * cume_prop, y = rank, color = sex, linetype = year)) +
  geom_line(size = 1.5) +
  theme_bw() +
  labs(x = '% of babies (by gender)',
       y = '# of names',
       title = 'Variety of names by year and gender') 
  

# Individual names over time ----------------------------------------------

babynames %>%
  filter(name %in% c('John', 'Jason', 'Elaine', 'Ava')) %>%
  group_by(year, name) %>%
  summarize(pct = 100 * sum(prop)) %>%
  ggplot(aes(x = year, y = pct, color = name)) +
  geom_line(size = 1.5) +
  theme_bw() +
  labs(x = '',
       y = '% of babies',
       title = 'Name popularity over time')

babynames %>%
  filter(name %in% c('John', 'Jason', 'Elaine', 'Ava')) %>%
  group_by(year, name) %>%
  summarize(pct = 100 * sum(prop)) %>%
  ggplot(aes(x = year, y = pct, color = name)) +
  geom_line(size = 1.5) +
  theme_bw() +
  labs(x = '',
       y = '% of babies',
       title = 'Name popularity over time') +
  facet_wrap( ~ name, scales = 'free_y') 

# Total occurrence of most popular names in select years
babynames %>%
  filter((name == 'John' & year == 1880 & sex == 'M') |
           (name == 'Mary' & year == 1880 & sex == 'F') |
           (name == 'Jacob' & year == 2008 & sex == 'M') |
           (name == 'Emma' & year == 2008 & sex == 'F')) %>%
  mutate(year = as.factor(year))  %>%
  ggplot(aes(x = reorder(name, n), y = n, fill = year)) +
  geom_col() +
  theme_bw() +
  labs(x = '', 
       y = '# of babies',
       title = 'Number of babies for most popular name')


# Most gender neutral names -----------------------------------------------


# Get reasonably common names
total_count <- babynames %>%
  group_by(name, sex) %>%
  summarize(total_n = sum(n))

quantile(total_count$total_n, c(.25, .5, .75))

top_names <- total_count %>%
  filter(total_n > 5000) %>%
  select(name, sex)

# Per name and year, calculate gender proportion
gender_mix <- babynames %>%
  inner_join(top_names) %>%
  select(-n) %>%
  pivot_wider(names_from = sex, values_from = prop) %>%
  rename(Female = F,
         Male = M) %>%
  replace_na(list(Female = 0, Male = 0)) %>%
  mutate(pct_female = 100 * Female / (Female + Male))
  
# Average pct_female across years
gender_mix_sum <- gender_mix %>%
  group_by(name) %>%
  summarize(avg_pct_female = mean(pct_female))

gender_mix_sum %>%
  ggplot(aes(x = avg_pct_female)) +
  geom_histogram(fill = 'hotpink') +
  theme_bw() +
  labs(x = '% Females',
       y = '# Names',
       title = 'Distribution of gender mix per name')

gender_neutral <- gender_mix_sum %>%
  filter(avg_pct_female >= 40 & avg_pct_female <= 60) %>%
  pull(name)

# Cluster baby names time series ------------------------------------------

top_names2 <- total_count %>%
  filter(total_n > 100000) %>%
  select(name, sex)

# 
# temp <- babynames %>%
#   inner_join(top_names2) %>%
#   select(-n) %>%
#   filter(sex == 'F') %>%
#   select(year, name, prop) %>%
#   split(.$name)

name_series <- babynames %>%
  inner_join(top_names2) %>%
  filter(sex == 'F') %>%
  select(year, name, prop) %>%
  pivot_wider(names_from = name, values_from = prop, values_fill = list(prop = 0)) %>%
  select(-year)

# hc_sbd <- tsclust(name_series, type = "h", k = 20L,
#                   preproc = zscore, seed = 899,
#                   distance = "sbd", centroid = shape_extraction,
#                   control = hierarchical_control(method = "average"))  
# 
# plot(hc_sbd)
# plot(hc_sbd, type = "sc")

babywideF <- babynames %>% 
  inner_join(top_names2) %>%
  filter(sex=="F") %>% 
  select(name, year, n) %>%
  spread(year, n, fill=0)

rownames(babywideF)<- babywideF %>% .$name  #set rownames
babywideF <- babywideF %>% select(-name) # remove name var.

### principal components analysis - females
resF.pca <- princomp(babywideF)
plot(resF.pca)

###k-means clustering analysis
set.seed(100)
resF.k <- kmeans(babywideF, 16)
table(resF.k$cluster)
  
names(resF.k$cluster[resF.k$cluster==1])

#no
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==1])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 1'
  ) +
  ylim(c(0, 100000))

#maybe
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==2])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 2'
  )  +
  ylim(c(0, 100000))

#maybe
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==3])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 3'
  )  +
  ylim(c(0, 100000))

# no
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==4])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 4'
  )  +
  ylim(c(0, 100000))

#yes
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==5])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 5'
  )  +
  ylim(c(0, 100000))

#maybe - similar to 5
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==6])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 6'
  )  +
  ylim(c(0, 100000))

#yes
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==7])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 7'
  )  +
  ylim(c(0, 100000))

#maybe - resurgence? low peak in 20s
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==8])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 8'
  )  +
  ylim(c(0, 100000))

#yes
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==9])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 9'
  )  +
  ylim(c(0, 100000))

#maybe - less popular 20s peak
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==10])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 10'
  )  +
  ylim(c(0, 100000))

#m yes 
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==11])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 11'
  )  +
  ylim(c(0, 100000))

#yes
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==12])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 12'
  )  +
  ylim(c(0, 100000))

#no
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==13])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 13'
  )  +
  ylim(c(0, 100000))

#yes
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==14])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 14'
  )  +
  ylim(c(0, 100000))

#yes - just Jennifer!
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==15])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 15'
  )  +
  ylim(c(0, 100000))

#no - too many
babynames %>%
  filter(sex == 'F') %>%
  filter(name %in% names(resF.k$cluster[resF.k$cluster==16])) %>%
  ggplot(aes(x = year, y = n, color = name)) +
  geom_line() +
  theme_bw() +
  scale_color_discrete('') +
  labs(
    x = '',
    y = '# of Babies',
    title = 'Names Cluster 16'
  )  +
  ylim(c(0, 100000))

