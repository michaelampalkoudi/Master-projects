# dslb 1.1.1 - Dataset health status

# Loading funModeling!
library(funModeling)
library(dplyr)
library(Hmisc)

spot_data <- read.csv("C:/Users/mixmp/Desktop/διερευνητικη ανάλυση/lecture 5/hw4/spotify_data_by_year.csv")
print(spot_data)
View(spot_data)
glimpse(spot_data)

# Profiling the Data Input
my_data_status=df_status(spot_data, print_results = F)

# Removing variables with 60% of zero values
vars_to_remove=filter(my_data_status, p_zeros > 60)  %>% .$variable
vars_to_remove

# Keeping all columns except the ones present in 'vars_to_remove' vector
spot_data_2=select(spot_data, -one_of(vars_to_remove))
df_status(spot_data_2)

#Ordering data by percentage of zeros
arrange(my_data_status, -p_zeros) %>% select(variable, q_zeros, p_zeros)

# other common statistics
# Total rows
nrow(spot_data)

# Total columns
ncol(spot_data)

# Column names
colnames(spot_data)




# dslb 1.1.3 Profiling numerical variables
library(Hmisc)

library(tidyverse)
view(spot_data)
str(spot_data)

# Using the `describe` on a complete dataset. # It can be run with one variable; for example, describe(data_subset$poverty_headcount_1.9)
describe(spot_data)

library(funModeling)

# Full numerical profiling in one function automatically
# excludes non-numerical variables
# Important: All the metrics are calculated having removed 
# the NA values. Otherwise, the table would be filled with NA's.
profiling_num(spot_data)

# select the most relevant metrics
my_profiling_table=profiling_num(spot_data) %>% select(variable, mean, p_01, p_99, range_80)
# Printing only the first three rows
head(my_profiling_table,n=12)

# Profiling numerical variables by plotting
plot_num(spot_data)
plot_num(spot_data, bins = 20)




# edav.info PART II 
# Continuous Variables

# Chart: Histogram

library(ggplot2) # plotting

num_var<-names(spot_data)[2:12]

 
for (i in num_var)
{
# spot_data histograms by year with overlayed density curves
 x1<-as.name(num_var[1])
 spot_data
 ggplot(spot_data, aes(x = acousticness , y = ..density..)) + 
  # plotting
  geom_histogram(bins = 20, colour = "#80593D", fill = "#9FC29F", boundary = 0) +
  geom_density(color = "#3D6480") + 
  
  # formatting
  ggtitle("Histogram per year",
          subtitle = "Beak Depth Density of Galapagos Finches by Year") +
  labs(x = "acousticness") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))
}

# Simple examples
# store data
x <- c(50, 51, 53, 55, 56, 60, 65, 65, 68)

### Histogram using base R
# plot data
hist(x, col = "lightblue", main = "Base R Histogram of x")
hist(x)

### Histogram using ggplot2
# import ggplot
library(ggplot2)
# must store data as dataframe

df <- data.frame(x)

# plot data
ggplot(df, aes(x)) +
  geom_histogram(color = "grey", fill = "lightBlue", 
                 binwidth = 5, center = 52.5) +
  ggtitle("ggplot2 histogram of x")

# format layout
op <- par(mfrow = c(1, 2), las = 1)

# right closed
hist(x, col = "lightblue", ylim = c(0, 4),
     xlab = "right closed ex. (55, 60]", font.lab = 2)
# right open
hist(x, col = "lightblue", right = FALSE, ylim = c(0, 4),
     xlab = "right open ex. [55, 60)", font.lab = 2)

# default...note the pop-up about default bin number
ggplot(finches, aes(x = Depth)) +
  geom_histogram() +
  ggtitle("Default with pop-up about bin number")

# using binwidth
p1 <- ggplot(finches, aes(x = Depth)) +
  geom_histogram(binwidth = 0.5, boundary = 6) +
  ggtitle("Changed binwidth value")

# using bins
p2 <- ggplot(finches, aes(x = Depth)) +
  geom_histogram(bins = 48, boundary = 6) +
  ggtitle("Changed bins value")

# format plot layout
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)



### Bin alignment
df <- data.frame(x)

# default alignment
ggplot(df, aes(x)) +
  geom_histogram(binwidth = 5,
                 fill = "lightBlue", col = "black") +
  ggtitle("Default Bin Alignment")

# specify alignment with boundary
p3 <- ggplot(df, aes(x)) +
  geom_histogram(binwidth = 5, boundary = 60,
                 fill = "lightBlue", col = "black") +
  ggtitle("Bin Alignment Using boundary")

# specify alignment with center
p4 <- ggplot(df, aes(x)) +
  geom_histogram(binwidth = 5, center = 67.5,
                 fill = "lightBlue", col = "black") +
  ggtitle("Bin Alignment Using center")

# format layout
library(gridExtra)
grid.arrange(p3, p4, ncol = 2)

## Interactive histograms with `ggvis`

library(tidyverse)
library(ggvis)
faithful
faithful %>% ggvis(~eruptions) %>% 
  layer_histograms(fill := "lightblue", 
                   width = input_slider(0.1, 2, value = .1, 
                                        step = .1, label = "width"))

### GDP example
df <-read.csv("countries2012.csv")
df %>% ggvis(~GDP) %>% 
  layer_histograms(fill := "green", 
                   width = input_slider(500, 10000, value = 5000, 
                                        step = 500, label = "width"))

### Change center interactively 
df <- data.frame(x = c(50, 51, 53, 55, 56, 60, 65, 65, 68))
df %>% ggvis(~x) %>% 
  layer_histograms(fill := "red", 
                   width = input_slider(1, 10, value = 5, step = 1, label = "width"),
                   center = input_slider(50, 55, value = 52.5, step = .5, label = "center"))

### Change center (with data values shown)
df <- data.frame(x = c(50, 51, 53, 55, 56, 60, 65, 65, 68), 
                 y = c(.5, .5, .5, .5, .5, .5, .5, 1.5, .5))
df %>% ggvis(~x, ~y) %>% 
  layer_histograms(fill := "lightcyan", width = 5,
                   center = input_slider(45, 55, value = 45, 
                                         step = 1, label = "center")) %>% 
  layer_points(fill := "blue", size := 200) %>% 
  add_axis("x", properties = axis_props(labels = list(fontSize = 20))) %>% 
  scale_numeric("x", domain = c(46, 72)) %>% 
  add_axis("y", values = 0:3, 
           properties = axis_props(labels = list(fontSize = 20)))

### Change boundary interactively
df %>% ggvis(~x) %>% 
  layer_histograms(fill := "red", 
                   width = input_slider(1, 10, value = 5, 
                                        step = 1, label = "width"),
                   boundary = input_slider(47.5, 50, value = 50,
                                           step = .5, label = "boundary"))



# Chart: Boxplot {#box}
knitr::opts_chunk$set(fig.align = 'center', out.width = '60%')
library(ggplot2)

# boxplot by feed supplement 
ggplot(chickwts, aes(x = reorder(feed, -weight, median), y = weight)) + 
  # plotting
  geom_boxplot(fill = "#cc9a38", color = "#473e2c") + 
  # formatting
  ggtitle("Casein Makes You Fat?!",
          subtitle = "Boxplots of Chick Weights by Feed Supplement") +
  labs(x = "Feed Supplement", y = "Chick Weight (g)", caption = "Source: datasets::chickwts") +
  theme_grey(16) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))

## Simple examples
### Single boxplots
# vector
boxplot(rivers) 

# single column of a data frame
boxplot(chickwts$weight, horizontal = TRUE) 

ggplot(chickwts, aes(weight)) + geom_boxplot()

ggplot(chickwts, aes(y = weight)) + 
  geom_boxplot() +
  theme_grey(16) # make all font sizes larger (default is 11)

ggplot(chickwts, aes(x = "all 71 chickens", y = weight)) + 
  geom_boxplot() + xlab("") + theme_grey(16)

### Multiple boxplots using ggplot2
str(chickwts)
ggplot(chickwts, aes(x = reorder(feed, -weight, median), y = weight)) +
  geom_boxplot() +
  xlab("feed type") +
  theme_grey(16)

library(tidyverse)
head(attitude)
tidyattitude <- attitude %>% gather(key = "question", value = "rating")
head(tidyattitude)

ggplot(tidyattitude, aes(reorder(question, -rating, median), rating)) + 
  geom_boxplot() +
  xlab("question short name") +
  theme_grey(16)


ggplot(tidyattitude, aes(reorder(question, rating, median), rating)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("question short name") +
  theme_grey(16)

ggplot(tidyattitude, aes(rating, reorder(question, rating, median))) + 
  geom_boxplot() +
  ggtitle("This is not what we wanted!") +
  ylab("question short name") +
  theme_grey(16)

library(likert) # data
library(dplyr) # data manipulation

# load/format data
data(pisaitems)
pisa <- pisaitems[1:100, 2:7] %>% 
  dplyr::mutate_all(as.integer) %>% 
  dplyr::filter(complete.cases(.))

head(pisa, 4)
# create theme
theme <- theme_grey(16) +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))

# create plot
plot <- ggplot(stack(pisa), aes(x = ind, y = values)) +
  geom_boxplot(fill = "#9B3535") +
  ggtitle("Don't Plot Boxplots of Categorical Variables",
          subtitle = "...seriously don't. Here, I'll make it red so it looks scary:") +
  labs(x = "Assessment Code", y = "Values", caption = "Source: likert::pisaitems")

# bad boxplot
plot + theme




# Chart: Violin Plot {#violin}

## Some Examples in R

# import ggplot and the Datasets Package
library(datasets)
library(ggplot2)

supps <- c("horsebean", "linseed", "soybean", "meatmeal", "sunflower", "casein")

# plot data

ggplot(chickwts, aes(x = factor(feed, levels = supps), 
                     y = weight)) + 
  # plotting
  geom_violin(fill = "lightBlue", color = "#473e2c") + 
  labs(x = "Feed Supplement", y = "Chick Weight (g)")

# import ggplot and the Datasets Package
library(datasets)
library(ggplot2)

supps <- c("horsebean", "linseed", "soybean", "meatmeal", "sunflower", "casein")

# plot data



## Adding Statistics to the Violin Plot 

### Adding the median and the interquartile range
ggplot(chickwts, aes(x = factor(feed, levels = supps), 
                     y = weight)) + 
  # plotting
  geom_violin(fill = "lightBlue", color = "#473e2c") + 
  labs(x = "Feed Supplement", y = "Chick Weight (g)") + 
  geom_boxplot(width=0.1)


### Displaying data as dots


# Chart: Ridgeline Plots {#ridgeline}

knitr::opts_chunk$set(echo = TRUE)
library("ggridges")
library("tidyverse")
library("cluster")

library("ggridges")
library("tidyverse")
Theoph_data <- Theoph
ggplot(Theoph_data, aes(x=Dose,y=Subject,fill=Subject))+
  geom_density_ridges_gradient(scale = 4, show.legend = FALSE) + theme_ridges() +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_x_continuous(expand = c(0.01, 0)) +
  labs(x = "Dose of theophylline(mg/kg)",y = "Subject #") +
  ggtitle("Density estimation of dosage given to various subjects") +
  theme(plot.title = element_text(hjust = 0.5))

## Simple examples

library("datasets")
head(Orange, n=5)

## Ridgeline Plots using ggridge

library("ggridges")
library("tidyverse")
ggplot(Orange, aes(x=circumference,y=Tree,fill = Tree))+
  geom_density_ridges(scale = 2, alpha=0.5) + theme_ridges()+
  scale_fill_brewer(palette = 4)+
  scale_y_discrete(expand = c(0.8, 0)) +
  scale_x_continuous(expand = c(0.01, 0))+
  labs(x="Circumference at Breast Height", y="Tree with ordering of max diameter")+
  ggtitle("Density estimation of circumference of different types of Trees")+
  theme(plot.title = element_text(hjust = 0.5))

library("ggridges")
library("tidyverse")
OrchardSprays_data <- OrchardSprays
ggplot(OrchardSprays_data, aes(x=decrease,y=treatment,fill=treatment))+
  geom_density_ridges_gradient(scale=3) + theme_ridges()+
  scale_y_discrete(expand = c(0.3, 0)) +
  scale_x_continuous(expand = c(0.01, 0))+
  labs(x="Response in repelling honeybees",y="Treatment")+
  ggtitle("Density estimation of response by honeybees to a treatment for scale=3")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(OrchardSprays_data, aes(x=decrease,y=treatment,fill=treatment))+
  geom_density_ridges_gradient(scale=5) + theme_ridges()+
  scale_y_discrete(expand = c(0.3, 0)) +
  scale_x_continuous(expand = c(0.01, 0))+
  labs(x="Response in repelling honeybees",y="Treatment")+
  ggtitle("Density estimation of response by honeybees to a treatment for scale=5")+
  theme(plot.title = element_text(hjust = 0.5))

library("ggridges")
library("tidyverse")
ggplot(InsectSprays, aes(x = count, y = spray, height = ..density.., fill = spray)) + 
  geom_density_ridges(stat = "binline", bins = 20, scale = 0.7, draw_baseline = FALSE)


library("ggridges")
library("tidyverse")
ggplot(InsectSprays, aes(x=count,y=spray,fill=spray))+
  geom_density_ridges_gradient() + theme_ridges()+
  labs(x="Count of Insects",y="Types of Spray")+
  ggtitle("The counts of insects treated with different insecticides.")+
  theme(plot.title = element_text(hjust = 0.5))



# Chart: QQ-Plot {#qqplot}
knitr::opts_chunk$set(echo = TRUE,message = FALSE,
                      warning = FALSE)


x <- rnorm(1000, 50, 10)
qqnorm(x)
qqline(x, col = "red")

### Non-normal qqplot
x <- rexp(1000, 5)
qqnorm(x)
qqline(x, col = "red")

## Different kinds of qqplots

## qqplot using ggplot
library(ggplot2)
x <- rnorm(1000, 50, 10)
x <- data.frame(x)
ggplot(x, aes(sample = x)) +
  stat_qq() +
  stat_qq_line()

library(ggplot2)
ggplot(mtcars, aes(sample = mpg, colour = factor(cyl))) +
  stat_qq() +
  stat_qq_line()



# (PART) Categorical Variables {-}

# Chart: Bar Chart {#bar}

library(datasets) # data
library(ggplot2) # plotting
library(dplyr) # manipulation

ship_grouped <- as.data.frame(Titanic) %>%
  group_by(Class, Sex, Survived) %>%
  summarise(Total = sum(Freq))

ggplot(ship_grouped, aes(x = Survived, y = Total, fill = Sex)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = Total), position = position_dodge(width = 0.9), 
            vjust = -0.4, color = "grey68") +
  facet_wrap(~Class) +
  # formatting
  ylim(0, 750) +
  ggtitle("Don't Be A Crew Member On The Titanic",
          subtitle = "Survival Rates of Titanic Passengers by Class and Gender") +
  scale_fill_manual(values = c("#b2df8a", "#a6cee3")) +
  labs(y = "Passenger Count", caption = "Source: titanic::titanic_train") +
  theme(plot.title = element_text(face = "bold")) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35")) +
  theme(plot.caption = element_text(color = "grey68"))

## Simple examples
colors <- as.data.frame(HairEyeColor)

# just female hair color, using dplyr
colors_female_hair <- colors %>%
  filter(Sex == "Female") %>%
  group_by(Hair) %>%
  summarise(Total = sum(Freq))

# take a look at data
head(colors_female_hair)

### Bar graph using base R
barplot(colors_female_hair[["Total"]], 
        names.arg = colors_female_hair[["Hair"]],
        main = "Bar Graph Using Base R")

### Bar graph using ggplot2
library(ggplot2) # plotting

ggplot(colors_female_hair, aes(x = Hair, y = Total)) +
  geom_bar(stat = "identity") +
  ggtitle("Bar Graph Using ggplot2")

### Flip Bars
ggplot(colors_female_hair, aes(x = Hair, y = Total)) +
  geom_bar(stat = "identity") +
  ggtitle("Bar Graph Using ggplot2") +
  coord_flip()

### Facet Wrap
ggplot(colors, aes(x = Sex, y = Freq)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Hair)




# Chart: Cleveland Dot Plot {#cleveland}
library(tidyverse)
# create a theme for dot plots, which can be reused
theme_dotplot <- theme_bw(14) +
  theme(axis.text.y = element_text(size = rel(.75)),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = rel(.75)),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.x = element_blank())

# move row names to a dataframe column        
df <-  swiss %>% tibble::rownames_to_column("Province")

# create the plot
ggplot(df, aes(x = Fertility, y = reorder(Province, Fertility))) +
  geom_point(color = "blue") +
  scale_x_continuous(limits = c(35, 95),
                     breaks = seq(40, 90, 10)) +
  theme_dotplot +
  xlab("\nannual live births per 1,000 women aged 15-44") +
  ylab("French-speaking provinces\n") +
  ggtitle("Standardized Fertility Measure\nSwitzerland, 1888")


## Multiple dots

df <- read_csv("data/SAT2010.csv", na = "s")

set.seed(5293)
tidydf <- df %>%
  filter(!is.na(`Critical Reading Mean`)) %>%
  sample_n(20) %>%
  rename(Reading = "Critical Reading Mean", Math = "Mathematics Mean",
         Writing = "Writing Mean") %>%
  gather(key = "Test", value = "Mean", "Reading", "Math", "Writing")

ggplot(tidydf, aes(Mean, `School Name`, color = Test)) +
  geom_point() +
  ggtitle("Schools are sorted alphabetically", sub = "not the best option") + ylab("") +
  theme_dotplot

ggplot(tidydf, 
       aes(Mean, fct_reorder2(`School Name`, Test=="Reading", Mean, .desc = FALSE),
           color = Test)) +
  geom_point() + ggtitle("Schools sorted by Reading mean") + ylab("") +
  theme_dotplot 

ggplot(tidydf, 
       aes(Mean, fct_reorder2(`School Name`, Test, Mean, .desc = FALSE),
           color = Test)) +
  geom_point() + ggtitle("Schools sorted by Writing mean") + ylab("") +
  theme_dotplot


ggplot(tidydf, 
       aes(Mean, fct_reorder2(`School Name`, Test, Mean, .fun = first2, .desc = FALSE),
           color = Test)) +
  geom_point() + ggtitle("Schools sorted by Math mean") + ylab("") +
  theme_dotplot

