library(network)
library(dplyr)
library(csv)
library(tidyverse)
greece<-read_csv("C:/Users/mixmp/Desktop/greece.csv")

str(greece)
library(igraph)
from <- greece %>%
  distinct(from)  %>%
  rename(label = from)
dim(from)

to <- greece %>%
  distinct(to) %>%
  rename(label = to)
dim(to)
nodes <- full_join(from, to, by = "label")
nodes<-as.data.frame(nodes)
nodes <- nodes %>% rowid_to_column("id")
nodes
connections <- data.frame(from = as.vector(greece$from), to = as.vector(greece$to),label=greece$distance,font.color=c("red"),font.size=20)

############################################################################################3
library(network)


routes_network <- network(connections, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
class(routes_network)
str(routes_network)
plot(routes_network)
summary(routes_network)
################################################################################################

library(visNetwork)
visNetwork(nodes, connections)%>%  
  visNodes(font = list(size = 25) ,
           scaling = list(label = list(
             enabled = TRUE,
             min = 25, max = 100,
             maxVisible = 100,
             drawThreshold = 1
           )))
##################################################################################################
library(tidyverse)
library(visNetwork)
library(dplyr)
df <- read_csv("C:/Users/mixmp/Desktop/‰ÈÂÒÂıÌÁÙÈÍÁ ·Ì‹ÎıÛÁ/≈—√¡”…≈”/HW5/greece.csv")
from <- df %>%
  distinct(from)  %>%
  rename(label = from)

to <- df %>%
  distinct(to) %>%
  rename(label = to)


nodes <- data.frame(id =as.vector(full_join(from, to, by = "label")))
colnames(nodes)="id"
nodes <- nodes %>% mutate(label = id)
connections <- data.frame(from = df$from, to = df$to)
visNetwork(nodes, connections)  %>%
  visNodes(font = list(size = 25) ,
           scaling = list(label = list(
             enabled = TRUE,
             min = 25, max = 100,
             maxVisible = 100,
             drawThreshold = 1
           )))
###############################################################################################3



##################################20 erwthma
library(tidygraph)

graph_routes <- as_tbl_graph(greece)

graph_routes


library(stringr)

graph_routes <- graph_routes %>%
  activate(nodes) %>%
  mutate(
    title = str_to_title(name),
    label = str_replace_all(title, " ", "\n")
  )

stations <- graph_routes %>%
  activate(nodes) %>%
  pull(title)

stations

library(ggplot2)

thm <- theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),
  ) 

theme_set(thm)


library(ggraph) 

graph_routes %>%
  ggraph(layout = "kk") +
  geom_node_point() +
  geom_edge_diagonal() 

graph_routes %>%
  ggraph(layout = "kk") +
  geom_node_text(aes(label = label, color = name), size = 3) +
  geom_edge_diagonal(color = "gray", alpha = 0.4) 


graph_routes %>% 
  activate(edges) %>% 
  arrange(desc(distance))

#if I wanted to rearrange the rows in the edges tibble to list those with the highest ‚Äúweight‚Äù first,
#I could use activate() and then arrange(). Here I simply print out the result rather than saving it.



ggraph(graph_routes, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width =distance), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

#To get the width of the line to change according to the weight variable, we place the argument within an aes() function. 
#In order to control the maximum and minimum width of the edges, I use scale_edge_width() and set a range. 


ggraph(graph_routes, layout = "stress") +
  geom_edge_link(aes(edge_alpha = distance), width = 2) +
  geom_node_point(colour = "blue", size = 4)+
  geom_node_label(aes(label = name), nudge_y = 0.1, nudge_x = -0.1) +
  theme_graph() +
  labs(edge_alpha = "distance")



graph_routes<-graph_routes %>% 
  activate(nodes) %>% 
  mutate(degree = centrality_degree())

ggraph(graph_routes, layout = "stress") +
  geom_edge_link(aes(edge_alpha = distance), width = 2) +
  geom_node_point(aes(size = degree), colour = "blue") +
  geom_node_label(aes(label = name), nudge_y = 0.1, nudge_x = -0.1) +
  scale_size(breaks = seq(3, 5, 1)) +
  theme_graph() +
  labs(edge_alpha = "distance", size = "Connections")


###############erwthma3#################################

from <- which(stations == "ŒàŒ≤œÅŒøœÇ")
to <-  which(stations == "ŒúŒµœÉœÉŒ∑ŒΩŒØŒ±")
str(graph_routes)

shortest <- graph_routes %>%
  morph(to_shortest_path,from ,to)

shortest$selected_node
shortest %>%
  mutate(selected_node = TRUE) %>%
  unmorph()
#While it was morphed, only the few nodes that make up the connections 
#between the Arras and Nancy stations were selected. A simple mutate() 
#adds a new variable called selected_node, which tags those nodes with 
#TRUE. 
#The new variable and value is retained once the rest of the nodes are 
#restored via the unmorph() command.

#The next step is to coerce each NA into a 1, and the shortest route into a 2. This will allow us 
#to easily re-arrange the order that the edges are drawn in the plot, ensuring that the route will be 
#drawn at the top.



shortest <- shortest %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph() 


shortest <- shortest %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

shortest

shortest %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = label, color =name, alpha = selected_node ), size = 3) 

##################################################################
from <- which(stations == "ŒàŒ≤œÅŒøœÇ")
to <-  which(stations == "ŒúŒµœÉœÉŒ∑ŒΩŒØŒ±")
str(graph_routes)

shortest <- graph_routes %>%
  morph(to_shortest_path,from ,to , weights = distance)

shortest$selected_node
shortest %>%
  mutate(selected_node = TRUE) %>%
  unmorph()
#While it was morphed, only the few nodes that make up the connections 
#between the Arras and Nancy stations were selected. A simple mutate() 
#adds a new variable called selected_node, which tags those nodes with 
#TRUE. 
#The new variable and value is retained once the rest of the nodes are 
#restored via the unmorph() command.

#The next step is to coerce each NA into a 1, and the shortest route into a 2. This will allow us 
#to easily re-arrange the order that the edges are drawn in the plot, ensuring that the route will be 
#drawn at the top.



shortest <- shortest %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph() 


shortest <- shortest %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

shortest

shortest %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = label, color =name, alpha = selected_node ), size = 3) 


###################################################################################
graph_routes <- as_tbl_graph(greece)

graph_routes


graph_routes <- graph_routes %>%
  activate(nodes) %>%
  mutate(
    title = str_to_title(name),
    label = str_replace_all(title, " ", "\n")
  )

stations <- graph_routes %>%
  activate(nodes) %>%
  pull(title)

stations
graph_routes1<-graph_routes
graph_routes1<-morph(to_minimum_spanning_tree,weights=distance)






str(graph_routes1)
from <- which(stations == "ŒàŒ≤œÅŒøœÇ")
to <-  which(stations == "ŒúŒµœÉœÉŒ∑ŒΩŒØŒ±")

shortest1 <- graph_routes1 %>%
  morph(to_shortest_path, from, to, weights = distance)   %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph() %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

shortest %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = label, color =name, alpha = selected_node ), size = 3)


############################################################################
shortest <-graph_routes %>%
  morph(to_minimum_spanning_tree,weights = NULL) %>%
  mutate(selected_node = TRUE) %>%
  activate(edges) %>%
  mutate(selected_edge = TRUE) %>%
  unmorph() %>%
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

shortest

shortest %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = label, color =name, alpha = selected_node ), size = 3)
