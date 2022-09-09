#!/usr/bin/env python
# coding: utf-8

# In[77]:


#| Import the libraries that we'll use

import matplotlib.pyplot as plt
import numpy as np
import networkx as nx
from networkx.algorithms import community
import random
import pandas as pd
from networkx.algorithms import community
import itertools as it
import scipy.cluster.hierarchy as hierarchy
get_ipython().run_line_magic('matplotlib', 'inline')
#| Import the libraries that we'll use

from community import community_louvain
from networkx.algorithms.community import greedy_modularity_communities
from networkx.algorithms.community import label_propagation_communities
from networkx.algorithms.community import asyn_lpa_communities
from sklearn.metrics import normalized_mutual_info_score
from sklearn.metrics import recall_score
from sklearn.metrics import accuracy_score
from sklearn.metrics import precision_score
from sklearn.metrics import mutual_info_score

#from purity import purity_score
from sklearn.metrics import adjusted_mutual_info_score

from IPython.display import display, HTML, display_pretty
import os
np.random.seed(50)

import random
import pandas as pd
import scipy.cluster.hierarchy as hierarchy
from networkx.algorithms.community.centrality import girvan_newman
import matplotlib.pyplot as plt
import networkx as nx

from networkx.drawing.nx_agraph import graphviz_layout
from collections import Counter


# In[163]:


from networkx.algorithms.community.centrality import girvan_newman


# In[78]:


############ sunarthsh gia ton upologismo tou conductance apo mia diamerish enos diktuou se koinothtes ##################

import networkx as nx

# Calculates the conductance score of a community

def calculateConductance(partition, graph):
    # max_conductance = 0

    # for group in partition:
    #     if (len(group) == 1 or len(group) == len(graph.nodes())):
    #         continue

    #     group_conductance = nx.conductance(graph, group)
    #     if group_conductance > max_conductance:
    #         max_conductance = group_conductance    
    
    # if (max_conductance == 0):
    #     return 1

    average_conductance = 0
    count = 0

    for group in partition:
        if (len(group) == 1 or len(group) == len(graph.nodes())):
            average_conductance += 1
            count += 1   
            continue

        average_conductance += nx.conductance(graph, group)
        count += 1

    return average_conductance/count


# In[79]:


######################  sunarthsh gia ton upologismo tou conductance apo mia diamerish enos diktuou se koinothtes ##################


def modularity(G, partition):
    m = G.number_of_edges()
    
    degree = G.degree()
    norm = 1.0/(2.0*m)
    
    Q = 0.0
    for c in partition:
        for u, v in it.product(c, repeat=2):
            w = 1 if G.has_edge(u, v) else 0
            #  double count self loop
            if u == v:
                w *= 2.0
            Q += w - degree[u] * degree[v] * norm
    return norm*Q


# In[216]:


########################### sunarthsh gia ton upologismo tou purity #####################################
def purity_score(y_true, y_pred):
   
    y_true=np.array(y_true)
    y_pred=np.array(y_pred)
    # matrix which will hold the majority-voted labels
    y_voted_labels = np.zeros(y_true.shape)
    # Ordering labels
    ## Labels might be missing e.g with set like 0,2 where 1 is missing
    ## First find the unique labels, then map the labels to an ordered set
    ## 0,2 should become 0,1
    labels = np.unique(y_true)
    ordered_labels = np.arange(labels.shape[0])
    for k in range(labels.shape[0]):
        y_true[y_true==labels[k]] = ordered_labels[k]
    # Update unique labels
    labels = np.unique(y_true)
    # We set the number of bins to be n_classes+2 so that 
    # we count the actual occurence of classes between two consecutive bin
    # the bigger being excluded [bin_i, bin_i+1[
    bins = np.concatenate((labels, [np.max(labels)+1]), axis=0)

    for cluster in np.unique(y_pred):
        hist, _ = np.histogram(y_true[y_pred==cluster], bins=bins)
        # Find the most present label in the cluster
        winner = np.argmax(hist)
        y_voted_labels[y_pred==cluster] = winner
    
    return accuracy_score(y_true, y_voted_labels)


# In[82]:



#| H parakatw sunarthsh vohthaei sto na metatrapoun oi diameriseis pou dhmiourgountai se
#|  listes classification.
#  Ousiastika metatrepei mia lista apo communities, opou kathe community einai mia lista apo kombous, se mia lista apo
#   classes, opou h timh v sth thesi i shmainei oti o kombos
#  i anhkei sthn community v.

def communitiesToClassification(communities):
    # Get number of nodes
    nNodes = len(G)
    # Initialize a list of size nNodes
    classification = [0]*nNodes
    for i in range(len(communities)):
            classification[i] = i
    # Return classification
    return classification


# In[212]:


##################### sunarthsh pou upologizei gia ena diktuo pou ta values twn nodes einai strings kai oxi arithmoi ta ekshs:
#i. Akribeia (precision)
#ii. Pososto Anaklhshs (recall)
#iii. Purity
#iv. NMI
#v. modularity (kai sugkrinetai me to modularity twn ground-truth koinothtwn)
#vi. conductance (kai sugkrinetai me to conductance twn ground-truth koinothtwn)


def evaluate(graph,commun):
  G = graph
 # Get the true set of communities
  communities = {frozenset(G.nodes[v]['value']) for v in G}
  communities = sorted(map(sorted, communities))
   # Turn partition into classification
  realClassification = communitiesToClassification(communities) 
    
  classification = communitiesToClassification(commun)
  # Calculate Normalized Mutual Information
  nmi = normalized_mutual_info_score(realClassification, classification, average_method='arithmetic')
  print("Normalized Mutual Information is:",nmi)
  print("\n")
  ac_score=precision_score(realClassification, classification,average='micro')
  print("The precision is:", ac_score)
  print("\n")
  re_score=recall_score(realClassification, classification,average='micro')
  print("The recall is:", re_score)
  print("\n")
 # pur_score=purity_score(realClassification,classification )
 # print("The purity is:", pur_score)  
  print("\n")
  mod=modularity(G, commun)
  print("The modularity is:", mod) 
  print("\n")
  pur_2=purity_score(realClassification,classification)
  print("The purity is:", pur_2) 
  print("\n")
  #Calculates the conductance score of a community
  conductance=calculateConductance(commun, graph)  
  print("The conductance is:", conductance)
  return 


# In[20]:


######################### load the data polbooks ##############################################################

book_net = nx.read_gml('C:/Users/mixmp/Desktop\ανάλυση δικτύων και εξόρυξη γνώσης από τον παγκόσμιο ιστό/2nd hw/polbooks.gml')


# In[21]:


#parakatw vlepoume poia clusters uparxoun gia ta biblia politikhs
{book_net.nodes[v]['value'] for v in  book_net}


# In[22]:


#parakatw vlepoume gia kathe node tou viliou polbooks, to cluster sto opoio anhkei
[book_net.nodes[v]['value'] for v in  book_net]


# In[31]:


#plhrofories gia to polbooks network
print(nx.info(book_net))


# In[24]:


#parakatw vlepoume to grafhma tou diktupo polbooks

spring_pos = nx.spring_layout(book_net)
plt.axis("off")
nx.draw_networkx(book_net, 
                 pos = spring_pos, 
                 with_labels = False, 
                 node_size = 35)


# In[25]:


########################### optikopoihsh twn ground truth koinothtwn gia to diktuo polbooks #################################


import networkx.algorithms.community as nx_comm
#Set up ground truth partition
G = book_net
dt_g = nx.get_node_attributes(G,'value')
dt_g
dt_values_g = list(dt_g.values())
color_map_g=[]
for x in dt_values_g:
    if x == 'c':
        color_map_g.append(0)
    elif x == 'l':
        color_map_g.append(1)
    else:
        color_map_g.append(2)
color_map_g

pos = nx.spring_layout(G)

communities_g = {}
for key, value in sorted(dt_g.items()):
    communities_g.setdefault(value, []).append(key)
communities_g_sets = [set(x) for x in communities_g.values()]
q_g = nx_comm.modularity(G,communities_g_sets)

#Plot ground truth partition
plt.figure(1)
plt.title("The modularity of Ground truth partition for book_net is, Q={}".format(q_g))
nx.draw(G, pos, node_color = color_map_g, cmap = plt.cm.get_cmap('rainbow'), with_labels=False, font_color = 'white')


# In[148]:


###############################3the ground truth partition is communities_g_sets 
mod=nx_comm.modularity(book_net,communities_g_sets)

print("The modularity of the ground truth communities for the book_net is:", mod)


# In[149]:


################################ upologismos tou conductance me bash tis ground truth communities me bash to book_net diktuo ####

#the ground truth partition is communities_g_sets 
conductance_book=calculateConductance( communities_g_sets,book_net)
print("The conductance of the ground truth communities for the book_net is:", conductance_book)


# In[232]:


#h parakatw sunarthsh optikopoiei tis koinothtes pou prokuptoun me bash tis diafores texnikes entopismou twn koinothtwn
def draw_communities_of_the_net(G, communities, name='title'):
    pos=nx.spring_layout(G)
    colors = ['b', 'g', 'r', 'c', 'm', 'y', 'k', 'w','coral','gold','plum','cyan','orange','orchid','lime','gray','pink','peru','olive','khaki','thistle','teal','darkblue','sienna','fuchsia','tomato','chocolate','indigo','azure','moccasin','navy','azure','beige','ivory','salmon','olive','brown','silver']
    plt.figure()
    plt.title(name, fontsize=20)
    aux = 0
    for community in communities:
        nx.draw_networkx_nodes(G, pos, community, node_size = 50, node_color = colors[aux])
        aux = aux + 1
    nx.draw_networkx_edges(G, pos, alpha=0.5)
    plt.show(block=True)


# In[83]:


############################### (b erwthma) sunarthsh gia th methodo megistopoihshs tou modularity (Louvain) #################################


def louvain_method(G):
    partition = community_louvain.best_partition(G)
    communities = list()
    for com in set(partition.values()) :
        list_nodes = [nodes for nodes in partition.keys() if partition[nodes] == com]
        communities.append(sorted(list_nodes))
    return sorted(communities)


# In[84]:


#apply Louvain method in the book network
communities1=louvain_method(book_net)


# In[85]:


#draw communities that are detected by Louvain method 
draw_communities_of_the_net(book_net, communities1, name='Louvain method')


# In[213]:


# Diktuo: polbooks

#Methodos Entopismou Koinothtwn: (Louvain)

#i. Akribeia (precision)
#ii. Pososto Anaklhshs (recall)
#iii. Purity
#iv. NMI
#v. modularity (kai sugkrinetai me to modularity twn ground-truth koinothtwn)
#vi. conductance (kai sugkrinetai me to conductance twn ground-truth koinothtwn)


evaluate(book_net,communities1)


# In[168]:


girvan_newman_method(book_net)


# In[169]:


#apply Girvan Newman method to detect communities in the  polbook  network
communities2=girvan_newman_method(book_net)


# In[89]:


communities2


# In[170]:


#draw communities that are detected by Girvan Newman method for polbook network
draw_communities_of_the_net(book_net, communities2, name='Girvan-Newman')


# In[171]:


# Diktuo: polbooks

#Methodos Entopismou Koinothtwn: (Girvan-Newman)

#i. Akribeia (precision)
#ii. Pososto Anaklhshs (recall)
#iii. Purity
#iv. NMI
#v. modularity (kai sugkrinetai me to modularity twn ground-truth koinothtwn)
#vi. conductance (kai sugkrinetai me to conductance twn ground-truth koinothtwn)

evaluate(book_net,communities2)


# In[96]:


#clique percolation method on detection communitites
from networkx.algorithms import community as cm
communities3 = cm.k_clique_communities(book_net, 3) # 3-cliques


# In[66]:


communities3


# In[97]:


#gia na exoun thn idia morfh ta communities pou anagnwrizontai apo th methodo clique percolation me ta communities
#pou anagnwristhkan apo tiw alles methodous, dhladh na einai lista apo listes kai oxi tou tupou  
#<generator object k_clique_communities at 0x00000243660766D0>. Akolouthoun oi akolouthes entoles:
communities3_1=[]
for community in communities3:
    communities3_1.append(list(community))


# In[64]:


#parakatw blepw oti einai lista apo listes
communities3_1


# In[98]:


#draw communities from 3 clique percolation method on detection communitites
draw_communities_of_the_net(book_net, communities3_1, name='3 clique percolation')


# In[99]:


evaluate(book_net,communities3_1)


# In[100]:


#Community detection using Newman spectral methods to maximize modularity
from modularity_maximization import partition
communities4 = partition(book_net)
communities4


# In[101]:


#Community detection using Newman spectral methods to maximize modularity
def method4(G):
    partition1 = partition(G)  
    communities = list()
    for com in set(partition1.values()) :
        list_nodes = [nodes for nodes in partition1.keys() if partition1[nodes] == com]
        communities.append(sorted(list_nodes))
    return sorted(communities)
communities4=method4(book_net)


# In[102]:


draw_communities_of_the_net(book_net, communities4, name='Community detection using Newman spectral methods to maximize modularity')


# In[103]:


evaluate(book_net,communities4)


# In[104]:


evaluate(book_net,communities4)


# In[157]:


############################## diktuo adjnoun_net #######################################



adjnoun_net=nx.read_gml('C:/Users/mixmp/Desktop/ανάλυση δικτύων και εξόρυξη γνώσης από τον παγκόσμιο ιστό/adjnoun.gml')


# In[153]:


#######################################draw ground truth communities for adjnoun_net ##########################3


############################### Briskw thn ground truth partition gia to adjnoun net me tis parakatw entoles ################
G = adjnoun_net
dt_g = nx.get_node_attributes(G,'value')
dt_g
dt_values_g = list(dt_g.values())
color_map_g=[i for i in dt_values_g]
pos = nx.spring_layout(G)

communities_g = {}
for key, value in sorted(dt_g.items()):
    communities_g.setdefault(value, []).append(key)
communities_g_sets = [set(x) for x in communities_g.values()]
q_g = nx_comm.modularity(G,communities_g_sets)

#Plot ground truth partition
plt.figure(1)
plt.title("Ground truth partition for adjnoun_net, Q={}".format(q_g))
nx.draw(G, pos, node_color = color_map_g, cmap = plt.cm.get_cmap('rainbow'), with_labels=True, font_color = 'white')


# In[154]:


################################ upologismos tou modulrity me bash tis ground truth communities me bash to adjnoun_net diktuo ####

#the ground truth partition is communities_g_sets 
communities_g_truth_adjnoun=communities_g_sets
modularity_adjnoun=nx_comm.modularity(adjnoun_net,communities_g_truth_adjnoun)
print("The modularity of the ground truth communities for the adjnoun_net is:",modularity_adjnoun)


# In[155]:


print("The ground truth partition of adjnoun_net is:",communities_g_truth_adjnoun)


# In[151]:


################################ upologismos tou conductance me bash tis ground truth communities me bash to adjnoun_net diktuo ####

#the ground truth partition is communities_g_sets 
communities_g_truth_adjnoun=communities_g_sets
conductance_adjnoun=calculateConductance( communities_g_sets,adjnoun_net)
print("The conductance of the ground truth communities for the adjnoun_net is:", conductance_adjnoun)


# In[217]:


############# sunarthsh gia aksiologhsh twn methodwn entopismou koinothtwn gia to adjnon net diktuo #######################3


def evaluate2(graph,commun):
  G = graph

  realClassification =  [G.nodes[v]['value'] for v in G]
    
  classification = communitiesToClassification(commun)
  # Calculate Normalized Mutual Information
  nmi = normalized_mutual_info_score(realClassification, classification, average_method='arithmetic')
  print("Normalized Mutual Information is:",nmi)
  print("\n")
  ac_score=precision_score(realClassification, classification,average='micro')
  print("The precision is:", ac_score)
  print("\n")
  re_score=recall_score(realClassification, classification,average='micro')
  print("The recall is:", re_score)
  print("\n")
 # pur_score=purity_score(realClassification,classification )
 # print("The purity is:", pur_score)  
  print("\n")
  mod=modularity(G, commun)
  print("The modularity is:", mod) 
  print("\n")
  pur_2=purity_score(realClassification,classification)
  print("The purity is:", pur_2) 
  #Calculates the conductance score of a community
  conductance=calculateConductance(commun, G)  
  print("The conductance is:", conductance)
    
  return 


# In[108]:


print(nx.info(adjnoun_net))


# In[109]:


spring_pos = nx.spring_layout(adjnoun_net)
plt.axis("off")
nx.draw_networkx(adjnoun_net, 
                 pos = spring_pos, 
                 with_labels = False, 
                 node_size = 35)


# In[110]:


#apply Louvain method in the adjnoun network
communities1_2=louvain_method(adjnoun_net)


# In[113]:


draw_communities_of_the_net(adjnoun_net, communities1_2, name='Louvain- adjnoun net')


# In[218]:


evaluate2(adjnoun_net,communities1_2)


# In[172]:


#apply Girvan Newman method method in the adjnoun network
communities2_2=girvan_newman_method(adjnoun_net)


# In[173]:


communities2_2


# In[174]:


#draw communities from Girvan Newman method 
draw_communities_of_the_net(adjnoun_net, communities2_2, name='Girvan Newman- adjnoun')


# In[219]:


evaluate2(adjnoun_net,communities2_2)


# In[131]:


#k clique percolation 
communities3_2 = cm.k_clique_communities(adjnoun_net, 2) # 2-cliques


# In[73]:


communities3_2


# In[132]:


#gia na exoun thn idia morfh ta communities pou anagnwrizontai apo th methodo clique percolation me ta communities
#pou anagnwristhkan apo tiw alles methodous, dhladh na einai lista apo listes kai oxi tou tupou  
#<generator object k_clique_communities at 0x00000243660766D0>. Akolouthoun oi akolouthes entoles:
communities3_2_2=[]
for community in communities3_2:
    communities3_2_2.append(list(community))


# In[133]:


#draw communities from 4 clique percolation method on detection communitites
draw_communities_of_the_net(adjnoun_net, communities3_2_2, name='2 clique percolation-adjnoun')


# In[134]:


evaluate2(adjnoun_net,communities3_2_2)


# In[131]:


#Community detection using Newman spectral methods to maximize modularity
from modularity_maximization import partition


# In[135]:


communities4_2 =method4(adjnoun_net)


# In[138]:


#draw communities from community detection using Newman spectral methods to maximize modularity
draw_communities_of_the_net(adjnoun_net, communities4_2, name='community detection using Newman spectral methods to maximize modularity')


# In[139]:


evaluate2(adjnoun_net,communities4_2)


# In[176]:



#################### Problhma 2: Paragwgh Sunthetikwn diktuwn ##############################################

#1o sunthetiko diktuo

gen_graph=nx.generators.random_graphs.powerlaw_cluster_graph(500,2,0.4)


# In[177]:


#plhrofories gia thn paragwgh tou prwtou sunthetikou diktuou
print(nx.info(gen_graph))


# In[178]:


#apply Louvain method to  network which is created by powerlaw_cluster_graph
communities1_3=louvain_method(gen_graph)


# In[179]:


communities1_3


# In[180]:


#draw communities that were detected by Louvain Method
draw_communities_of_the_net(gen_graph, communities1_3, name='Sunthetiko diktuo 1-Louvain')


# In[181]:


modularity(gen_graph, communities1_3)


# In[182]:


calculateConductance(communities1_3, gen_graph)


# In[183]:


#spectral analysisi method gia to 1o sunthetiko diktuo
communities4_2 =method4(gen_graph)


# In[231]:


#draw communities that were detected by Spectral Method
draw_communities_of_the_net(gen_graph, communities4_2, name='1o Sunthetiko diktuo-Spectral Method')


# In[184]:


modularity(gen_graph, communities4_2)


# In[187]:


calculateConductance(communities4_2, gen_graph)


# In[188]:


#2o sunthetiko diktuo

gen_graph2=nx.generators.random_graphs.powerlaw_cluster_graph(500,1,0.3)


# In[189]:


#plhrofories gia thn paragwgh tou 2ou sunthetikou diktuou
print(nx.info(gen_graph2))


# In[190]:


#apply Louvain method to  network which is created by powerlaw_cluster_graph
communities1_3_2=louvain_method(gen_graph2)


# In[199]:


#draw communities that were detected by Louvain Method
draw_communities_of_the_net(gen_graph2, communities1_3_2, name='2o Sunthetiko diktuo 1-Louvain')


# In[201]:


modularity(gen_graph2, communities1_3_2)


# In[202]:


calculateConductance(communities1_3_2,gen_graph2 )


# In[220]:


#spectral analysisi method gia to 2o sunthetiko diktuo
communities4_2_2 =method4(gen_graph2)


# In[238]:


#draw communities that were detected by Louvain Method
draw_communities_of_the_net(gen_graph2, communities4_2_2, name='2o Sunthetiko diktuο- Spectral Analysis')


# In[222]:


modularity(gen_graph2, communities4_2_2)


# In[223]:


calculateConductance(communities4_2_2,gen_graph2 )


# In[224]:


#3o sunthetiko diktuo

gen_graph3=nx.generators.random_graphs.powerlaw_cluster_graph(500,1,0.1)


# In[225]:


#apply Louvain method to  network which is created by powerlaw_cluster_graph
communities3_3_1=louvain_method(gen_graph3)


# In[230]:


#draw communities that were detected by Louvain Method
draw_communities_of_the_net(gen_graph3, communities3_3_1, name='3o Sunthetiko diktuο- Louvain')


# In[234]:


modularity(gen_graph3, communities3_3_1)


# In[227]:


calculateConductance(communities3_3_1,gen_graph3 )


# In[228]:


#spectral analysisi method gia to 3o sunthetiko diktuo
communities3_2_2 =method4(gen_graph3)


# In[233]:


draw_communities_of_the_net(gen_graph3, communities3_2_2, name='3o Sunthetiko diktuο- Spectral Analysis')


# In[235]:


modularity(gen_graph3, communities3_2_2)


# In[237]:


calculateConductance(communities3_2_2,gen_graph3)


# In[ ]:




