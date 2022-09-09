# -*- coding: utf-8 -*-
"""
Created on Tue Mar 16 00:33:03 2021

@author: mixmp
"""




import networkx as nx
import pandas as pd
import matplotlib.pyplot as plt
import collections
import numpy
#!/usr/bin/env python3
import sys

import pylab as pl


import itertools
import matplotlib.pyplot
import networkx
import numpy
import powerlaw
import random
import scipy, scipy.special


import numpy as np


from scipy.stats import binned_statistic




################################## Problhma 1#####################################


#################### Erwthma 1a###############################


# Load the fisrt graph into an undirected NetworkX graph
G1_undirected = nx.read_edgelist("p2p-Gnutella09 (3).txt.gz", comments='#', delimiter='\t', nodetype=int, create_using=nx.Graph())

# Load the first graph into an directed NetworkX graph
G1_directed = nx.read_edgelist("p2p-Gnutella09 (3).txt.gz", comments='#', delimiter='\t', nodetype=int, create_using=nx.DiGraph())


#################### Erwthma 1a###############################
nodes1 = len(G1_undirected)
print("The number of nodes of the first graph is: ",nodes1)


################## Erwthma 1 b/number of edges #######################
#edges of undirected graph
edges1_undirected = G1_undirected.size()

print("The number of edges of the first undirected graph is: ",edges1_undirected)

##edges of directed graph
edges1_directed = G1_directed.size()

print("The number of edges of the first directed graph is: ",edges1_directed)



#################### Erwthma 1c ####################################

number_reciprocated_edges1=nx.reciprocity(G1_directed)*edges1_directed 
print("the number of the reciprocated edges of the first graph is:",number_reciprocated_edges1)




#number_of_reciprocated_edges1=G1_undirected. to_undirected ( True ).size()

###################### Erwthma  1d / diameter########################

nx.is_strongly_connected(G1_directed) 

#diameter for the first directed graph
set_largest_scc_1 = max(nx.strongly_connected_components(G1_directed), key=len)
largest_scc_1 = G1_directed.subgraph(set_largest_scc_1)
diameter1_directed=nx.diameter(largest_scc_1)
print('The diameter of the first directed graph is',diameter1_directed)

#diameter for the first undirected graph
set_largest_scc_1_undirected = max(nx.connected_components(G1_undirected), key=len)
largest_scc_1_undirected = G1_undirected.subgraph(set_largest_scc_1_undirected)
diameter1_undirected=nx.diameter(largest_scc_1_undirected)
print('The diameter of the first undirected graph is', diameter1_undirected)





################# Erwthma 1e avaerage degree ######################

#directed first graph

degree = [val for (node, val) in G1_directed.degree()]
avg_degree = 0
for i in range(len(degree)):
    avg_degree = avg_degree + degree[i]
average_degree_1_dir = avg_degree/len(degree)
print("The average degree of the directed first graph is:  ", average_degree_1_dir)

#undirected second graph

degree = [val for (node, val) in G1_undirected.degree()]
avg_degree = 0
for i in range(len(degree)):
    avg_degree = avg_degree + degree[i]
average_degree_1_undir = avg_degree/len(degree)
print("The average degree of the undirected first graph is:  ", average_degree_1_undir )


##################### Erwthma 1 st  ###############

#directed first graph

#average clustering coefficient for directed first graph
av_clust_coef1_dir=nx.average_clustering(G1_directed)
print("The average clustering coefficient of the first directed graph is:", av_clust_coef1_dir)

#global clustering coefficient for directed first graph
clust_coef1_dir=nx.transitivity(G1_directed)
print("The global clustering coefficient of the first directed graph is:",clust_coef1_dir)

#undirected first graph


#average clustering coefficient for undirected first graph
av_clust_coef1_undir=nx.average_clustering(G1_undirected)
print("The average clustering coefficient of the first undirected graph is:", av_clust_coef1_undir)

#global clustering coefficient for undirected first graph
clust_coef1_undir=nx.transitivity(G1_undirected)
print("The global clustering coefficient of the first directed graph graph is:",clust_coef1_undir)





################ Erwthma 1z #################

#for the directed first graph

set_largest_scc_1_dir = max(nx.strongly_connected_components(G1_directed) , key=len)
largest_scc_1_dir = G1_directed.subgraph(set_largest_scc_1_dir)
print('The largest strongly connected component of the first directed graph has number of nodes: ', largest_scc_1_dir.number_of_nodes())
print('The largest strongly connected component of the first directed has number of edges: ', largest_scc_1_dir.number_of_edges())

#for the first undirected graph

set_largest_scc_1_undir = max(nx.connected_components(G1_undirected), key=len)
largest_scc_1_undir = G1_undirected.subgraph(set_largest_scc_1_undir)
print('The largest strongly connected component of the first undirected graph has number of nodes: ', largest_scc_1_undir.number_of_nodes())
print('The largest strongly connected component of the first undirected has number of edges: ', largest_scc_1_undir.number_of_edges())




################   Erwthma h        #################

#for the first directed graph

set_weakly_cc_1_dir = max(nx.weakly_connected_components(G1_directed) , key=len)
largest_wcc_1_dir = G1_directed.subgraph(set_weakly_cc_1_dir)
print('The largest weakly connected component of the first directed graph has number of nodes: ', largest_wcc_1_dir .number_of_nodes())
print('The largest weakly connected component of the first directed has number of edges: ',largest_wcc_1_dir.number_of_edges())

#for the first undirected graph

set_weakly_cc_1_undir = max(nx.connected_components(G1_undirected) , key=len)
largest_wcc_1_undir = G1_undirected.subgraph(set_weakly_cc_1_undir)
print('The largest weakly connected component of the second undirected graph has number of nodes: ', largest_wcc_1_undir.number_of_nodes())
print('The largest weakly connected component of the second undirected has number of edges: ',largest_wcc_1_undir.number_of_edges())


############### Erwthma 2 katanomh twn bathmwn twn kombwn################

####################### ERWTHMA 2a ###################################
def plot_degree_dist(G):
    degrees = [G.degree(n) for n in G.nodes()]
    plt.plot(degrees)
    plt.xlabel('Degree')
    plt.ylabel('Frequency')
    plt.title("Degree Distribution of the graph in linear scale")
    plt.show()
    
plot_degree_dist(G1_directed)

plot_degree_dist(G1_undirected)


######################### Erwthma 2b ######################################

#log-log scale
def degree_distribution(G):
    
    #Plot the degree distribution on a log-log plot
    
    d = nx.degree_histogram(G)
    x = range(len(d))
    y = [i / float(sum(d)) for i in d]
    plt.figure()
    plt.loglog(x,y)
    plt.title("Degree Distribution Log-Log Plot")
    plt.savefig('Degree Distribution Loglog Plot.png')
    plt.show()
    

degree_distribution(G1_directed)
degree_distribution(G1_undirected)

#2os tropos goa to erwthma 2b

hist = nx.degree_histogram(G1_directed)
print(hist)
pl.figure()
pl.loglog(hist)
pl.title('Original degree distribution')

############################### Erwthma 2c ###########################3

############################## 1os tropos gia 2c ##############
from collections import Counter
import math
import networkx as nx
#import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np


def drop_zeros(a_list):
    return [i for i in a_list if i>0]


def log_binning(counter_dict, bin_count=35):
    
    max_x = math.log10(max(counter_dict.keys()))
    max_y = math.log10(max(counter_dict.values()))
    max_base = max([max_x, max_y])
    
    min_x = math.log10(min(drop_zeros(counter_dict.keys())))
    
    bins = np.logspace(min_x, max_base, num=bin_count)
    
    data_x = np.array(list(counter_dict.keys()))
    data_y = np.array(list(counter_dict.values()))
    
    
    bin_means_x = (np.histogram(data_x, bins, weights=data_x)[0] /np.histogram(data_x, bins)[0])
    bin_means_y = (np.histogram(data_y, bins, weights=data_y)[0] /np.histogram(data_y, bins)[0])
    return bin_means_x, bin_means_y

mygraph = G1_directed

ba_c = nx.degree_centrality(mygraph)
# To convert normalized degrees to raw degrees
ba_c2 = dict(Counter(ba_c.values()))

ba_x,ba_y = log_binning(ba_c2,50)

plt.xscale("log")
plt.yscale("log")

plt.scatter(ba_x,ba_y,c='r',marker='s',s=50)
plt.scatter(ba_c2.keys(),ba_c2.values(),c='b',marker='x')

plt.xlim((1e-4,1e-1))
plt.ylim((.9,1e4))

plt.xlabel('Connections (normalized)')
plt.ylabel('Frequency')

plt.show()



#################################################################

# 2os tropos gia 2c##########################

def log_binning(graph: networkx.Graph, base: int = 2) -> list:
    
    if base == 1:
        raise ValueError
    n = graph.number_of_nodes()
    dist = degree_distribution(graph)
    k_max = max(list(dist.keys()))
    number_of_bins = int(scipy.logn(base, k_max)) + 1
    print(f'Dividing the degree distribution in {number_of_bins: d} bins')

    bins = [
        {'n': 0, 'n*k': 0, 'avg_k': 0.0, 'p_k': 0.0}
        for _ in range(number_of_bins)
    ]
    for k in dist.keys():
        bins[int(scipy.logn(base, k))]['n'] += dist[k]
        bins[int(scipy.logn(base, k))]['n*k'] += dist[k] * k

    for i, _bin in enumerate(bins):
        _bin['avg_k'] = _bin['n*k'] / _bin['n']
        _bin['p_k'] = _bin['n'] / n / scipy.power(base, i)
        # _bin['p_k'] = _bin['n'] / scipy.power(base, i)

    return bins





def plot_pdf(graph: networkx.Graph, log_b=True):
    """
    Plot the degree distribution with relevant power law and poisson
    distribution fits
    :param graph: the networkx Graph() object
    :param log_b: plot the log binned distribution if required
    :return: plot of the distribution
    """
    graph=G1_directed
    matplotlib.pyplot.figure()
    matplotlib.pyplot.grid(True)
    deg = graph.degree()
    d = degree_distrib(graph)
    k = numpy.fromiter(d.keys(), dtype=numpy.int)
    p = numpy.fromiter(
        map(lambda x: x / len(deg), d.values()),
        # d.values(),
        dtype=numpy.float
    )
    r = powerlaw.Fit(
        numpy.fromiter(dict(deg).values(), dtype=numpy.float) / len(deg)
    )
    # p = p / len(deg)
    matplotlib.pyplot.loglog(k, p, 'ro', alpha=.3)
    matplotlib.pyplot.ylabel('p(k)')
    matplotlib.pyplot.xlabel('k')
    if log_b:
        binned = log_binning(graph)
        binned_k = [i['avg_k'] for i in binned]
        binned_p = [i['p_k'] for i in binned]
        matplotlib.pyplot.loglog(binned_k, binned_p, 'b+')

    # pdf = scipy.power(k, -r.alpha) * scipy.power(r.xmin, r.alpha - 1) * (
    #         r.alpha - 1
    # )
    pdf = scipy.power(k, -r.alpha) / scipy.special.zeta(r.xmin, r.alpha)
    matplotlib.pyplot.loglog(k, pdf, 'm')
    # r.plot_pdf()
    matplotlib.pyplot.show()
    
plot_pdf(G1_directed)




###################### Erwthma 2d ####################################




def degree_distrib(graph):

    #Calculate the actual degree distribution of a network.
    d = sorted(graph.degree, key=lambda x: x[1])
    return collections.Counter(map(lambda deg: deg[1], d))



def degree_cumulative_distribution(graph):
     d = degree_distrib(graph)
     p_k = numpy.fromiter(d.values(), numpy.float) / sum(d.values())
     k = numpy.fromiter(d.keys(), numpy.float)
     _P_k = numpy.fromiter(itertools.accumulate(p_k), dtype=numpy.float)
    #cumulative distribution
     matplotlib.pyplot.loglog(k, _P_k, 'k*')
     matplotlib.pyplot.ylabel('P(k)')
     matplotlib.pyplot.xlabel('k')
     matplotlib.pyplot.xlabel("cummulative distribution in log-log scale")
     matplotlib.pyplot.show()
     return _P_k

degree_cumulative_distribution(G1_directed)
degree_cumulative_distribution(G1_undirected)



########################## Erwthma 2e ####################################

#log-log scale
def zipf_sec(G):
    
    #Plot the degree distribution on a log-log plot
    
    d = nx.degree_histogram(G)
    x = range(len(d))
    y = [i / float(sum(d)) for i in d]
    plt.figure()
    plt.loglog(y,x)
    plt.title("Zipf Degree Distribution Log-Log Plot")
    plt.show()
    

zipf_sec(G1_directed)




#def zipf_plot_degree_dist(G):
    
    #degrees = [G.degree(n) for n in G.nodes()]
    #inv_degrees= [-i for i in degrees]
    #sort_index = numpy.argsort(inv_degrees)
    #degrees=sorted(degrees,reverse=True)
    #xs = [x for x in numpy.log(sort_index)]
    #ys = [x for x in numpy.log(degrees)]
    #plt.plot(xs, ys)
    #plt.xlabel('log node')
   # plt.ylabel('log degree')
   # plt.show()
    
#zipf_plot_degree_dist(G1_directed)



############################## Net: High Energy Physics #####################3 




# Load the second graph into an undirected NetworkX graph
G2_undirected = nx.read_edgelist("CA-HepTh.txt", comments='#', delimiter='\t', nodetype=int, create_using=nx.Graph())

# Load the second graph into an directed NetworkX graph
G2_directed = nx.read_edgelist("CA-HepTh.txt", comments='#', delimiter='\t', nodetype=int, create_using=nx.DiGraph())


#################### Erwthma 1a###############################
nodes2 = len(G2_undirected)
print("The number of nodes of the second graph is: ",nodes2)


################## Erwthma 1 b/number of edges #######################
edges2 = G2_undirected.size()

print("The number of edges of the second undirected graph is: ",edges2)


edges2_directed = G2_directed.size()

print("The number of edges of the second directed graph is: ",edges2_directed)



#################### Erwthma 1c ####################################

number_reciprocated_edges2=nx.reciprocity(G2_directed)*edges2_directed 
print("the number of the reciprocated egges in the second directed graph is:",number_reciprocated_edges2)


###################### Erwthma  1d / diameter########################

nx.is_strongly_connected(G2_directed) 

#diameter for the second directed graph
set_largest_scc_2 = max(nx.strongly_connected_components(G2_directed), key=len)
largest_scc_2 = G2_directed.subgraph(set_largest_scc_2)
diameter2_directed=nx.diameter(largest_scc_2)
print('The diameter of the second directed graph is',diameter2_directed)

#diameter for the second undirected graph
set_largest_scc_2_undirected = max(nx.connected_components(G2_undirected), key=len)
largest_scc_2_undirected = G2_undirected.subgraph(set_largest_scc_2_undirected)
diameter2_undirected=nx.diameter(largest_scc_2_undirected)
print('The diameter of the second undirected graph is', diameter2_undirected)





################# Erwthma 1e avaerage degree ######################

#directed second graph

degree = [val for (node, val) in G2_directed.degree()]
avg_degree = 0
for i in range(len(degree)):
    avg_degree = avg_degree + degree[i]
average_degree_2_dir = avg_degree/len(degree)
print("The average degree of the directed second graph is:  ", average_degree_2_dir)

#undirected second graph

degree = [val for (node, val) in G2_undirected.degree()]
avg_degree = 0
for i in range(len(degree)):
    avg_degree = avg_degree + degree[i]
average_degree_2_undir = avg_degree/len(degree)
print("The average degree of the undirected second graph is:  ", average_degree_2_undir )


##################### Erwthma 1 st  ###############

#directed second graph

#average clustering coefficient for directed second graph
av_clust_coef2_dir=nx.average_clustering(G2_directed)
print("The average clustering coefficient of the second directed graph is:", av_clust_coef2_dir)

#global clustering coefficient for directed second graph
clust_coef2_dir=nx.transitivity(G2_directed)
print("The global clustering coefficient of the second directed graph is:",clust_coef2_dir)

#undirected second graph


#average clustering coefficient for undirected second graph
av_clust_coef2_undir=nx.average_clustering(G2_undirected)
print("The average clustering coefficient of the second undirected graph is:", av_clust_coef2_undir)

#global clustering coefficient for undirected second graph
clust_coef2_undir=nx.transitivity(G2_undirected)
print("The clustering coefficient of the second directed graph graph is:",clust_coef2_undir)





################ Erwthma 1z #################

#for the directed second graph

set_largest_scc_2_dir = max(nx.strongly_connected_components(G2_directed) , key=len)
largest_scc_2_dir = G2_directed.subgraph(set_largest_scc_2_dir)
print('The largest strongly connected component of the second directed graph has number of nodes: ', largest_scc_2_dir.number_of_nodes())
print('The largest strongly connected component of the second directed has number of edges: ', largest_scc_2_dir.number_of_edges())

#for the second undirected graph

set_largest_scc_2_undir = max(nx.connected_components(G2_undirected), key=len)
largest_scc_2_undir = G2_undirected.subgraph(set_largest_scc_2_undir)
print('The largest strongly connected component of the second undirected graph has number of nodes: ', largest_scc_2_undir.number_of_nodes())
print('The largest strongly connected component of the second undirected has number of edges: ', largest_scc_2_undir.number_of_edges())




################   Erwthma h        #################

#for the second directed graph

set_weakly_cc_2_dir = max(nx.weakly_connected_components(G2_directed) , key=len)
largest_wcc_2_dir = G2_directed.subgraph(set_weakly_cc_2_dir)
print('The largest weakly connected component of the second directed graph has number of nodes: ', largest_wcc_2_dir .number_of_nodes())
print('The largest weakly connected component of the second directed has number of edges: ',largest_wcc_2_dir.number_of_edges())

#for the second undirected graph

set_weakly_cc_2_undir = max(nx.connected_components(G2_undirected) , key=len)
largest_wcc_2_undir = G2_undirected.subgraph(set_weakly_cc_2_undir)
print('The largest weakly connected component of the second undirected graph has number of nodes: ', largest_wcc_2_undir.number_of_nodes())
print('The largest weakly connected component of the second undirected has number of edges: ',largest_wcc_2_undir.number_of_edges())


#################### ERWTHMA 2 PLOTS #######################

###################### ERWTHMA 2a ###################################

    
plot_degree_dist(G2_directed)



######################### Erwthma 2b ######################################

#log-log scale

    
degree_distribution(G2_directed)




hist = nx.degree_histogram(G2_directed)
print('When binned:')
print(hist)
pl.figure()
pl.loglog(hist)
pl.title('Original degree distribution')


############################### Erwthma 2c ###########################3


#2os tropos

from collections import Counter
import math
import networkx as nx
#import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np


def drop_zeros(a_list):
    return [i for i in a_list if i>0]


def log_binning(counter_dict, bin_count=35):
    
    max_x = math.log10(max(counter_dict.keys()))
    max_y = math.log10(max(counter_dict.values()))
    max_base = max([max_x, max_y])
    
    min_x = math.log10(min(drop_zeros(counter_dict.keys())))
    
    bins = np.logspace(min_x, max_base, num=bin_count)
    
    data_x = np.array(list(counter_dict.keys()))
    data_y = np.array(list(counter_dict.values()))
    
    
    bin_means_x = (np.histogram(data_x, bins, weights=data_x)[0] /np.histogram(data_x, bins)[0])
    bin_means_y = (np.histogram(data_y, bins, weights=data_y)[0] /np.histogram(data_y, bins)[0])
    return bin_means_x, bin_means_y

mygraph = G2_directed

ba_c = nx.degree_centrality(mygraph)
# To convert normalized degrees to raw degrees
ba_c2 = dict(Counter(ba_c.values()))

ba_x,ba_y = log_binning(ba_c2,50)

plt.xscale("log")
plt.yscale("log")

plt.scatter(ba_x,ba_y,c='r',marker='s',s=50)
plt.scatter(ba_c2.keys(),ba_c2.values(),c='b',marker='x')

plt.xlim((1e-4,1e-1))
plt.ylim((.9,1e4))

plt.xlabel('Connections (normalized)')
plt.ylabel('Frequency')

plt.show()



###################### Erwthma 2d ####################################



degree_cumulative_distribution(G2_directed)

########################## Erwthma 2e ####################################


zipf_sec(G2_directed)



#########################Problhma 2####################################



#paragwgh tuxaiwn diktuwn
import random as rd

import scipy as sp


rd.seed()

# h sunarthsh 2a sunopsizei ta erwthmata tou provlhmatos 1 gia na ta xrhsimopoihw gia ta tuxaia diktua pou tha paraxthoun
def question_1_2(graph):
    #################### Erwthma 1a###############################
    nodes = len(graph)
    print("The number of nodes of the graph is: ",nodes)
    
    flag=nx.is_directed(graph)
    ################## Erwthma 1 b/number of edges #######################
    
    if flag:
       edges =graph.size()
       print("The number of edges of the second directed graph is: ",edges)
    else:  
       edges  = graph.size()
       print("The number of edges of the undirected graph is: ",edges)
      
    
    if flag:
        #################### Erwthma 1c ####################################
        number_reciprocal_edges=nx.reciprocity(graph)*edges
        print("The number of reciprocal edges of the directed graph is:",number_reciprocal_edges)
        
    ###################### Erwthma  1d / diameter########################
    
     
    if flag:
        #diameter for the directed graph
        set_largest_scc = max(nx.strongly_connected_components(graph), key=len)
        largest_scc = graph.subgraph(set_largest_scc)
        diameter=nx.diameter(largest_scc)
        print('The diameter of the directed graph is',diameter)
    else:
        #diameter for the undirected graph
        set_largest_scc= max(nx.connected_components(graph), key=len)
        largest_scc= graph.subgraph( set_largest_scc)
        diameter=nx.diameter(largest_scc)
        print('The diameter of the second undirected graph is', diameter)
    
    
    
    
    
    ################# Erwthma 1e avaerage degree ######################
    
    degree = [val for (node, val) in graph.degree()]
    avg_degree = 0
    for i in range(len(degree)):
        avg_degree = avg_degree + degree[i]
    average_degree = avg_degree/len(degree)
    print("The average degree of the  graph is:  ", average_degree)
    
    
    
    ##################### Erwthma 1 st  ###############
    
    #directed second graph
    
    #average clustering coefficient for the graph
    av_clust_coef=nx.average_clustering(graph)
    print("The average clustering coefficient of the  graph is:", av_clust_coef)
    
    #global clustering coefficient for the graph
    clust_coef=nx.transitivity(graph)
    print("The global clustering coefficient of the  graph is:",clust_coef)
    
    
    
    
    
    ################ Erwthma 1z #################
    
    #for the directed graph
    if flag:
        set_largest_scc= max(nx.strongly_connected_components(graph) , key=len)
        largest_scc = graph.subgraph(set_largest_scc)
        print('The largest strongly connected component of the directed graph has number of nodes: ', largest_scc.number_of_nodes())
        print('The largest strongly connected component of the directed has number of edges: ', largest_scc.number_of_edges())
    else:
        #for the second undirected graph
        
        set_largest_scc = max(nx.connected_components(graph), key=len)
        largest_scc = graph.subgraph(set_largest_scc)
        print('The largest strongly connected component of the undirected graph has number of nodes: ', largest_scc.number_of_nodes())
        print('The largest strongly connected component of the undirected has number of edges: ', largest_scc.number_of_edges())
        
    
    
    
    ################   Erwthma h        #################
    
    #for the first directed graph
    if flag:
        set_weakly_cc = max(nx.weakly_connected_components(graph) , key=len)
        largest_wcc = graph.subgraph(set_weakly_cc)
        print('The largest weakly connected component of the directed graph has number of nodes: ', largest_wcc.number_of_nodes())
        print('The largest weakly connected component of the  directed has number of edges: ',largest_wcc.number_of_edges())
    else:
        #for the second undirected graph 
        set_weakly_cc = max(nx.connected_components(graph) , key=len)
        largest_wcc= graph.subgraph(set_weakly_cc)
        print('The largest weakly connected component of the undirected graph has number of nodes: ', largest_wcc.number_of_nodes())
        print('The largest weakly connected component of the undirected has number of edges: ',largest_wcc.number_of_edges())
    
    
    #################### ERWTHMA 2 PLOTS #######################
    
    ###################### ERWTHMA 2a ###################################
    
        
    plot_degree_dist(graph)
    
    
    
    ######################### Erwthma 2b ######################################
    
    #pdf of degree in log-log scale
    
        
    degree_distribution(graph)
    
    ############################### Erwthma 2c ###########################3
    
        
    def drop_zeros(a_list):
     return [i for i in a_list if i>0]


    def log_binning(counter_dict, bin_count=35):
    
        max_x = math.log10(max(counter_dict.keys()))
        max_y = math.log10(max(counter_dict.values()))
        max_base = max([max_x, max_y])
        
        min_x = math.log10(min(drop_zeros(counter_dict.keys())))
        
        bins = np.logspace(min_x, max_base, num=bin_count)
        
        data_x = np.array(list(counter_dict.keys()))
        data_y = np.array(list(counter_dict.values()))
        
        
        bin_means_x = (np.histogram(data_x, bins, weights=data_x)[0] /np.histogram(data_x, bins)[0])
        bin_means_y = (np.histogram(data_y, bins, weights=data_y)[0] /np.histogram(data_y, bins)[0])
        return bin_means_x, bin_means_y

    mygraph = graph
    
    ba_c = nx.degree_centrality(mygraph)
    # To convert normalized degrees to raw degrees
    ba_c2 = dict(Counter(ba_c.values()))
    
    ba_x,ba_y = log_binning(ba_c2,50)
    
    plt.xscale("log")
    plt.yscale("log")
    
    plt.scatter(ba_x,ba_y,c='r',marker='s',s=50)
    plt.scatter(ba_c2.keys(),ba_c2.values(),c='b',marker='x')
    
    plt.xlim((1e-4,1e-1))
    plt.ylim((.9,1e4))
    
    plt.xlabel('Connections (normalized)')
    plt.ylabel('Frequency')
    
    plt.show()
    
    
    ###################### Erwthma 2d ####################################
    
    
    
    degree_cumulative_distribution(graph)
    
    ########################## Erwthma 2e ####################################
    
    
    #zipf_plot_degree_dist(graph)
    
    
    zipf_sec(graph)
    return
    



#########################                   Erwthma A           #####################################
 
#########################Erdos Renyi###################################

#Create an G{n,m} random graph with n nodes and m edges
#This graph is sometimes called the Erdős-Rényi graph



n = 8500  # 8500 nodes
m = edges2  # 25998 edges

er = nx.gnm_random_graph(8500, edges2)
nx.is_directed(er)



#gia to diktuo twn Erdos Renyi upologizw ksana ta erwwthmata tou problhmatos 1 mesw ths sunarthshs question_1_2

question_1_2(er)


#   plot 2c 
def drop_zeros(a_list):
    return [i for i in a_list if i>0]


def log_binning(counter_dict, bin_count=35):
    
    max_x = math.log10(max(counter_dict.keys()))
    max_y = math.log10(max(counter_dict.values()))
    max_base = max([max_x, max_y])
    
    min_x = math.log10(min(drop_zeros(counter_dict.keys())))
    
    bins = np.logspace(min_x, max_base, num=bin_count)
    
    data_x = np.array(list(counter_dict.keys()))
    data_y = np.array(list(counter_dict.values()))
    
    
    bin_means_x = (np.histogram(data_x, bins, weights=data_x)[0] /np.histogram(data_x, bins)[0])
    bin_means_y = (np.histogram(data_y, bins, weights=data_y)[0] /np.histogram(data_y, bins)[0])
    return bin_means_x, bin_means_y

mygraph = er

ba_c = nx.degree_centrality(mygraph)
# To convert normalized degrees to raw degrees
ba_c2 = dict(Counter(ba_c.values()))

ba_x,ba_y = log_binning(ba_c2,50)

plt.xscale("log")
plt.yscale("log")

plt.scatter(ba_x,ba_y,c='r',marker='s',s=50)
plt.scatter(ba_c2.keys(),ba_c2.values(),c='b',marker='x')

plt.xlim((1e-4,1e-1))
plt.ylim((.9,1e4))

plt.xlabel('Connections (normalized)')
plt.ylabel('Frequency')

plt.show()



#################### ERWTHMA B ##############################33

#################### barabasi_albert_graph #####################




ba = nx.barabasi_albert_graph(8500, 3)



nodes_ba = len(ba)
print("The number of nodes of the  graph is: ",nodes_ba)


edges_ba =  ba.size()
ba.number_of_edges()

print("The number of edges of the graph is: ",edges_ba)

#gia to diktuo twn barabasi_albert upologizw ksana ta erwwthmata tou problhmatos 1 mesw ths sunarthshs question_1_2

question_1_2(ba)

# erwthma gia plot 2c
def drop_zeros(a_list):
    return [i for i in a_list if i>0]


def log_binning(counter_dict, bin_count=35):
    
    max_x = math.log10(max(counter_dict.keys()))
    max_y = math.log10(max(counter_dict.values()))
    max_base = max([max_x, max_y])
    
    min_x = math.log10(min(drop_zeros(counter_dict.keys())))
    
    bins = np.logspace(min_x, max_base, num=bin_count)
    
    data_x = np.array(list(counter_dict.keys()))
    data_y = np.array(list(counter_dict.values()))
    
    
    bin_means_x = (np.histogram(data_x, bins, weights=data_x)[0] /np.histogram(data_x, bins)[0])
    bin_means_y = (np.histogram(data_y, bins, weights=data_y)[0] /np.histogram(data_y, bins)[0])
    return bin_means_x, bin_means_y

mygraph = ba

ba_c = nx.degree_centrality(mygraph)
# To convert normalized degrees to raw degrees
ba_c2 = dict(Counter(ba_c.values()))

ba_x,ba_y = log_binning(ba_c2,50)

plt.xscale("log")
plt.yscale("log")

plt.scatter(ba_x,ba_y,c='r',marker='s',s=50)
plt.scatter(ba_c2.keys(),ba_c2.values(),c='b',marker='x')

plt.xlim((1e-4,1e-1))
plt.ylim((.9,1e4))

plt.xlabel('Connections (normalized)')
plt.ylabel('Frequency')

plt.show()



 #######################################ERWTHMA C
 
##################### Watts-Strogatz###############################
ws2 = nx.watts_strogatz_graph(8500, 7, 0.09)
nodes_ws = len(ws2)
print("The number of nodes of the  graph is: ",nodes_ws)


################## Erwthma 1 b/number of edges#######################
edges_ws =  ws2.size()

print("The number of edges of the graph is: ",edges_ws)


#gia to diktuo twn Watts-Strogatz upologizw ksana ta erwwthmata tou problhmatos 1 mesw ths sunarthshs question_1_2
question_1_2(ws2)

#  Erwthma gia plot 2c
def drop_zeros(a_list):
    return [i for i in a_list if i>0]


def log_binning(counter_dict, bin_count=35):
    
    max_x = math.log10(max(counter_dict.keys()))
    max_y = math.log10(max(counter_dict.values()))
    max_base = max([max_x, max_y])
    
    min_x = math.log10(min(drop_zeros(counter_dict.keys())))
    
    bins = np.logspace(min_x, max_base, num=bin_count)
    
    data_x = np.array(list(counter_dict.keys()))
    data_y = np.array(list(counter_dict.values()))
    
    
    bin_means_x = (np.histogram(data_x, bins, weights=data_x)[0] /np.histogram(data_x, bins)[0])
    bin_means_y = (np.histogram(data_y, bins, weights=data_y)[0] /np.histogram(data_y, bins)[0])
    return bin_means_x, bin_means_y

mygraph = ws2

ba_c = nx.degree_centrality(mygraph)
# To convert normalized degrees to raw degrees
ba_c2 = dict(Counter(ba_c.values()))

ba_x,ba_y = log_binning(ba_c2,50)

plt.xscale("log")
plt.yscale("log")

plt.scatter(ba_x,ba_y,c='r',marker='s',s=50)
plt.scatter(ba_c2.keys(),ba_c2.values(),c='b',marker='x')

plt.xlim((1e-4,1e-1))
plt.ylim((.9,1e4))

plt.xlabel('Connections (normalized)')
plt.ylabel('Frequency')

plt.show()



zipf_sec(ws2)






