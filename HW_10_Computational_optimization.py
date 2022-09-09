# -*- coding: utf-8 -*-
"""
Created on Tue Jan  5 19:27:16 2021

@author: mixmp
"""


import random
import math
import numpy as np
import mpmath
import time


def improve_a_constaruction_heuristic(path):
    
    ###############reading TSP document##################################
    def nearest_neighbor_for_TPS(path):
        ###################function for reading the tps document########################33
        def read_tps(path):
         with open(path, 'r') as f:
        		content = f.read().splitlines()#content is a list whose every element contains every line
                                               #line of the txt document
        		cleaned = [x.lstrip() for x in content if x != ""]#The gaps that exist at the start of every element
                                                                  #are removed
         for element in cleaned:#I find the line which strats with ΝΑΜΕ word and then
                                #I take the name of the problem
        		if element.startswith("NAME"):
        			 a=element
         if len(a.split())==2:#this happens when : is next to the word NAME         
           name=a.split()[1]
         elif len(a.split())==3:#this happens when there is a gap between ΝΑΜΕ and :
           name=a.split()[2]
         i=0
         flag=False
         while flag==False:
             if cleaned[i]=="NODE_COORD_SECTION" or cleaned[i]=="DISPLAY_DATA_SECTION":
                 loc=i#loc is the number of the row of the txt doument where it is written NODE_COORD_SECTION or
                      #NODE_COORD_SECTION and after this line there are the coordinates of the problem up to
                      #the line where it is written EOF
                 flag=True
             i=i+1
         cleaned1=[]#A list where I save the lines of the txt document which contain the coordinates after
                    #having applied split method to every line
         for i in range((loc+1),(len(cleaned)-1)):
            cleaned1.append(cleaned[i].split())
         for element in cleaned:#I find the line which strats with DIMENSION word and then
                                #I take the number after the DIMENSION 
        		if element.startswith("DIMENSION"):
        			 a=element
         if len(a.split())==2:#this happens when : is next to the word DIMENSION         
           dimension=int(a.split()[1])
         elif len(a.split())==3:#this happens when there is a gap between DIMENSION and :
           dimension=int(a.split()[2])
         for element in cleaned:
        		if element.startswith("EDGE_WEIGHT_TYPE"):
        			 a=element
         if len(a.split())==2:#this happens when : is next to EDGE_WEIGHT_TYPE    
           EDGE_WEIGHT_TYPE=a.split()[1]
         elif len(a.split())==3:#this happens when there is a gap between EDGE_WEIGHT_TYPE   and :
            EDGE_WEIGHT_TYPE=a.split()[2] 
         coordinates=[]
         coordinates1=[]
         for i in cleaned1:
             coordinates.append((int(i[0]),float(i[1]),float(i[2])))#coordinates is a list which contains the id
                                                                    #number and dimensions of every point
             coordinates1.append((float(i[1]),float(i[2])))#coordinates is a list which contains 
                                                           #the dimensions of every point
         #dimension=len(cleaned)-(loc+1)-1#A second way to find the dimensions of the problem
         
        ##########In the following lines I compute for every pair of points, the distance of them
        ##########and I create a nxn matrix which contains the distance of every 2 points of the problem
        ##########The type of the distance is given by the edge weight type
         
         if EDGE_WEIGHT_TYPE== "EUC_2D":
          ##EDGE_WEIGHT_TYPE : EUC_2D
          n=len(coordinates1)
          dist = np.zeros((n,n))
          for p in range(n):
           for q in range(p + 1, n):
              d = math.sqrt((coordinates1[p][0]-coordinates1[q][0])**2 + (coordinates1[p][1]-coordinates1[q][1])**2)
              dist[p][q] = d
              dist[q][p] = d
        
         if EDGE_WEIGHT_TYPE== "ATT":      
          #EDGE_WEIGHT_TYPE : ATT
          ###ATT distance
          n=len(coordinates1)
          dist= np.zeros((n,n))
          for p in range(n):
            for q in range(p + 1, n):
              d1 = math.sqrt(((coordinates1[p][0]-coordinates1[q][0])**2 + (coordinates1[p][1]-coordinates1[q][1])**2)/10)
              d2=mpmath.nint(d1)
              if d2<d1:
                  d=d2+1
              else:
                  d=d2
              dist[p][q] = d
              dist[q][p] = d
            
         if EDGE_WEIGHT_TYPE== "GEO":   
            #EDGE_WEIGHT_TYPE : GEO
          n=len(coordinates1)
          dist = np.zeros((n,n))
          for p in range(n):
           for q in range(p + 1, n):
              PI = math.pi;
              deg =mpmath.nint(coordinates1[p][0]);
              min = coordinates1[p][0] - deg;
              latitude_p= PI * (deg + 5.0 * min / 3.0 ) / 180.0
              deg = mpmath.nint( coordinates1[p][1] );
              min =  coordinates1[p][1] - deg;
              longitude_p = PI * (deg + 5.0 * min / 3.0 ) / 180.0
              deg = mpmath.nint(coordinates1[q][0]);
              min = coordinates1[q][0] - deg;
              latitude_q= PI * (deg + 5.0 * min / 3.0 ) / 180.0
              deg = mpmath.nint( coordinates1[q][1] );
              min =  coordinates1[q][1] - deg;
              longitude_q = PI * (deg + 5.0 * min / 3.0 ) / 180.0
              RRR = 6378.388;
              q1 = math.cos( longitude_p - longitude_q );
              q2 = math.cos( latitude_p - latitude_q );
              q3 = math.cos( latitude_p + latitude_q );
              d = int(( RRR * math.acos( 0.5*((1.0+q1)*q2 - (1.0-q1)*q3) ) + 1.0))
              dist[p][q] = d
              dist[q][p] = d
        
         return cleaned,cleaned1,dimension,coordinates1,coordinates,EDGE_WEIGHT_TYPE,dist,name
        ######################################################################################
        
        
      
        ##loading the tps document
        file_read=read_tps(path) 
        coordinates=file_read[3]
        dimension=file_read[2]
        name=file_read[7]
        print("The name for the TSP problem is:",name)
        print("\n")
        dist=file_read[6]
        ###############dist is the matrix which contains the distances between all the pairs of nodes
        EDGE_WEIGHT_TYPE=file_read[5]
        
        ############################Nearest Neighbor algorithm###########################
        def nearest_neighbor(dist_matrix):
            n=dist_matrix.shape[0]#the total number of the nodes of the problem
            start=random.choice(list(range(n)))#random selection between all the nodes for the starting node
            not_visited=list(range(n))#at first all the nodes are not visited, so at first I create
            #the not_visited list as a sequence of all nodes of the problem
            path=[start]#the first node of the path is the initial node-start
            not_visited=[ele for ele in not_visited if ele not in path]#taking out from not_visited list
            #the elemnts of the path, beacause they are already visited
            cost=0
            #the repetition continues until the no_visited list is empty(its length is zero), 
            #which means that every node of the problem is visited
            while len(not_visited)>0:
             last=path[-1]#last is the last visited node in the path
             next_loc_index=np.argmin(dist_matrix[last][not_visited])
             #the index for the next node that will be visited is computed by finding which node from the
             #nodes that are not visited has the minimum distance from the last visited node.
             #next_loc_index contains the index for that node
             next_loc=not_visited[next_loc_index]#next_loc is the next node that will be visited
             min_dist=dist_matrix[last][next_loc]#minimum distance from the previous visited node to the next
             #viisited node
             path.append(next_loc)#adding the current visited node to  the end of the path list
             not_visited=[ele for ele in not_visited if ele not in path]#taking out of the not_visited lsit
             #the node that is just visited and added to the path list 
             cost+=min_dist#each time the minimum distance from the previous visited node to the next
             #viisited node is added to cost
            cost+=dist_matrix[path[-1]][start]# at the end the distance from the last visited node to the
            #initial node is added to the cost as the process is cyclical and we should 
            #return to the start node
            path.append(start)
            return path, cost,start
        nn_alg=nearest_neighbor(dist)
        path=nn_alg[0]
        cost=nn_alg[1]
        start=nn_alg[2]
        return  path, dist
        
        
    #sunarthsh gia na ginei swap se 2 theseis enos array
    def swap_positions(array, pos1, pos2): 
          array[pos1], array[pos2] = array[pos2], array[pos1] 
          return array
    number = input("Please enter the number of minutes you want to run the improvement heuristic:\n: ")
    print("\n")
    #number corresponds to the minutes that we run the improvement heuristic
    number=float(number)
    t_end = time.time() + 60 * number
    
   ###################################################################
   #H sunarthsh gia to improvement heuristic gia to TSP problem
    def improvement_heuristic(cost_mat, route1):
           #to route1 periexei ta stoixeia tou route1 , xwris to teluatio stoixeio tou
           #route1 pou einai idio me to arxiko, kathws exoume TSP
           route=[h for h in route1[:-1]]
           #h cost list periexei ta kosth apo tis diadoxikes katastaseis ths route
           cost_list=[]
           for i in range(len(route)-1):
               cost_list.append(cost_mat[route[i]][route[i+1]])
           cost_list.append(cost_mat[route[len(route)-1]][route[0]])
           cost_prev=sum(cost_list)#the initial cost of given route
           print("The initial route from the nearest neighbor algorithm is:", route)
           print("\n")
           print("The initial cost from the nearest neighbor algorithm is:",cost_prev)
           print("\n")
           #init_route=[i for i in route]
           new_cost_list=[i for i in cost_list]
           while time.time() < t_end:
            # h try_cost_list einai proswrinh sunarthsh me ta diadoxika kosth
            #pou antistoixoun sthn route pou prokuptei epeita apo
            #tis antimetatheseis twn i,j
            try_cost_list=[k for k in new_cost_list]
            length=len(route)
            i=random.choice(range(length))#i,j epilexontai tuxaia 
            j=random.choice(range(length))
            while i==j :
                j=random.choice(range(length))
           
           #parakatw akolouthoun oi katallhles ananewseis ths  try_cost_list
           #analoga me tis periptwseis twn antimetathesewn
           
            #ean i,j einai endoiamesoi deiktes gia antimetathesh dhladh den einai
            #isoi oute me 0 oute me len(route)-1
            
            if ((i>0) and (j>0) and (i<len(route)-1)  and (j<len(route)-1)) and i<j:
                try_cost_list[i-1]=cost_mat[route[i-1]][route[j]]
                try_cost_list[i]=cost_mat[route[j]][route[i+1]]
                try_cost_list[j-1]=cost_mat[route[j-1]][route[i]]
                try_cost_list[j]=cost_mat[route[i]][route[j+1]]
            if ((i>0) and (j>0) and (i<len(route)-1)  and (j<len(route)-1)) and j<i:
                try_cost_list[j-1]=cost_mat[route[j-1]][route[i]]
                try_cost_list[j]=cost_mat[route[i]][route[j+1]]
                try_cost_list[i-1]=cost_mat[route[i-1]][route[j]]
                try_cost_list[i]=cost_mat[route[j]][route[i+1]]  
            
            elif i==0 : ##antimetathetw to prwto stoixeio ths route
               
                if j<len(route)-1: #me kapoio endoiameso ginetai h antimetathesi tou prwtou
                #stoixeiou
                  try_cost_list[0]=cost_mat[route[j]][route[1]]
                  try_cost_list[j-1]=cost_mat[route[j-1]][route[0]]
                  try_cost_list[j]=cost_mat[route[0]][route[j+1]] 
                  try_cost_list[len(route)-1]=cost_mat[route[len(route)-2]][route[j]]
                if j==len(route)-1  :#antimeathetw to prwto me to teleutaio
                #stoixeio ths route
                  try_cost_list[0]=cost_mat[route[j]][route[1]]                
                  try_cost_list[j-1]=cost_mat[route[j-1]][route[0]]
            elif j==0:#j<i antimetathesh ki pali tou prwtou stoixeiou
                if i<len(route)-1:#me kapoio endoiameso
                  try_cost_list[0]=cost_mat[route[i]][route[1]]
                  try_cost_list[i-1]=cost_mat[route[i-1]][route[0]]
                  try_cost_list[i]=cost_mat[route[0]][route[i+1]] 
                  try_cost_list[len(route)-1]=cost_mat[route[len(route)-2]][route[i]]
                if i==len(route)-1  :#antimetathesi tou prwtou me to
                #teleutaio stoixeio ths route
                  try_cost_list[0]=cost_mat[route[i]][route[1]]                
                  try_cost_list[i-1]=cost_mat[route[i-1]][route[0]]
            elif j==len(route)-1:#antimetathetw to teleutaio stoixeio ths route
                if i==0:#antimetathetw to teleutaio stoixeio me to prwto, j>i
                    try_cost_list[0]=cost_mat[route[j]][route[1]]
                    try_cost_list[j-1]=cost_mat[route[j-1]][route[0]]  
                else:#antimetathetw to teleutaio me kapoio endoiameso
                    try_cost_list[j-1]=cost_mat[route[j-1]][route[i]]  
                    try_cost_list[len(route)-1]=cost_mat[route[i]][route[0]] 
                    try_cost_list[i-1]=cost_mat[route[i-1]][route[j]]
                    try_cost_list[i]=cost_mat[route[j]][route[i+1]]
            elif i==len(route)-1:#an i>j, antimetathesh teleutaiou stoixeiou ths route
                if j==0:#antimetathesi tou teleutaiou stoixeiou ths route me to prwto
                    try_cost_list[0]=cost_mat[route[i]][route[1]]
                    try_cost_list[i-1]=cost_mat[route[i-1]][route[0]]  
                else:#antimetathesi tou teleutaiou stoixeiou ths route me kapoio endoiameso
                    try_cost_list[i-1]=cost_mat[route[i-1]][route[j]]  
                    try_cost_list[len(route)-1]=cost_mat[route[j]][route[0]] 
                    try_cost_list[j-1]=cost_mat[route[j-1]][route[i]]
                    try_cost_list[j]=cost_mat[route[i]][route[j+1]]
            current_cost=sum(try_cost_list)#upologismos proswrinou kostous
            if current_cost<cost_prev:#an to proswrino k0stos mikrotero apo auto
            #pou eixame prin
                 route=[m for m in swap_positions(route, i, j)]#tote kane pragmati
                 #anrimetathesi twn stoixeiwn
                 cost_prev=current_cost
                 new_cost_list=[p for p in try_cost_list] 
           route_final=[w for w in route]  #h teliki lush  
           route_final.append(route[0])
           print("The new improved route from the improvement_heuristic is:",route_final)
           print("\n")
           print("The new improved cost is",cost_prev)
           print("\n")
           return route_final,cost_prev
    ##################applying nearest_neighbor_for_TPS function to TSP problems
    near_neigh=nearest_neighbor_for_TPS(path)
    #construction heuristic
    route1=np.array(near_neigh[0])
    cost_mat=near_neigh[1]  
    #################applying the improvement heuristic
    impr_heur=improvement_heuristic(cost_mat, route1) 
    route_final=impr_heur[0]
    cost_prev=impr_heur[1]
    return  route_final,cost_prev

#dokimazw thn improve_a_constaruction_heuristic se kapoia arxeia tsp
improve_a_constaruction_heuristic("C:/Users/mixmp/Desktop/computational optimazation/HW9 Balkoudi Michaela/burma14.txt")
improve_a_constaruction_heuristic("pr144.txt")


  