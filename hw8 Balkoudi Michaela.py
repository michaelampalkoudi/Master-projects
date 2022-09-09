# -*- coding: utf-8 -*-
"""
Created on Mon Dec  7 19:15:32 2020

@author: mixmp
"""

#ask for the dimension of the oroblem
import random
while True:
    number = input("Please enter the dimension of the problem:\n: ")
    try:
        dimension = int(number)
        if dimension < 0:  # if not a positive int print message and ask for input again
            print("Sorry, input must be a positive integer, try again")
            continue
        break
    except ValueError:
        print("That's not an int!")     
# else all is good, dimension is >=  0 and an integer

#ask for the upper and lower bounds of the range of the coordinates of the problem:
while True:
    low = input("Please enter the lower bound of the range of the coordinates of the problem:\n: ")  
    low=float(low) 
    up = input("Please enter the upper bound of the range of the coordinates of the problem:\n: ")
    up=float(up)
    if up<= low:  # if not a positive int print message and ask for input again
     print("Sorry,the lower bound of the range should be less than the upper bound of the range , try again")
     continue
    break
        
name=input("Please enter the name of the problem:\n: ")
comment=input("Please enter a comment of the problem:\n: ")
type="TSP"
EDGE_WEIGHT_TYPE=input("Please enter the edge weight type of the problem:\n: ")

#functions in order to have aligned coordinates in the txt document
def align(k, l,m):
    return "{:<10s}{:<10s}{:<10s}".format(k, l,m)

def align2(k, l,m,z):
    return "{:<10s}{:<10s}{:<10s}{:<10s}".format(k, l,m,z)

import os
os.chdir("C:/Users/mixmp/Desktop/computational optimazation/hw 8 balkoudi Michaela")

####write a txt document with EDGE_WEIGHT_TYPE 2D
f = open("distance2d.txt", "x")
f = open("distance2d.txt", "a")
f.write("NAME" + '  ' + name)
f.close()
with open("distance2d.txt", "a+") as file_object:

    file_object.seek(0)
    # If file is not empty then append '\n'
    data = file_object.read(1000)
    if len(data) > 0 :
        file_object.write("\n")
    # Append text at the end of file
    file_object.write("COMMENT:"+ '  ' + comment)
    file_object.write("\n")
    file_object.write("TYPE:"+ '  ' + type)
    file_object.write("\n")
    file_object.write("DIMENSION:"+ '  ' + str(dimension))
    file_object.write("\n")
    file_object.write("EDGE_WEIGHT_TYPE:"+ '  ' + EDGE_WEIGHT_TYPE)
    file_object.write("\n")
    file_object.write("NODE_COORD_SECTION")
    file_object.write("\n")
    for i in range(1,dimension+1):
        string= align(str(i),str(random.randrange(low,up)),str(random.randrange(low,up)))
        file_object.write(string)
        file_object.write("\n")
    file_object.write("EOF")
  
##write a txt document with EDGE_WEIGHT_TYPE 3D
f = open("distance3d.txt", "x")
f = open("distance3d.txt", "a")
f.write("NAME" + '  ' + name)
f.close()
with open("distance3d.txt", "a+") as file_object:

    file_object.seek(0)
    # If file is not empty then append '\n'
    data = file_object.read(1000)
    if len(data) > 0 :
        file_object.write("\n")
    # Append text at the end of file
    file_object.write("COMMENT:"+ '  ' + comment)
    file_object.write("\n")
    file_object.write("TYPE:"+ '  ' + type)
    file_object.write("\n")
    file_object.write("DIMENSION:"+ '  ' + str(dimension))
    file_object.write("\n")
    file_object.write("EDGE_WEIGHT_TYPE:"+ '  ' + "EUC_3D")
    file_object.write("\n")
    file_object.write("NODE_COORD_SECTION")
    file_object.write("\n")
    for i in range(1,dimension+1):
        string= align2(str(i),str(random.randrange(low,up)),str(random.randrange(low,up)),str(random.randrange(low,up)))
        file_object.write(string)
        file_object.write("\n")
    file_object.write("EOF")
 
