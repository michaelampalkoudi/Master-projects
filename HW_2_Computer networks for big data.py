# -*- coding: utf-8 -*-
"""
Created on Wed Apr 13 16:28:04 2022

@author: mixmp
"""

import csv
import numpy as np   
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

df = pd.read_csv ("trace.csv")
print (df)
print("The info of the network trace is:")
print("-------------------------------")
for i in list(df.columns):
   print(i)
   print("-------")


df_sub=df[(df['Protocol']=='UDP') | (df['Protocol']=='TCP')]     
print("There are",df_sub.shape[0],"packets for the protocols UDP and TCP")


split_info=df_sub["Info"].str.split( expand = True)
#subseting source ports
src_port=split_info[0]
#subsetting destination ports"
dest_port=split_info[2]


data_sub=df_sub.assign(src_port=list(src_port.values))
df_sub=data_sub.assign(dest_port=list(dest_port.values))


####################       Grouping packets per flow     ###########################

df_agg = (df_sub.groupby(by=['Source', 'Destination','src_port', 'dest_port','Protocol'],as_index=False)
          .agg({'Time':'min','Length':'sum'})
          .rename(columns={'Time':'minimum_time', 'Length':'flow_size'})
          )
print("There are", df_agg.shape[0],"flows in the trace")

max_time=pd.DataFrame(df_sub.groupby(['Source', 'Destination','src_port', 'dest_port','Protocol'])['Time'].agg(maximum='max')).reset_index()['maximum']

max_time=list(max_time.values)

df_agg=df_agg.assign(max_time=max_time)

difference_time=[]
for i in range(0,len(max_time)):
  if df_agg["max_time"][i]!=df_agg["minimum_time"][i]:
    difference_time.append(df_agg["max_time"][i]-df_agg["minimum_time"][i])   
  else:
    difference_time.append(df_agg["minimum_time"][i])   

df_agg=df_agg.assign(flow_duration=difference_time)


packets_per_flow=df_sub.groupby(['Source', 'Destination','src_port', 'dest_port','Protocol']).size().reset_index(name='counts')['counts']
df_agg=df_agg.assign(packets_per_flow=list(packets_per_flow.values))


print("Grouping the packets into flows resulted in the following:")
print("---------------------------------------------------------------")
print(df_agg)


############## cdf for the size of flows  ####################



flow_size_ar=list(df_agg['flow_size'].values)
plt.plot(np.sort(flow_size_ar), np.linspace(0, 1, len(flow_size_ar), endpoint=False))
#plt.xlim(0, 20000)
plt.ylabel('Cumulative Probability')
plt.xlabel('The size of flow in bytes')
plt.title("CDF of the size of flow")
plt.show()

# Density Plot
#sns.distplot(list(df_agg['flow_size'].values), hist=True, kde=True)



###################################################

############## cdf of the duration of flows###############
flow_dur=list(df_agg['flow_duration'].values)
plt.plot(np.sort(flow_dur), np.linspace(0, 1, len(flow_dur), endpoint=False))
#plt.xlim(0, 50)
plt.ylabel('Cumulative Probability')
plt.xlabel('The duration of flow in sec')
plt.title("CDF of the duration of flows")
plt.show()
######################################################





##################### finding the frequency of traffic per protocol#################

length_packets=[]
count_tcp=0
count_udp=0
count_icmp=0
count_arp=0
count_all=0
for i in range(0,len(df)):
    length_packets.append(df['Length'][i])
    if df['Protocol'][i]=="TCP":
        count_tcp=count_tcp+1
    elif df['Protocol'][i]=="UDP":
        count_udp=count_udp+1
    elif df['Protocol'][i]=="ICMP":    
        count_icmp=count_icmp+1
    elif  df['Protocol'][i]=="ARP":   
        count_arp=count_arp+1
    count_all=count_all+1    
        
    
    

print("Number of UDP packets:", count_udp)
print("====================================")
print(("Number of TCP packets:", count_tcp))
print("====================================")
print("Number of ARP packets:", count_arp)
print("====================================")
print(("Number of ICMP packets:", count_icmp))


#################### cdf of the size of all packets##############
plt.plot(np.sort(length_packets), np.linspace(0, 1, len(length_packets), endpoint=False))
plt.ylabel('Cumulative Probability')
#plt.xlim(0, 1700)
plt.xlabel('The size of packet in bytes')
plt.title("CDF of the size of packets")
plt.show()

x_m200=0
x_1486=0
for i in list(np.sort(length_packets)):
    if(i>200 and i<1486):
        x_m200=x_m200+1
    if(i==1486):
        x_1486=x_1486+1
#len(list(np.sort(length_packets)))
#x_1486/len(list(np.sort(length_packets)))

print("The frequency of the value 1486 for the size of packet is:",x_1486/len(list(np.sort(length_packets))))


################# barplot for the frequencies of traffic per protocol####################

freq_tcp=count_tcp/count_all
freq_udp=count_udp/count_all
freq_icmp=count_icmp/count_all
freq_arp=count_arp/count_all
 
 
# creating the dataset
data = pd.DataFrame({'UDP':freq_udp, 'ARP':freq_arp, 'ICMP':freq_icmp,
        'TCP':freq_tcp},index=[0])

protocols = list(data.keys())
values = list(data.values[0])
  

# function to add value labels
def addlabels(x,y):
	for i in range(len(x)):
		plt.text(i,y[i],y[i])

if __name__ == '__main__':
	# creating data on which bar chart will be plot
	x = protocols
	y = [round(i,ndigits=5) for i in values]
	
	# making the bar chart on the data
	plt.bar(x, y)
	
	# calling the function to add value labels
	addlabels(x, y)
	
	# giving title to the plot
	plt.title("Frequency of traffic per protocol")
	
	# giving X and Y labels
	plt.xlabel("Protocol")
	plt.ylabel("Frequency of traffic")
	
	# visualizing the plot
	plt.show()




