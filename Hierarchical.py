import pandas as pd
import matplotlib.pyplot as plt 
Univ = pd.read_csv("E:\\Datasets\\KmeansClustering\\Universities.csv")


# Normalization function 
def norm_func(i):
    x = (i-i.min())	/	(i.max()	-	i.min())
    return (x)

# alternative normalization function 

#def norm_func(i):
 #   x = (i-i.mean())/(i.std())
 #   return (x)


# Normalized data frame (considering the numerical part of data)
df_norm = norm_func(Univ.iloc[:,1:])
df_norm.describe()


from scipy.cluster.hierarchy import linkage 
import scipy.cluster.hierarchy as sch # for creating dendrogram 

#p = np.array(df_norm) # converting into numpy array format 
help(linkage)
z = linkage(df_norm, method="complete",metric="euclidean")


plt.figure(figsize=(15, 5));plt.title('Hierarchical Clustering Dendrogram');plt.xlabel('Index');plt.ylabel('Distance')
sch.dendrogram(
    z,
    leaf_rotation=0.,  # rotates the x axis labels
    leaf_font_size=8.,  # font size for the x axis labels
)
plt.show()

# Now applying AgglomerativeClustering choosing 3 as clusters from the dendrogram
from sklearn.cluster import	AgglomerativeClustering 
h_complete	=	AgglomerativeClustering(n_clusters=3,linkage='complete',affinity = "euclidean").fit(df_norm) 

h_complete.labels_

cluster_labels=pd.Series(h_complete.labels_)

Univ['clust']=cluster_labels # creating a  new column and assigning it to new column 
Univ = Univ.iloc[:,[7,0,1,2,3,4,5,6]]
Univ.head()

# getting aggregate mean of each cluster
Univ.groupby(Univ.clust).mean()

# creating a csv file 
Univ.to_csv("University.csv",index=False) #,encoding="utf-8")
