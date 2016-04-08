
# coding: utf-8

# In[1]:

get_ipython().magic('matplotlib notebook')


# In[2]:

import sklearn as sk


# In[3]:

import numpy as np


# In[4]:

import matplotlib.pyplot as plt


# In[5]:

# Datasets
from sklearn import datasets
iris = datasets.load_iris()
X_iris, y_iris = iris.data, iris.target


# In[6]:

print(X_iris.shape, y_iris.shape) 


# In[7]:

# Select training set and test set
from sklearn.cross_validation import train_test_split
from sklearn import preprocessing
# Select the first two column
X, y = X_iris[:, :2], y_iris
# Test set will be 25% taken randomly
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.25, random_state = 33)
# Standardize features by removing the mean and scaling to unit variance (sklearn.preprocessing.StandardScaler())
scaler = preprocessing.StandardScaler().fit(X_train)
X_train = scaler.transform(X_train)
X_test = scaler.transform(X_test)


# In[16]:

# Plot Iris
colors = ['red', 'greenyellow', 'blue']
for i in range(len(colors)):
    xs = X_train[:, 0][y_train == i]
    ys = X_train[:, 1][y_train == i]
    plt.scatter(xs, ys, c = colors[i])
plt.legend(iris.target_names)
plt.xlabel('Sepal length')
plt.ylabel('sepal width')


# In[ ]:



