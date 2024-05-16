# -*- coding: utf-8 -*-
"""
Created on Tue Apr 16 13:02:41 2024

@author: jonat
"""

#Project Big Data Security 

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestClassifier
from sklearn.datasets import make_classification
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.metrics import precision_score, recall_score


PhishingDataFrame=pd.read_csv("C:/Users/jonat/OneDrive/Desktop/TTU Stuff/Spring/Cyber/Project/Phishing_Legitimate_full.csv")

correlations=PhishingDataFrame.corr()




# Creating train and test splits with data x and label y


xVariables=PhishingDataFrame.drop(columns=["id","HttpsInHostname","CLASS_LABEL"])
label=PhishingDataFrame.loc[:,["CLASS_LABEL"]]


#Calculating % split of data that are labeled phishing and not phishing

countPhish=PhishingDataFrame["CLASS_LABEL"].value_counts().get(1,0)
print(countPhish)



#NOT GONNA USE THIS LMAO
#Visualization
plt.pie([5000,5000],labels=["Phishing","Safe"])

# Adding labels and title

plt.title('Pie')

# Displaying the plot
plt.show()

#Visualization Attempt 2

















# Step 1: Split the data into training and test sets
X_train, X_test, y_train, y_test = train_test_split(xVariables, label, test_size=0.2, random_state=42)

# Create a Random Forest classifier
clf = RandomForestClassifier(n_estimators=100, random_state=42)

# Train the classifier
clf.fit(X_train, y_train)

# Predict on the test set
y_pred = clf.predict(X_test)

# Evaluate accuracy
accuracy = accuracy_score(y_test, y_pred)
print("Accuracy:", accuracy)

precision = precision_score(y_test, y_pred, average='weighted')  
recall = recall_score(y_test, y_pred, average='weighted')  

print("Precision:",precision)
print("Recall:",recall)

