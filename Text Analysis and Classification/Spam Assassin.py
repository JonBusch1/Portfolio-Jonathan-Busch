# -*- coding: utf-8 -*-
"""
Created on Tue Apr 16 14:27:43 2024

@author: jonat
"""

#SPam Assissin Stuff

import numpy as np 
import pandas as pd 
import seaborn as sns 
import matplotlib.pyplot as plt
import os
from sklearn.feature_extraction.text import TfidfVectorizer
import matplotlib as mpl 
import matplotlib.pyplot as plt
from sklearn.model_selection import StratifiedShuffleSplit
from sklearn.model_selection import cross_val_score, cross_val_predict
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import RandomizedSearchCV
from sklearn.metrics import confusion_matrix
from sklearn.metrics import precision_score, recall_score

emailSpam=pd.read_csv("C:/Users/jonat/OneDrive/Desktop/TTU Stuff/Spring/Cyber/Project/spam_assassin.csv")
print(emailSpam.head())

print(emailSpam.shape)



sss = StratifiedShuffleSplit(n_splits=1, test_size=0.2, random_state=0)

for train_index, test_index in sss.split(emailSpam.text, emailSpam.target):
    train_X, test_X = emailSpam.text.loc[train_index], emailSpam.text.loc[test_index]
    train_y, test_y = emailSpam.target.loc[train_index], emailSpam.target.loc[test_index]
    
vect = TfidfVectorizer(min_df=5, ngram_range=(1,3)).fit(train_X)
X_train_vectorized = vect.transform(train_X)

def add_feature(X, feature_to_add):
   
    from scipy.sparse import csr_matrix, hstack
    return hstack([X, csr_matrix(feature_to_add).T], 'csr')
 
 

add_length=train_X.str.len()
add_digits=train_X.str.count(r'\d')
add_dollars=train_X.str.count(r'\$')
add_characters=train_X.str.count(r'\W')
 
X_train_transformed = add_feature(X_train_vectorized , [add_length, add_digits,  add_dollars, add_characters])
 

add_length_t=test_X.str.len()
add_digits_t=test_X.str.count(r'\d')
add_dollars_t=test_X.str.count(r'\$')
add_characters_t=test_X.str.count(r'\W')
 
 
X_test_transformed = add_feature(vect.transform(test_X), [add_length_t, add_digits_t,  add_dollars_t, add_characters_t])

forest_clf = RandomForestClassifier(random_state = 42)
y_probas_forest = cross_val_predict(forest_clf , X_train_transformed , train_y , cv = 3 , method = 'predict_proba')


param_grid = [
    {'n_estimators' : [3, 10, 30], 'max_features' : [2, 4, 6, 8]},
    {'bootstrap' : [False], 'n_estimators' : [3, 10], 'max_features' : [2, 3, 4]},
    
]

forest_reg = RandomForestClassifier()

random_search = RandomizedSearchCV(forest_reg, param_grid, cv = 5,
                          scoring = 'neg_mean_squared_error',
                          return_train_score = True)

random_search.fit(X_train_transformed , train_y)

random_search.best_params_

random_search.best_estimator_

final_model = random_search.best_estimator_
final_model.fit(X_train_transformed, train_y)

predictions = final_model.predict(X_test_transformed)
from sklearn.metrics import accuracy_score
print(accuracy_score(predictions, test_y))

cm=confusion_matrix(emailSpam.target.loc[test_index],predictions)

print(cm)

precision = precision_score(emailSpam.target.loc[test_index],predictions, average='weighted')  
recall = recall_score(emailSpam.target.loc[test_index],predictions, average='weighted')  

print('The precision is',precision)
print("The recall is",recall)



#Getting most frequent words in Spam
import pandas as pd
from collections import Counter

# Sample DataFrame



# Tokenize the text and count frequencies
word_counts = Counter()
for text in emailSpam.loc[emailSpam.target==1].text:
    words = text.split()
    word_counts.update(words)

# Sort the frequencies in descending order
sorted_word_counts = sorted(word_counts.items(), key=lambda x: x[1], reverse=False)

# Print the sorted word counts
#for word, count in sorted_word_counts:
    #print(f"{word}: {count}")


sorted_word_counts[-20:]


#Non spam
word_counts = Counter()
for text in emailSpam.loc[emailSpam.target==0].text:
    words = text.split()
    word_counts.update(words)

# Sort the frequencies in descending order
sorted_word_counts_Non = sorted(word_counts.items(), key=lambda x: x[1], reverse=False)

sorted_word_counts_Non[-20:]

#total
word_counts = Counter()
for text in emailSpam.text:
    words = text.split()
    word_counts.update(words)

# Sort the frequencies in descending order
sorted_word_counts_tot = sorted(word_counts.items(), key=lambda x: x[1], reverse=False)
sorted_word_counts_tot[-20:]

#SPam
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_style("darkgrid")
# Data
word_freq = sorted_word_counts[-20:]

# Convert to DataFrame
df = pd.DataFrame(word_freq, columns=['Word', 'Frequency'])

# Plot
plt.figure(figsize=(10, 6))
mpl.rcParams['font.size'] = 16
sns.barplot(data=df, x='Word', y='Frequency', palette='magma',edgecolor='darkgrey')
plt.title('Word Frequencies')
plt.xlabel('Word')
plt.ylabel('Frequency')
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()


#SPam
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_style("darkgrid")
# Data
word_freq = sorted_word_counts_Non[-20:]

# Convert to DataFrame
df = pd.DataFrame(word_freq, columns=['Word', 'Frequency'])

# Plot
plt.figure(figsize=(10, 6))
mpl.rcParams['font.size'] = 16
sns.barplot(data=df, x='Word', y='Frequency', palette='magma',edgecolor='darkgrey')
plt.title('Word Frequencies')
plt.xlabel('Word')
plt.ylabel('Frequency')
plt.xticks(rotation=45)
plt.tight_layout()
plt.show()







