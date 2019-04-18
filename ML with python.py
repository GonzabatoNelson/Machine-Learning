# Required Libraries
# scipy numpy matplotlib pandas sklearn
import pandas as pd
import numpy as np
import matplotlib as plt
from sklearn import model_selection
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC
from sklearn.metrics import classification_report
from sklearn.metrics import accuracy_score
from sklearn.metrics import confusion_matrix
from sklearn.naive_bayes import GaussianNB
from sklearn.linear_model import LogisticRegression

#Loading the data set
url = "https://raw.githubusercontent.com/jbrownlee/Datasets/master/iris.csv"
names = ['sepal-length', 'sepal-width', 'petal-length', 'petal-width', 'class']
dataset = pd.read_csv(url, names=names)
dataset.head(3)

# Simple EDA

dataset.shape

dataset.describe().head()
dataset.groupby("class").size()
dataset.groupby("class").apply(lambda x: len(x))

# Some summary stats

dataset.groupby("class").apply(lambda x: np.mean(x))

dataset.groupby("class").apply(lambda x: np.std(x) )

# Apply multiple functions with agg
dataset.dtypes
dataset.groupby("class").agg([np.sum,np.mean,np.std,np.max,np.min])
# What proprotion of the data set is each species
#dataset.groupby("class").apply(if type(x)=="object" lambda x: print(x))


 # define custom apply



# Some basic plots

dataset.plot(kind='box',subplots=True,sharex=False,sharey=False,layout=(2,2))
dataset.plot(kind="hist",subplots=True,sharex=False,sharey=False,layout=(2,2))
dataset.plot.box(subplots=True,sharex=False,layout=(2,2))
#plt.scatter(dataset.sepal-length,dataset.petal-length,c=dataset.class)
dataset.plot.hist(subplots=True,bins=50)
#multi_variate plots
from pandas.plotting import scatter_matrix
scatter_matrix(dataset,alpha=0.9,diagonal="hist")
#kernel denisty estimation
scatter_matrix(dataset,alpha=0.95 , diagonal="kde", grid=False)
# diagonal grouping suggests a high correlation
# Create a correlation matrix
data_correlations = dataset.corr(method="spearman")
# Create a figure to take our data
dataset.plot.scatter(x="sepal-length",y="petal-length",s=dataset["sepal-width"])
dataset.groupby("class").agg("sum").unstack(1).plot.bar(subplots=True)
# stacked bar
# rot controls rotation of our words
dataset.groupby("class").\
    agg(np.sum).plot.bar(stacked=False,rot=2.5).\
    set(title="A stacked bar plot coloured by Class",ylabel="Value")


# Create a validation data set:

array = dataset.values
X=array[:,0:4]
Y=array[:,4]
seed=233
validation_size=0.25
# Split the data set. 75% training

X_train,X_validation,Y_train,Y_validation=model_selection.train_test_split(X,Y,test_size=validation_size,
                                                              random_state=seed)

# Cross validation
seed = 233
scoring = "accuracy"

# Use logistic regression, linear discriminnat analysis, knn, cart, nb, svm
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
models=[]
models.append(("LDR",LogisticRegression(solver="liblinear",multi_class="ovr")))
models.append(("LDA",LinearDiscriminantAnalysis()))
models.append(("KNN",KNeighborsClassifier()))
models.append(("SVM",SVC(gamma="auto")))
models.append(("CART",DecisionTreeClassifier()))
models.append(("NB",GaussianNB()))
results=[]
names=[]
for name,model in models:
    kfold=model_selection.KFold(n_splits=10,random_state=seed)
    cv_results=model_selection.cross_val_score(model,X_train,Y_train,cv=kfold)
    results.append(cv_results)
    names.append(name)
    msg="%s: %f(%f)" % (name,cv_results.mean(),cv_results.std())
    print(msg)

# Run a KNN
knn= KNeighborsClassifier()
knn.fit(X_train,Y_train)
predictions=knn.predict(X_validation)
print(accuracy_score(Y_validation,predictions))
print(confusion_matrix(Y_validation,predictions))
print(classification_report(Y_validation,predictions))

# Predict with LDA
lda= LinearDiscriminantAnalysis()
lda.fit(X_train,Y_train)
predictions=lda.predict(X_validation)
print(accuracy_score(Y_validation,predictions))
print(confusion_matrix(Y_validation,predictions))
print(classification_report(Y_validation,predictions))