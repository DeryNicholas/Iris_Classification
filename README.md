# Iris_Classification
## Overview
This project performs a classification analysis of the Iris dataset. The Iris dataset contains measurements or Sepal Length, Sepal Width, Petal length, Petal Width for three species of Iris flowers (Iris-setosa,Iris-versicolor and Iris-virginical).The goal is to build a model that can accurately predict the species of an Iris flower based on its measurements.

## Data
The Iris dataset is a well-known dataset in the field of machine learning.The 'iris.csv'file contains the following columns:
'Id': Unique identifier
'SepalLengthCm': Sepal length in centimeters
'SepalWidthCm' : Sepal width in centimeters
'PetalLengthCm' :Petal length in centimeters
'PetalWidthCm' : Petal width in centimeters
'Species'      : Species(target variable)

The data is stored in the 'data/iris.csv' file.


## Code
The analysis and modeling are performed in the 'notebookks/Iris_data.ipynb Jupyter Notebook.
The notebook covers the following steps:

1.**Importing Libraries:**
Loading necessary python libraries(pandas,scikit-learn,matplotlib, seaborn).
2.**Loading and viewing Data:**
Reading the Iris dataset and displaying its first few rows,information and shape.
3.**Exploratory Data Analysis(EDA):**
Calculating descriptive statistics,checking the distribution of species, checking missing data and duplicates, visualize outliers using boxplots and feature relationships using pairplots.
4.**Outliers Removal:**
Removing identified outliers from the data.
5.**Data preprocessing:**
Dropping the ID column.
6.**Model Training:**
Splits the data into training and testing sets
7.**Building the model:**
Building the model using RandomForestClassifier 
8.**Model Evaluation:**
Evaluating the model performance using a classification report and accuracy

## Dependencies
The following python packages are required to run the code:
pandas
matplotlib
seaborn
scikit-learn


