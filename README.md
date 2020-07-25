# Word-Predicting-Web-Application  
As part of the Data Science Specialization by John Hopkins on Coursera, this Caspstone Project allows us to showcase our skills learned throughout the specialization by developing a data product in the form of a Word Predicting Web Application using ShinyR.  

## About the files uploaded  
### Creating_the_corpus.R  
An R code for cleaning and filtering the corpus provided to us. It creates data frames containing the N-grams(from uni-gram to 5-grams) from the corpus which would be used in the downstream tasks.  

### app.R  
Code for creating the Web Application using ShinyR.  

### Model.R  
To keep the app efficient a simple model of **“Stupid Backoff”** was implemented to predict the next word which just calculates the probability of the next word given a set of words.  
I have implemented a 5-gram model to predict the next word and if the entered word length is less than four then lower-grams models were implemented. The code for the same is given in this file.  

### final_report.Rmd  
The Final Presentation Summarizing the analysis and the results of the capstone project.  

### Milestone-Report.Rmd  
The milestone report briefing the exploratory analysis and formation of N-grams from the corpus.  
