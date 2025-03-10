{\rtf1\ansi\ansicpg1252\cocoartf1348\cocoasubrtf170
{\fonttbl\f0\fswiss\fcharset0 Helvetica;}
{\colortbl;\red255\green255\blue255;}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\pard\tx720\tx1440\tx2160\tx2880\tx3600\tx4320\tx5040\tx5760\tx6480\tx7200\tx7920\tx8640\pardirnatural

\f0\fs24 \cf0 April 24, 2016\
\
Description of the project:\
\
The purpose of this project is to demonstrate your ability to collect, work with, \
and clean a data set. The goal is to prepare tidy data that can be used for later \
analysis. You will be graded by your peers on a series of yes/no questions related \
to the project. You will be required to submit: 1) a tidy data set as described \
below, 2) a link to a Github repository with your script for performing the analysis, \
and 3) a code book that describes the variables, the data, and any transformations \
or work that you performed to clean up the data called CodeBook.md. You should \
also include a README.md in the repo with your scripts. This repo explains how \
all of the scripts work and how they are connected.\
\
One of the most exciting areas in all of data science right now is wearable \
computing - see for example this article . Companies like Fitbit, Nike, and \
Jawbone Up are racing to develop the most advanced algorithms to attract new users. \
The data linked to from the course website represent data collected from the \
accelerometers from the Samsung Galaxy S smartphone. A full description is \
available at the site where the data was obtained:\
        \
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones\
\
Here are the data for the project:\
        \
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip\
\
You should create one R script called run_analysis.R that does the following.\
\
Merges the training and the test sets to create one data set.\
Extracts only the measurements on the mean and standard deviation for each measurement.\
Uses descriptive activity names to name the activities in the data set\
Appropriately labels the data set with descriptive variable names.\
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.\
\
Explanation of the scripts:\
\
The run_analysis script includes the functions that are needed there. The steps to perform the analysis are:\
\
1. Download the data.\
2. Read the text files (creating the column separation)\
3. Read systematically the files, based on the variable names stored in features, and save the tables in a new folder\
4. \
\
}