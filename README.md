# LCF-Project


<br></br>

Living Costs and Food Survey (LCF) Project repository

<br></br>


# Overview


<br></br>


The ONS Big Data team was approached by ONS Social Survey Division (SSD) about the possibility of using 
commercial data and/or data science methods to help improve the processing of the 
Living Costs and Food Survey (LCF).


![diaryprocess](https://github.com/ONSBigData/LCF-project/blob/master/LCFDiaryProcess.png "")  

     
In order to facilitate the LCF diary process, two prototypes were developed by the Big Data Team in consultation with 
the Social Survey Division, Surveys and Life Events Processing and the end user DEFRA. 

The proposed solutions harness information from clean historic LCF diary data to help complete 
missing product quantity information (i.e. amount, volume or weight purchased) at the point of data entry.   
     
     
<br></br>


## strand A: Using historical data to create a lookup    

<br></br>
    
https://github.com/ONSBigData/LCF-project/tree/master/LCF-analysis


Entering LCF data from diaries into the database takes a significant amount of time. Currently it is done in a system called Blaise 
and the most resource intensive part is the amount (weight) information retrieval as it is often missing in the diary / on the receipt.

Although the customer (DEFRA) only requires amounts to be completed for half of the survey respondents, the additional
time taken to find the correct amounts (usually via internet searches outside of Blaise) is a large 
contributing factor in diary processing delays.

A solution which could integrate easily into the current system and coders’ work flow was piloted using flat look-up functions 
already available in Blaise. The goal was to give the coder an option to choose an amount from a list of matching 
or very similar items previously entered within the Blaise environment
(eliminating the need for an internet search on different machine or browser).



![FlatFileApp](https://github.com/ONSBigData/LCF-project/blob/master/FlatFiles.png "")  


The picture above shows a summary of the data processing pipeline for the flat-file prototype.
The prepared lists get exported into a CSV file and handed over to the Blaise team,
who convert them into a (proprietary) format suitable for loading from within the questionnaire. 

Each look-up file still contains a lot of items and therefore the items’ ordering is important. 
When the look-up file opens in Blaise, the position of the cursor needs to be such that the next few products 
are the most similar to what the coder is looking for.

This has been achieved by a modified K-Nearest Neighbour classifcation algorithm.


<br></br>


## strand B: Using a SOLR-based indexing solution


<br></br>


https://github.com/ONSBigData/LCF-project/tree/master/LCF-shiny

![SOLRShinyApp](https://github.com/ONSBigData/LCF-project/blob/master/LCF-2a.png "")  


<br></br>


### * additional code: LCF Scanning Receipt Optical Character Recognition shiny app prototype


<br></br>


https://github.com/ONSBigData/LCF-project/tree/master/LCFshinyReceiptOCR

![OCRShinyApp](https://github.com/ONSBigData/LCF-project/blob/master/good.receipt.scan.png "")


<br></br>


### * additional code: prototype COICOP Classification using scikit learn jupyter notebook

<br></br>

https://github.com/ONSBigData/LCF-project/tree/master/LCF-COICOPclassification


A jupyter notebook containing 3 types of scikit learn classifiers (machine learning algorithms) trained to 
automatically assign a COICOP code based on a product description.  

- Naive Bayes
- Support Vector Machines
- Random Forests

Additionaly a jupyter notebook containing a Python implementation of the BM25 algorithm used in products such as Apache Lucene and SOLR

<br></br>


## Contributors

[Iva Spakulova](https://github.com/ivyONS)

[Theodore Manassis](https://github.com/mamonu)

[Alessandra Sozzi](https://github.com/AlessandraSozzi)

working for the [Office for National Statistics Big Data project](https://www.ons.gov.uk/aboutus/whatwedo/programmesandprojects/theonsbigdataproject)


## LICENSE

Released under the [GNU General Public License, version 3](LICENSE).
