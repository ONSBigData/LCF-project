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

![FlatFileApp](https://github.com/ONSBigData/LCF-project/blob/master/FlatFiles.png "")  


<br></br>


## strand B: Using a SOLR-based indexing solution


<br></br>


https://github.com/ONSBigData/LCF-project/tree/master/LCF-shiny

![SOLRShinyApp](https://github.com/ONSBigData/LCF-project/blob/master/LCF-2a.png "")  


<br></br>


### - additional code: LCF Scanning Receipt Optical Character Recognition shiny app prototype


<br></br>


https://github.com/ONSBigData/LCF-project/tree/master/LCFshinyReceiptOCR

![OCRShinyApp](https://github.com/ONSBigData/LCF-project/blob/master/good.receipt.scan.png "")


<br></br>


 ### additional code: prototype COICOP Classification using scikit learn jupyter notebook

<br></br>

https://github.com/ONSBigData/LCF-project/tree/master/LCF-COICOPclassification


A jupyter notebook containing 3 types of scikit learn classifiers (machine learning algorithms) trained to 
automatically assign a COICOP code based on a product description.  

- Naive Bayes
- Support Vector Machines
- Random Forests

additionaly a jupyter notebook containing a Python implementation of the BM25 algorithm used in products such as Apache Lucene and SOLR

<br></br>


## Contributors

[Iva Spakulova](https://github.com/ivyONS)

[Theodore Manassis](https://github.com/mamonu)

[Alessandra Sozzi](https://github.com/AlessandraSozzi)

working for the [Office for National Statistics Big Data project](https://www.ons.gov.uk/aboutus/whatwedo/programmesandprojects/theonsbigdataproject)


## LICENSE

Released under the [GNU General Public License, version 3](LICENSE).
