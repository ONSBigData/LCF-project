
SOLR server configuration 

1.Install SOLR on a Ubuntu 14.04 LTS

1.1 Install Java 8 or later

    sudo add-apt-repository ppa:webupd8team/java	
    sudo apt-get update
    sudo apt-get upgrade	
    sudo apt-get install oracle-java8-installer	
	
    sudo apt-get install oracle-java8-set-default	
	
    #Edit .bashrc and add if not there :	
    JAVA_HOME=<jdk-install-dir>	
    export JAVA_HOME	
    PATH=$JAVA_HOME/bin:$PATH	
    export PATH	
    and then run:  source .bashrc	
	
1.2 install solr from apache mirror
	
    cd ~	
    wget https://www.apache.org/dist/lucene/solr/6.3.0/solr-6.3.0.tgz	
    tar xzf solr-6.3.0.tgz solr-6.3.0/bin/install_solr_service.sh --strip-components=2	
    sudo bash ./install_solr_service.sh solr-6.3.0.tgz	
	
    sudo service solr status	
	

everything should be working and solr should be on localhost:8983	


-------------------------------------------------------------------------


2.Configure SOLR


Now that SOLR is running there are 3 config files that should be edited to allow for customisation on our end.


#solr.xml 

#conf/solrconfig.xml  

The CSV request handler needs to be configured in solrconfig.xml. This should already be present in the example solrconfig.xml 

 	<!-- CSV update handler, loaded on demand -->
  	<requestHandler name="/update/csv" class="solr.CSVRequestHandler" startup="lazy">
  	</requestHandler>

#conf/schema.xml   (a copy of the one used is attached below).

 schema.xml firstly describes various types used in SOLR and then defines the kind of schema we will use. 
We need a unique key (a line number for example like below) and the simplest type we can use is text.
However there are plenty of types defined in the schema.xml file and they might add functionality if used.


	<fields><!---->
	<field name="line" type="string" indexed="true" stored="true" required="true"/>
	<field name="EXPDESC" type="text" indexed="true" stored="true"/>
	<field name="CODING" type="text" indexed="true" stored="true"/>
	<field name="FinCode" type="text" indexed="true" stored="true"/>
	<field name="CodeDesc" type="text" indexed="true" stored="true"/>
	<field name="Paid1" type="text" indexed="true" stored="true"/>
	<field name="Quantity" type="text" indexed="true" stored="true"/>
	<field name="Units" type="text" indexed="true" stored="true"/>
	</fields>
	<uniqueKey>line</uniqueKey></schema>

-------------------------------------------------------------------------


3. Import data in SOLR by streaming CSV file with schema defined on schema.xml


Send an HTTP request of the form: (http://) 
	 localhost:8080/solr/update/csv?commit=true&stream.file=/home/bigdata/data/nsmLCF.csv

where stream.file can be any valid URL/URI (in the case above it was a local file however a url could be at its place).


If there is any problem on the csv (ie if it doesnt follow the schema) expect a lot of errors here!!!!


-------------------------------------------------------------------------


4. Query data in SOLR 


an example R script that accesses SOLR is included above. Below the code shows a query that takes a string , replaces all punctuation and space chars with "+" and then gets 10000 records in csv format. 


solrdf<- solr_search(q=paste0('CodeDesc:',str_replace_all(input$text,"([[:punct:]])|\\s+","+")), rows=10000,wt='csv' )


there are a lot of options to be explored here .the manual at this point is pretty helpful about the standard query parser and other parsers available:

https://cwiki.apache.org/confluence/display/solr/The+Standard+Query+Parser

* add info about score




QUERY via JSON with JSON request API


http://yonik.com/solr-json-request-api/


-------------------------------------------------------------------------

5. Example of query in SOLR via a REST web interface (or via a URL for non-techies!) 

to sum up:
We provide a query like this

http://localhost:8983/solr/techproducts/select?q=id:SP2514N

get a reply like this (in XML)


	<?xml version="1.0" encoding="UTF-8"?>
	<response>
	<responseHeader><status>0</status><QTime>1</QTime></responseHeader>
	<result numFound="1" start="0">
 	<doc>
  	<arr name="cat"><str>electronics</str><str>hard drive</str></arr>
  	<arr name="features"><str>7200RPM, 8MB cache, IDE Ultra ATA-133</str>
    	<str>NoiseGuard, SilentSeek technology, Fluid Dynamic Bearing (FDB) motor</str></arr>
  	<str name="id">SP2514N</str>
  	<bool name="inStock">true</bool>
  	<str name="manu">Samsung Electronics Co. Ltd.</str>
 	 <str name="name">Samsung SpinPoint P120 SP2514N - hard drive - 250 GB - ATA-133</str>
  	<int name="popularity">6</int>
  	<float name="price">92.0</float>
  	<str name="sku">SP2514N</str>
 	</doc>
	</result>
	</response>




or another example using JSON request and getting JSON reply:


	$ curl http://localhost:8983/solr/query -d '
	{
	  query:"hero"
	}'
	RESPONSE:{
	  "responseHeader":{
	    "status":0,
	    "QTime":2,
	    "params":{
	      "json":"n{n  query:"hero"n}"}},
	  "response":{"numFound":1,"start":0,"docs":[
	      {
		"id":"book3",
		"author":"Brandon Sanderson",
		"author_s":"Brandon Sanderson",
		"title":["The Hero of Aages"],
		"series_s":"Mistborn",
		"sequence_i":3,
		"genre_s":"fantasy",
		"_version_":1486581355536973824
	      }]
	  }
	}




7. SOLR relevancy function queries


Lucene’s default ranking function uses factors such as tf, idf, and norm to help calculate relevancy scores.

Solr has now exposed these factors as function queries.

	docfreq(field,term) returns the number of documents that contain the term in the field.
	termfreq(field,term) returns the number of times the term appears in the field for that document.
	idf(field,term) returns the inverse document frequency for the given term, using the Similarityfor the field.
	tf(field,term) returns the term frequency factor for the given term, using the Similarity for the field.
	norm(field) returns the “norm” stored in the index, the product of the index time boost and then length normalization factor.
	maxdoc() returns the number of documents in the index, including those that are marked as deleted but have not yet been purged.
	numdocs() returns the number of documents in the index, not including those that are marked as deleted but have not yet been purged.



8. SOLR Scoring exact matches higher than partial matches

see here:

http://stackoverflow.com/questions/26365002/solr-scoring-exact-matches-higher-than-partial-matches


PS. Need to apply these before importing any new files for setting correctly the solr.xml schema , STILL TODO.



9. New file imported. Steps




# to delete index

	   curl http://localhost:8080/solr/update -H "Content-type: text/xml" \
	   --data-binary '<delete><query>*:*</query></delete>'

	   curl http://localhost:8080/solr/update -H "Content-type: text/xml" \
	   --data-binary '<commit />'

	   curl http://localhost:8080/solr/update -H "Content-type: text/xml" \
	   --data-binary '<optimize />'





The schema.xml this time looks like this. 


	<fields>

	<field name="line" type="string" indexed="true" stored="true" required="true"/>
	<field name="coicop" type="integer" indexed="true" stored="true"/>
	<field name="EXPDESC" type="text" indexed="true" stored="true"/>
	<field name="Paid1" type="float" indexed="true" stored="true"/>
	<field name="Shop" type="text" indexed="true" stored="true"/>
	<field name="MAFFQuan" type="float" indexed="true" stored="true"/>
	<field name="MAFFUnit" type="text" indexed="true" stored="true"/>

	</fields>


