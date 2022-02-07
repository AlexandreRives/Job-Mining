### JobMining
Application for textual analysis of job offers

### DESCRIPTION

Within the framework of a "Text Mining" project, we created an RShiny application that exploits job offers from the data domain directly from the Pôle Emploi website. The objective of this application was to perform a textual analysis of the recovered data. For this, the API made available by Pôle Emploi allowed us to retrieve in real time the targeted job offers in order to get the necessary information to carry out the analysis using dynamic graphs.

### ARCHITECTURE

<img width="857" alt="Architecture" src="https://user-images.githubusercontent.com/35383273/152772363-ba08ca36-49d0-4a16-ae43-c6ab9d391bdb.png">

First of all, we connected our application to the API of Pôle Emploi to be able to retrieve job offers in real time.
The data is retrieved in JSON format and we store it in our database after having done all the cleaning processes beforehand.The database is updated each time the user loads new offers. 
 In our application we propose two analyses:
  - a descriptive analysis of the words in the corpus (summary of offers, map of offers and statistical analysis of the corpus)
  - a semantic analysis of the corpus

### USER GUIDE

Now, we are going to present you the functioning of the 4 different tabs of our application

## summary of offers

![resume](https://user-images.githubusercontent.com/35383273/152774517-8d37fca4-784d-4554-9090-600fe684f67c.PNG)

This page is dedicated to the visibility of the offers currently present in the database. Indeed, via a simple table we have displayed brief information on the job offers recovered.
A click on the number of the offer redirects us to the page of the offer on the Pôle Emploi website. 
The "Load data" button allows you to retrieve the new offers published on the Pôle Emploi website.
We have set the threshold of available offers to 500 for more readability in our analyses.

## map of offers
![carte](https://user-images.githubusercontent.com/35383273/152774545-794dbe46-b77a-421c-9633-e3f735bdd303.PNG)

The map of offers shows the distribution of offers according to the different geographical areas of France. By clicking on an offer we can see: the title, the type of contract, the experience and the required skills.

## statistical analysis of the corpus
![corpus](https://user-images.githubusercontent.com/35383273/152774570-4139642a-3f49-4ee7-b498-23c11803696c.PNG)
The corpus analysis includes: 
- KPIs that summarize a part of our corpus (number of offers, the most present offer in the database, the offers accessible to beginners"
- some descriptive statistics on the words of the corpus accompanied by 2 histograms. 
The first histogram shows how many documents contain a given word. This is represented in the form of a ranking and the size of the top is configurable. By default we have a top 10 but we can go up to 25.
The second graph shows the frequency of appearance of the selected words according to 3 different axes (type of contract, experience level and status). The number of words that can be selected is limited to 5 so as not to overload the graph.

## semantic analysis of the corpus
![analyse](https://user-images.githubusercontent.com/35383273/152774594-8f3b364a-b975-441e-89dc-7461f7e3b2d6.PNG)
For the semantic analysis of our corpus, we used a LSA (Latent semantic analysis) which allows us to discover the hidden and latent semantics of our offers. This allows us to project our offers and terms into a new space.
The LSA allowed us to highlight the three main themes present in our corpus, namely: Cloud, Management and Analysis
