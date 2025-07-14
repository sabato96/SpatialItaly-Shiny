# SpatialItaly
Analysis of spatial distribution of income in Italy

<img width="1915" height="953" alt="plotly_map" src="https://github.com/user-attachments/assets/d19fefbe-d1ee-44da-85cd-e4e777b98b98" />


Il divario economico tra Nord e Sud Italia è fin dal 1861 un problema sociale ma soprattutto economico che si ripercuote sulla qualità della vita di milioni di persone. Secondo i dati dell'ISTAT il Mezzogiorno presenta un ampio differenziale di crescita rispetto al Nord. Nel corso degli anni molti studi hanno provato a indetificare le cause di questo divario, che rimangono tutt'oggi difficili da individuare con precisione. Ciò che possiamo sicuramente asserire è che una delle conseguenze di questo divario è l'assenza, al Sud, di un tessuto industriale strutturato e di conseguenza il mercato del lavoro è profondamente diverso da quello del resto d'Italia, condannando i giovani, in particolare i meridionali, ad un futuro incerto e precario dal punto di vista lavorativo. 


Questo lavoro si pone l’obiettivo di studiare la distribuzione spaziale delle varie tipologie di reddito sul territorio italiano, in particolare verranno utilizzati metodi propri dell’analisi spaziale al fine di valutare se ci sono zone dello stivale in cui le variabili osservate presentano autocorrelazione spaziale o meno.

Lo studio procederà per gradi. In primis verranno analizzati i dati dal punto di vista descrittivo per identificare le propietà statistiche delle variabili considerate e quindi procedere ad una trattazione efficace. Successivamente verranno gettate le basi teoriche dell'analisi spaziale descrivendo minuziosamente il framework utilizzato per l'analisi.

DOWNLOAD COMPLETO: https://drive.google.com/file/d/1O8YBXTwrRaG_N9HHmvsJJJG-nsmzg198/view?usp=sharing

PROCEDIMENTO:

1. Estrarre il contenuto dell'archivio
2. Entrare nella cartella estratta e aprire il file "Shiny Progetto Deployment"

IMPORTANTE!! SE RSTUDIO CHIEDE DI INSTALLARE I PACCHETTI MANCANTI, PROCEDERE ALL'INSTALLAZIONE PRIMA DI OGNI ALTRO PASSAGGIO SUCCESSIVO.

3. Una volta installati i pacchetti, navigare tra i file della cartella in basso a destra in RStudio e cliccare su .RData
In questo modo verrà caricato l'environment in Rstudio
4. Una volta caricato, aprire e lanciare app.R per l'applicazione oppure "progetto markdown" per il file markdown.



#####
Il file ProgettoLab.rar contiene:

1. Cartella "Data": Contiene sia il file csv dei redditi sia gli shapefile utilizzati per il territorio italiano

2. .Rdata: Contiene l'environement necessario al funzionamento (Tale environment è stato generato dal "source script.R")

3. app.R : Applicazione shiny

4. Progetto Markdown: Documento interattivo RMarkdown contenente tesina e widget shiny

5. blocco e table generator: Genera blocchi per block bootstrap e genera tabelle contenenti i risultati dell'analisi. 
	Queste tabelle sono poi state inserite alla fine del file RMarkdown.
	Il contenuto di questi script è già presente in "source script".
	

6. biblio.bib : Contiene riferimenti bibliografici utilizzati in RMarkdown

7. footer,header : Rispettivamente footer e header in html utilizzati in RMarkdown




