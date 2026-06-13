# Statistical Inference and Learning: Analisi Inferenziale sui Sinistri Assicurativi Auto

Questo repository contiene il progetto finale per il corso di **Statistical Inference and Learning** (SIL). 
A differenza dei classici approcci predittivi (Machine Learning puro), questo progetto si concentra su un'**analisi inferenziale** di un dataset di assicurazioni auto. L'obiettivo primario non è semplicemente massimizzare l'accuratezza predittiva, ma **interpretare e comprendere quali fattori socio-demografici e legati al veicolo incidono realmente sulla probabilità che un cliente presenti una richiesta di risarcimento (sinistro)**.

## 🎯 Obiettivi del Progetto
* **Esplorazione dei dati (EDA):** Analisi delle distribuzioni e delle correlazioni tra le variabili del profilo assicurativo.
* **Inferenza Statistica:** Identificare quali variabili (es. anni di esperienza alla guida, storico delle violazioni, chilometraggio annuo) hanno un effetto statisticamente significativo sulla probabilità di sinistro.
* **Modellistica:** Utilizzo di modelli di classificazione interpretabili (con un focus particolare sulla *Regressione Logistica* e l'interpretazione degli *Odds Ratio*).
* **Selezione del Modello:** Applicazione di tecniche di *Model Selection* e *Cross-Validation* per evitare l'overfitting e garantire la robustezza delle stime.

## 📊 Provenienza dei Dati
Il dataset utilizzato per questo progetto è **Car Insurance Data** ed è pubblicamente disponibile su Kaggle.
* **Fonte:** [Kaggle - Car Insurance Data by Sagnik1511](https://www.kaggle.com/datasets/sagnik1511/car-insurance-data/data)
* **Descrizione:** Il dataset contiene informazioni sui clienti di una compagnia assicurativa, tra cui dati demografici, caratteristiche del veicolo, storico di guida e la variabile target `OUTCOME` (che indica se il cliente ha richiesto o meno un risarcimento).

## 📂 Struttura del Repository
* Lo script `progetto_SIL.Rmd` (R Markdown) con tutto il codice R documentato passo dopo passo per la pulizia, l'analisi e la modellazione.
* Il report finale (formato HTML/PDF) generato a partire dal file RMarkdown, strutturato per la discussione d'esame.

## 🛠️ Tecnologie Utilizzate
* **Linguaggio:** R
* **Ambiente:** RStudio / R Markdown
* **Librerie Principali:** `tidyverse` (per manipolazione dati e plot), `ISLR` / `MASS` / `caret` (per i modelli statistici), `car` (per test VIF e diagnostica).
