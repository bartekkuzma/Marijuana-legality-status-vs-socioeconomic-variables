# Marijuana legality status vs socioeconomic variables

Project created for <i>Advanced Econometrics I</i> classes at WNE UW.

## About project

Main objective of this project was to investigate wheater legality status of marijuana in US states is connected with socioeconomic factors somehow associated with this drug with like price of joint, number of overdose deaths, admissions to rehab, number of prescription for opoids and so on. Accordingly to previous studies I also added such factors like arrivals to the state or crime rates in each state.  
My target variable was catergorical with 3 levels (illegal, legal for medical purposes and legal for recreational use). For estimation I used <b>linear regression</b> (it is not appropriate in this case, but it was one of the requirements), <b>ordered logit</b> and <b>ordered probit</b>.  

## About repository

<b>Does marijuana legalization change something?</b> is a desription of whole project (in Polish).   
Jupyter Notebook file <b>Visualization.ipynb</b> contains some plots, charts and maps. Those were neccesery for exploratory data analysis. However, for some reason, <i>Plotly Express</i> has a problem to display graphs here, that's why repository contains images folder with static images of the charts. That's problematic, because some of those graphs are interactable and to see it that way you should open this notebook with <ins>nbviewer</ins> (button in top right corner when you open notebook here).  
All modeling part of the project was performed with R in <b>Modeling.R</b> file. I resigned from Python for two reasons. Firstly, because I want to practise with this language and secondly because Python is not super friendly environment with ordinal regression.  
Of course repository also contains dataset file which I manualy created and used in this study.

## Tech stack

* R
* Python

## Authors

* **Bartek Kuźma**

