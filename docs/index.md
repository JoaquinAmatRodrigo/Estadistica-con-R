---
title: Bibliografía sobre estadística, bioestadística, data science y programación en R
author: "Joaquín Amat Rodrigo j.amatrodrigo@gmail.com"
date: '2017'
output:
  html_document:
    css: ~/R/x86_64-pc-linux-gnu-library/3.3/rmarkdown/rmd/h/bootstrap-3.3.5/css/journal.min_helvetica_custom_2.css
    highlight: haddock
    number_sections: yes
    theme: journal
    toc: yes
  pdf_document:
    latex_engine: xelatex
    toc: yes
  word_document:
    toc: yes
---
<base target="_top"/>

<style>
.tocify-header {
text-indent: 1px;
}

.tocify {
  font-size: 15px;
}

h1, h2, h3, h4, h5, h6, .h1, .h2, .h3, .h4, .h5, .h6 {
color: #1f1f1f;
}

h1.title{
text-align: center;
font-weight: 700;
letter-spacing: -1px;
font-size: 2.2em;
line-height: 1.2;
margin-bottom: .5em;
}

h4.author{
text-align: center;
font-style: normal;
font-weight: normal;
margin-top: 20px;
font-family: 'Open Sans','Helvetica Neue',Helvetica,Arial,sans-serif;
text-shadow: none;
color: #6d6d6d;
}

h4.date{
text-align: center;
font-style: normal;
font-weight: normal;
color: #6d6d6d;
}

body{
background-color: #ffffff;
font-family: 'Open Sans','Helvetica Neue',Helvetica,Arial,sans-serif;
font-size: 18px;
line-height: 1.5;
font-weight: 400;
text-shadow: none;
color: #333333;
text-rendering: optimizeLegibility;
letter-spacing: +0.1px;
}

.toc-content {
    padding-left: 20px;
    padding-right: 10px;
}

code{
  background-color: #f2f2f2;
  font-family: monospace;
  font-size: 0.9em;
  line-height: 1.4;
}

pre {
    padding: 20px;
    text-shadow: none;
    overflow: auto;
    font-size: 0.8em;
    line-height: 1.4;
}
</style>

Versión PDF: https://github.com/JoaquinAmatRodrigo/Estadistica-con-R

<br>

\ \ \ \ \ \ El siguiente listado contiene las fuentes de información sobre estadística, *data science* y programación en `R` que he ido encontrando y que me han resultado útiles. Están ordenadas según, a mi parecer, facilitan el aprendizaje.

A la hora de adentrarse en estas disciplinas existen diferentes aproximaciones, en mi caso, el objetivo inicial fue aprender las principales herramientas estadísticas empleadas en el análisis de datos del ámbito biomédico. La programación (en este caso `R`) era simplemente la herramienta auxiliar de la que me ayudaba para realizar los cálculos necesarios. La experiencia me he convencido de que adquirir ambos conocimientos en paralelo mejora mucho el aprendizaje. ¡Nada mejor para asimilar un método estadístico que poder simular, representar y aplicar fácilmente los cálculos necesarios!

Siguiendo esta idea, *utilizar la programación como herramienta para el análisis de datos*, recomiendo el siguiente orden:
<br>

1. **Open Intro Statistics**: Una buena introducción a la estadística. Bien explicado y con ejemplos interesantes. Libro gratuito con un curso online también gratuito.

2. **Swirl**: aprender `R` desde el terminal de forma dinámica. Visitar página web para ver instrucciones de instalación.

3. **R for Data Science, Garrett Grolemund and Hadley Wickham**: Introducción al uso de `R` aplicado al análisis de datos, con particular énfasis en el manejo, limpieza y visualización de la información.

4. **Handbook of Biological Statistics**: Un libro que explica los test estadísticos de forma muy clara sin profundizar en la matemática, identifica las ventajas y limitaciones de cada uno. Muy recomendable.

5. **Bioestadística Francisca Rius Díaz**: Libro en español de introducción a la bioestadística. Con ejemplos para realizar de forma manual, no por ordenador.

6. **Introduction to Statistical Learning with Aplications in R**: Libro en el que se introducen y se describen los principales modelos de regresión así como su implementación en `R`. Muy recomendable.

7. **Comparing groups Randomization and Bootsrap Methods using R Andrew S. Zieffler**: Descripción detallada de los métodos de *resampling* y *bootstrapping.*

8. **Practical Guide To Cluster Analysis in R, Alboukadel Kassambara**: Introducción a las técnicas de *clustering* y su implementación en `R`.

9. **Mastering Software Development in R Roger D. Peng, Sean Kross, and Brooke Anderson**: Libro dedicado a profundizar sobre el lenguaje de programación `R`.
<br><br>

Recomiendo leer la reflexión que hace *David Robinson* en su [blog](http://varianceexplained.org/r/teach-tidyverse/) sobre las diferentes formas de introducir el lenguaje de programación `R` a principiantes.
<br><br>

Otras fuentes de información que me han resultado útiles:

+ **Points of Significance, Nature**: La revista científica *Nature Methods* publica unas columnas en las que se explica de forma muy clara los principales métodos estadísticos empleados en ciencia.

+ **Statistics Using R with Biological Examples**: En este libro se introduce de forma simultánea el uso básico de `R` y conceptos de estadística.

+ **TheRBook Michael J Crawley**: Es un manual de uso de `R`. Recomendable leer los primeros capítulos para aprender los conceptos básicos. Los últimos capítulos son más útiles como consulta de temas concretos.

+ **Métodos estadísticos en ingeniería Rafael Romero Villafranca, Luisa Rosa Zúnica Ramajo**

+ **Statistical Bioinformatics with R, Sunil K. Mathur**

+ **R Tutorials by William B. King, Ph.D htt**p://ww2.coastal.edu/kingw/statistics/R-tutorials/. Describe los test estadísticos más comunes de forma práctica, con sus ventajas y desventajas. Incluye ejemplos en R.

+ http://www.um.es/ae/FEIR/20/ apuntes de la Universidad de Murcia. Repaso de los diferentes test estadísticos, ventajas, limitaciones y ejemplos en R.

+ **Bootstrap Methods and Permutation Tests by Tim Hesterberg**: Introducción muy intuitiva a los conceptos de *bootstrapping* y *resampling*.

+ **Linear models with R, Faraway**: Libro en el que se introducen y se describen los principales modelos de regresión así como su implementación en `R`. Profundiza un poco más en el aspecto matemático que el libro *Introduction to Statistical Learning with Aplications in R*, por lo que recomiendo leerlo a continuación de este último.

<br><br>
