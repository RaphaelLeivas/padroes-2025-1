# padroes-2025-1
Repo para códigos da disciplina de Reconhecimento de Padrões 2025/1, Engenharia de Sistemas UFMG

# Setting Up Development Environment (R)

- install R on your machine (Windows) https://cran-r.c3sl.ufpr.br/
- R is usually installed in this folder: C:\Program Files\R\R-4.2.3\bin . Take this folder and add it to your PATH system variable.
- install RStudio (Windows) https://posit.co/download/rstudio-desktop/
    - when asked which version of R you want to use, select the one that was locally installed in your computer by the previous two steps
- Change the RStudio theme to a dark one in Tools -> Global Options -> Appearance
- to install any packages, if the RStudio IDE doesn't show a notification already, do the following in the RStudio terminal:

``` 
install.packages("package_name")
```

- open an R script and run CTRL + SHITF + S to run it. 