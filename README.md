# (Descriptive) Data Analyses with Shiny & R

Note that this app is in a very early stage.

You need the following R packages (hopefully i did not forget one):


```
pkgList <- list("shiny", "data.table", "DT", "survey", "ggplot2", "rmarkdown")
```

```{r}
lapply(pkgList,
       function(x) 
         if(!x %in% rownames(installed.packages())) install.packages(x))
```


You can use the app with the following code:

```
shiny::runGitHub(repo = "descriptive-data-analyses",
                 username = "RiegerSven",
                 ref = "main")
```