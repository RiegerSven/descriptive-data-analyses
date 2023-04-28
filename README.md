# (Descriptive) Data Analyses with Shiny & R

You need the following R packages:


```
pkgList <- list("shiny", "data.table", "DT", "survey")
```

```{r}
lapply(pkgList,
       function(x) 
         if(!x %in% rownames(installed.packages())) install.packages(x))

```