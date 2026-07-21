# export_qmd renders the expected document

    Code
      cat(export_qmd(sects_fixture(), "Iris report"))
    Output
      ---
      title: "Iris report"
      ---
      
      # Stack
      
      Data preparation.
      
      The iris data.
      
      ```{r}
      #| label: tbl-data
      #| tbl-cap: "Dataset"
      data <- datasets::iris
      data
      ```
      
      Setosa only.
      
      ```{r}
      #| label: tbl-sub
      #| tbl-cap: "Subset"
      sub <- subset(data, Species == "setosa")
      sub
      ```
      
      ```{r}
      #| label: head
      #| include: false
      head <- utils::head(sub, 3)
      ```

# export_spin renders the expected script

    Code
      cat(export_spin(sects_fixture()))
    Output
      #' # Stack
      #' Data preparation.
      #' 
      #' The iris data.
      #+ data
      data <- datasets::iris
      data
      
      #' Setosa only.
      #+ sub
      sub <- subset(data, Species == "setosa")
      sub
      
      #+ head, include=FALSE
      head <- utils::head(sub, 3)

