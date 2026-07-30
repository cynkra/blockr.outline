# export_qmd renders the expected document

    Code
      cat(export_qmd(sects_fixture(), "Iris report"))
    Output
      ---
      title: "Iris report"
      df-print: kable
      ---
      
      # Stack
      
      Data preparation.
      
      The iris data.
      
      ```{r}
      #| label: data
      #| tbl-cap: "Dataset"
      data <- datasets::iris
      blockr.viz::static_exhibit(data)
      ```
      
      Setosa only.
      
      ```{r}
      #| label: sub
      #| tbl-cap: "Subset"
      sub <- subset(data, Species == "setosa")
      blockr.viz::static_exhibit(sub)
      ```

# export_spin renders the expected script

    Code
      cat(export_spin(sects_fixture()))
    Output
      #' # Stack
      #' Data preparation.
      #' 
      #' **Dataset**
      #' The iris data.
      #+ data
      data <- datasets::iris
      blockr.viz::static_exhibit(data)
      
      #' **Subset**
      #' Setosa only.
      #+ sub
      sub <- subset(data, Species == "setosa")
      blockr.viz::static_exhibit(sub)

