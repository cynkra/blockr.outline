# the report qmd and spin render the expected documents

    Code
      cat(export_qmd(s, "Iris pilot", block_level = "##", flags = rpt_flags(), items = rpt_items(),
      settings = rpt_settings()))
    Output
      ---
      title: "Iris pilot"
      fig-width: 8
      fig-height: 4.5
      df-print: kable
      format:
        html:
          embed-resources: true
      execute:
        warning: false
        message: false
      ---
      
      ```{r}
      #| label: data
      #| include: false
      data <- datasets::iris
      ```
      
      Why we look.
      
      ```{r}
      #| label: sub
      #| output: false
      sub <- subset(data, Species == "setosa")
      ```
      
      The **first** rows.
      
      ## Head
      
      
      ```{r}
      #| label: head
      #| echo: false
      head <- utils::head(sub, 3)
      head
      ```

---

    Code
      cat(export_spin(s, block_level = "##", title = "Iris pilot", flags = rpt_flags(),
      items = rpt_items(), settings = rpt_settings()))
    Output
      #' ---
      #' title: "Iris pilot"
      #' fig-width: 8
      #' fig-height: 4.5
      #' df-print: kable
      #' format:
      #'   html:
      #'     embed-resources: true
      #' execute:
      #'   warning: false
      #'   message: false
      #' ---
      
      #+ data, include=FALSE
      data <- datasets::iris
      
      #' Why we look.
      
      #+ sub, results="hide", fig.show="hide"
      sub <- subset(data, Species == "setosa")
      
      #' The **first** rows.
      
      #' ## Head
      #+ head, echo=FALSE
      head <- utils::head(sub, 3)
      head

