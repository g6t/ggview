# printing lists the collection and its parameters

    Code
      print(plots_init())
    Message
      -- <plots_tbl> ------------------------------------------------------ 0 plots --
      Empty — add a plot with `plots_append()`.
    Code
      print(plots)
    Message
      -- <plots_tbl> · Test bundle ---------------------------- 2 plots in 1 folder --
      Module 1
       1* first  bar     4 x 8   Changed
       2  second heatmap 16 x 10 <no title>
      1 plot without a title.
      1 plot changed · `plots_reset()` forgets it
      `plots_get()` renders one · `as.gallery()` browses them all
    Code
      print(plots_params(plots, "Module 1/first"))
    Message
      -- <plots_params> ---------------------------------------------- 8 parameters --
        title    text   Title -> Changed
        subtitle text   Subtitle
        caption  text   Caption
        x        text   wt
        y        text   mpg
        legend   choice <none>  (right, left, top, bottom, none)
        width    number 12 -> 4
        height   number 8
    Code
      print(plots_dims(heatmap = c(18, 11)))
    Message
      -- <plots_dims> ------------------------------------------------------ inches --
        bar          12 x 8
        diverging    15 x 8
        dodged_bar   15 x 9
        dumbbell     16 x 9
        heatmap      18 x 11
        line         15 x 9
        stacked_bar  15 x 9
        .default     15 x 9
    Code
      print(customizer_default())
    Message
      -- <plots_customizer> ----------------------------------------------------------
        parameters: title, subtitle, caption, x, y, legend

