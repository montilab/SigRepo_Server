output$signaturePie <- renderPlot({
  data <- signatures_df
  
  group_by_value <- input$group_by
  
  if (group_by_value == "None") {
    plot_data <- data %>%
      count(Signature) %>%
      arrange(desc(n)) %>%
      mutate(perc = n / sum(n) * 100,
             label = paste0(Signature, " (", round(perc, 1), "%)"))
    
    ggplot(plot_data, aes(x = "", y = n, fill = Signature)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Overall Signature Distribution") +
      theme_void()
    
  } else {
    plot_data <- data %>%
      count(!!sym(group_by_value)) %>%
      arrange(desc(n)) %>%
      mutate(perc = n / sum(n) * 100,
             label = paste0(!!sym(group_by_value), " (", round(perc, 1), "%)"))
    
    ggplot(plot_data, aes(x = "", y = n, fill = !!sym(group_by_value))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = paste("Distribution by", group_by_value)) +
      theme_void()
  }
})