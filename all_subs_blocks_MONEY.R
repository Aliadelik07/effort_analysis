

dfa <- dfa[dfa$m_correct <= dfa$score, ] # Remove rows where m_correct > score


ggplot(subset(dfa,strategy == 'static'), aes(x = factor(m_correct), y = score, fill = factor(n_back))) +
  geom_boxplot(alpha = 0.6) +
  labs(
    title = "Free choice effort",
    x = "Required score",
    y = "FCE"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    strip.text = element_text(size = 16, face = "bold")  # Facet title font
  ) +
  #facet_wrap(~ m_correct, scales = "fixed") +
  scale_fill_manual(
    values = c("1" = "blue", "2" = "green", "3" = "red")  # Fill colors
  ) +
  #scale_y_continuous(breaks = c(0,4, 8, 12,20)) +
  expand_limits(y = 0) 
