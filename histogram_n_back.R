# Subset and prepare the data
df <- subset(dfa, task == 'n-back')
df$mi <- df$pac3_f

df$ps_r <-round(df$ps)

# Create the plot
ggplot(df, aes(x = mi, color = factor(n_back))) +
  geom_density(size = 0.5) +
  facet_wrap(~strategy) +
  #labs(title = "Kernel Density of pupil by n_back",x = "ps", y = "Density", color = "n_back") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12),
        legend.position = "top")

