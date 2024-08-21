# 1
n100p200s6_df = list(est = n100p200s6_RCV, fixed = n100p200s6_Fixed)

# 2
n100p300s6_df = list(est = n100p300s6_RCV, fixed = n100p300s6_Fixed)

# 3
n100p500s6_df = list(est = n100p500s6_RCV, fixed = n100p500s6_Fixed)


# Plot -- Estimated sigma
boxplot((n100p150s6_RCV2$estimated_sigma)^2,(n100p200s6_RCV$estimated_sigma)^2,
        (n100p300s6_RCV$estimated_sigma)^2,(n100p500s6_RCV$estimated_sigma)^2,
        names = c(150,200,300,500),xlab = "Dimensionality p",ylab = expression(hat(sigma)^2),cex.lab = 1.2)
abline(h = 1, col = "red", lwd = 2, lty = 1)


# Plot -- CI Length
ggarrange(result_plot(n100p200s6_df),  result_plot(n100p500s6_df),
          labels = c("A", "B"), ncol = 2, common.legend = TRUE, legend="bottom")


# Table -- CI Length
result_table(n100p200s6_df)[1]
result_table(n100p300s6_df)[1]
result_table(n100p500s6_df)[1]


# Plot -- Coverage probability
df1 <- plot_cov(n100p150s6_df)
df2 <- plot_cov(n100p200s6_df)
df3 <- plot_cov(n100p300s6_df)
df4 <- plot_cov(n100p500s6_df)

df1 <- df1 %>% mutate(p = 150)
df2 <- df2 %>% mutate(p = 200)
df3 <- df3 %>% mutate(p = 300)
df4 <- df4 %>% mutate(p = 500)

combined_df <- bind_rows(df1, df2, df3, df4)

custom_labeller <- labeller(`beta == 0` = c("TRUE" = "Beta == 0", "FALSE" = "Beta != 0"))
ggplot(combined_df, aes(x = p, y = `Mean coverage`, color = sigma, group = sigma)) +
  geom_line() +
  geom_point() +
  facet_wrap(~`beta == 0`, labeller = custom_labeller) +
  labs(x = "Dimensionality p",
       y = "Mean Coverage probability",
       color = "Type") +
  scale_color_manual(values = c("#F8766D", "#00BFC4"),
                     labels = c(expression(Estimated~sigma^2), 
                                expression(Fixed~sigma^2))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title.x = element_text(size = 12.5,margin = margin(t = 10)),
        axis.title.y = element_text(size = 12.5,margin = margin(r = 10)),
        strip.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12.5),
        legend.spacing.x = unit(20, 'cm'),  
        legend.spacing.y = unit(20, 'cm'))
