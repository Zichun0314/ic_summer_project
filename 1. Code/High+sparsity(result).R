#S6
n100p150s6_df = list(est = n100p150s6_RCV2, fixed = n100p150s6_Fixed)

#S10
n100p150s10_df = list(est = n100p150s10_RCV2, fixed = n100p150s10_Fixed)

#S14
n100p150s14_df = list(est = n100p150s14_RCV2, fixed = n100p150s14_Fixed)

#S20
n100p150s20_df = list(est = n100p150s20_RCV2, fixed = n100p150s20_Fixed)

#S50
n100p150s50_df = list(est = n100p150s50_RCV2, fixed = n100p150s50_Fixed)

#S100
n100p150s100_df = list(est = n100p150s100_RCV2, fixed = n100p150s100_Fixed)

#S150
n100p150s150_df = list(est = n100p150s150_RCV2, fixed = n100p150s150_Fixed)




# choose lambda for RCV -- MSE

## s6
mean(((n100p150s6_RCV1$estimated_sigma)^2-1)^2)
mean(((n100p150s6_RCV2$estimated_sigma)^2-1)^2)
n100p150s6_RCV3_sigmahat = n100p150s6_RCV3$estimated_sigma[!(n100p150s6_RCV3$estimated_sigma=="NaN"|n100p150s6_RCV3$estimated_sigma=="Inf")]
mean(((n100p150s6_RCV3_sigmahat)^2-1)^2)

## s10
mean(((n100p150s10_RCV1$estimated_sigma)^2-1)^2)
mean(((n100p150s10_RCV2$estimated_sigma)^2-1)^2)
n100p150s10_RCV3_sigmahat = n100p150s10_RCV3$estimated_sigma[!(n100p150s10_RCV3$estimated_sigma=="NaN"|n100p150s10_RCV3$estimated_sigma=="Inf")]
mean(((n100p150s10_RCV3_sigmahat)^2-1)^2)

## s14
mean(((n100p150s14_RCV1$estimated_sigma)^2-1)^2)
mean(((n100p150s14_RCV2$estimated_sigma)^2-1)^2)
n100p150s14_RCV3_sigmahat = n100p150s14_RCV3$estimated_sigma[!(n100p150s14_RCV3$estimated_sigma=="NaN"|n100p150s14_RCV3$estimated_sigma=="Inf")]
mean(((n100p150s14_RCV3_sigmahat)^2-1)^2)


## s20
mean(((n100p150s20_RCV1$estimated_sigma)^2-1)^2)
mean(((n100p150s20_RCV2$estimated_sigma)^2-1)^2)
n100p150s20_RCV3_sigmahat = n100p150s20_RCV3$estimated_sigma[!(n100p150s20_RCV3$estimated_sigma=="NaN"|n100p150s20_RCV3$estimated_sigma=="Inf")]
mean(((n100p150s20_RCV3_sigmahat)^2-1)^2)


## s50
mean(((n100p150s50_RCV1$estimated_sigma)^2-1)^2)
mean(((n100p150s50_RCV2$estimated_sigma)^2-1)^2)
n100p150s50_RCV3_sigmahat = n100p150s50_RCV3$estimated_sigma[!(n100p150s50_RCV3$estimated_sigma=="NaN"|n100p150s50_RCV3$estimated_sigma=="Inf")]
mean(((n100p150s50_RCV3_sigmahat)^2-1)^2)

## s100
mean(((n100p150s100_RCV1$estimated_sigma)^2-1)^2)
mean(((n100p150s100_RCV2$estimated_sigma)^2-1)^2)
n100p150s100_RCV3_sigmahat = n100p150s100_RCV3$estimated_sigma[!(n100p150s100_RCV3$estimated_sigma=="NaN"|n100p150s100_RCV3$estimated_sigma=="Inf")]
mean(((n100p150s100_RCV3_sigmahat)^2-1)^2)

## s150
mean(((n100p150s150_RCV1$estimated_sigma)^2-1)^2)
mean(((n100p150s150_RCV2$estimated_sigma)^2-1)^2)
n100p150s150_RCV3_sigmahat = n100p150s150_RCV3$estimated_sigma[!(n100p150s150_RCV3$estimated_sigma=="NaN"|n100p150s150_RCV3$estimated_sigma=="Inf")]
mean(((n100p150s150_RCV3_sigmahat)^2-1)^2)

# s20 --TPR, Sigma
par(mfrow=c(1,2))
boxplot(n100p150s20_RCV1$TPR,n100p150s20_RCV2$TPR,n100p150s20_RCV3$TPR,
        names = c(0.2,0.1,0.05),xlab = expression(lambda),ylab = "True positive rate",cex.lab = 1.2)
boxplot((n100p150s20_RCV1$estimated_sigma)^2,(n100p150s20_RCV2$estimated_sigma)^2,(n100p150s20_RCV3$estimated_sigma)^2,
        names = c(0.2,0.1,0.05),xlab = expression(lambda),ylab = expression(hat(sigma)^2),cex.lab = 1.2)
abline(h = 1, col = "red", lwd = 2, lty = 1)


# Plot -- Estimated sigma
par(mfrow=c(1,1))
boxplot((n100p150s6_RCV2$estimated_sigma)^2,(n100p150s10_RCV2$estimated_sigma)^2,
        (n100p150s14_RCV2$estimated_sigma)^2,(n100p150s20_RCV2$estimated_sigma)^2,
        (n100p150s50_RCV2$estimated_sigma)^2,(n100p150s100_RCV2$estimated_sigma)^2,
        (n100p150s150_RCV2$estimated_sigma)^2,
        names = c(6,10,14,20,50,100,150),xlab = "Sparsity level s",ylab = expression(hat(sigma)^2),cex.lab = 1.2)
abline(h = 1, col = "red", lwd = 2, lty = 1)

# Plot -- CI Length
result_plot(n100p150s6_df)

# Table -- CI Length
result_table(n100p150s6_df)[1]
result_table(n100p150s20_df)[1]
result_table(n100p150s150_df)[1]

# Plot -- Coverage probability
df1 <- plot_cov(n100p150s6_df)
df2 <- plot_cov(n100p150s10_df)
df3 <- plot_cov(n100p150s14_df)
df4 <- plot_cov(n100p150s20_df)
df5 <- plot_cov(n100p150s50_df)
df6 <- plot_cov(n100p150s100_df)
df7 <- plot_cov(n100p150s150_df)

df1 <- df1 %>% mutate(s = 6)
df2 <- df2 %>% mutate(s = 10)
df3 <- df3 %>% mutate(s = 14)
df4 <- df4 %>% mutate(s = 20)
df5 <- df5 %>% mutate(s = 50)
df6 <- df6 %>% mutate(s = 100)
df7 <- df7 %>% mutate(s = 150)

combined_df <- bind_rows(df1, df2, df3, df4,df5,df6,df7)


custom_labeller <- labeller(`beta == 0` = c("TRUE" = "Beta == 0", "FALSE" = "Beta != 0"))
ggplot(combined_df, aes(x = s, y = `Mean coverage`, color = sigma, group = sigma)) +
  geom_line() +
  geom_point() +
  facet_wrap(~`beta == 0`, labeller = custom_labeller) +
  labs(x = "Sparsity level s",
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
