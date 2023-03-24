library(readr)
library(ggplot2)
library(dplyr)

x <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/beitbridge_discharge.csv")
y <- x %>%
     select(Year,`Height (m)`) %>%
     rename(height=`Height (m)`)

m <- lm(y$height~y$Year)
summary(m)
confint(m)
# Call:
#      lm(formula = y$height ~ y$Year)
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.23808 -0.16895 -0.01556  0.12205  0.32043 
# Coefficients:
#      Estimate Std. Error t value Pr(>|t|)
# (Intercept)  1.3666340  7.5547394   0.181    0.858
# y$Year      -0.0004995  0.0037623  -0.133    0.895
# Residual standard error: 0.1741 on 26 degrees of freedom
# (3 observations deleted due to missingness)
# Multiple R-squared:  0.0006776,	Adjusted R-squared:  -0.03776 
# F-statistic: 0.01763 on 1 and 26 DF,  p-value: 0.8954
#                   2.5 %       97.5 %
# (Intercept) -14.16235519 16.895623166
# y$Year       -0.00823302  0.007233954

ggplot(y) +
     geom_point(aes(x=Year,y=height)) +
     geom_smooth(aes(x=Year,y=height), method = "lm", se = TRUE, color='blue') +
     xlab("Hydrologic Year") +
     ylab("Mean Gauge Height (m)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12))


s <- read_csv("/Users/davidkahler/Documents/Hydrology_and_WRM/Limpopo_Basin_Study/Land_Movement_Gabi/d181_Durban_Daily_RawData_UHSLC.csv")
t <- s %>%
     group_by(Hydrologic_Year) %>%
     summarize(mn=mean(Value, na.rm = TRUE)/1000) # initially in mm

n <- lm(t$mn~t$Hydrologic_Year)
summary(n)
confint(n)
# Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       -1.0813041  0.8430176  -1.283    0.206  
# t$Hydrologic_Year  0.0011073  0.0004226   2.620    0.012 *
#      ---
#      Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 
#                          2.5 %      97.5 %
# (Intercept)       -2.7802944621 0.617686331
# t$Hydrologic_Year  0.0002555817 0.001959008

ggplot(t) +
     geom_point(aes(x=Hydrologic_Year, y=mn)) +
     geom_smooth(aes(x=Hydrologic_Year,y=mn), method = "lm", se = TRUE, color='blue') +
     xlab("Hydrologic Year") +
     ylab("Mean Sea-Level Height (m)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) + 
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 12)) +
     theme(axis.title = element_text(face = "plain", size = 12))








