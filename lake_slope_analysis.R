library(readr)
library(ggplot2)

x <- read_csv("lake_slope_analysis.csv")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `NE Catchment 1`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("NE1.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `NE Catchment 2`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("NE2.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `NE Catchment 3`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("NE3.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `NE Catchment 4`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("NE4.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `NW Catchment`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("NW.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `SW Catchment 7`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("SW7.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `SW Catchment 6`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("SW6.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `SW Catchment 5`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("SW5.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `SW Catchment 4`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("SW4.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `SW Catchment 3`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("SW3.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `SW Catchment 2`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("SW2.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `SW Catchment 1`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("SW1.png", p, device = "png")

p <- ggplot(x, aes(x = Zone)) +
     geom_col(aes(y = `Outlet Catchment`)) +
     xlab("Slope (0-600,000, dimensionless)") +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(axis.text = element_text(face = "plain", size = 20)) +
     theme(axis.title = element_text(face = "plain", size = 20)) +
     theme(axis.text.x = element_blank())
ggsave("Out.png", p, device = "png")


