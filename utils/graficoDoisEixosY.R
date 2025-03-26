rm(list = ls())
dev.off()

f <- c(1, 2, 5, 10, 15, 20, 25) * 10^3

# CALCULOS PRE RELATORIO 11

# Rin <- 1e3
# R <- 56
# C <- 200e-9
# L <- 1.41e-3

# omega <- 2 * pi * f
#
# Z_real <- Rin + (1/R)/((1/R)^2 + ((omega * C - 1/(omega * L))^2))
# Z_imag <- - (omega * C - 1 / (omega * L)) / ((1/R)^2 + ((omega * C - 1/(omega * L))^2))
#
# Z_mod <- sqrt(Z_real^2 + Z_imag^2)
# Z_phase <- atan2(Z_imag, Z_real)
#
# ress <- 1 / sqrt(L * C)

# def_seconds <- c(10, 11, 2, -1.5, -0.5, -0.4, -0.25) * 10^-6
# T <- 1 / f
#
# Z_phase <- 2 * pi * def_seconds / T
#
# Vp <- rep(2.5, length(f))
# Ip <- c(2.479, 2.475, 2.41, 2.36, 2.37, 2.39, 2.42) * 10^-3
# Z_mod_calc <- Vp / Ip


## CALCULOS RELATORIO 5

# PARTE 1 - CIRCUITO RC

Rc <- 470

ip_medidos_C <- c(250, 450, 650, 750, 800, 810, 825) * 10^-3 / Rc
ip_prelatorio_C <- c(1.205, 2.163, 3.508, 3.923, 3.987, 3.994, 4.015) * 10^-3 / 2

e_ip_C <- ((ip_medidos_C - ip_prelatorio_C) / ip_prelatorio_C) * 100

def_us_medidos_C <- c(-200, -80, -20, -5, -2, -1.4, -1) * 10^-6
def_rad_medidos_C <- def_us_medidos_C * 2 * pi * f
def_rad_calc_C <- c(-1.32, -1.03, -0.64, -0.29, -0.23, -0.1, -0.09)

e_def_C <- ((def_rad_medidos_C - def_rad_calc_C) / def_rad_calc_C) * 100

# PARTE 2 - CIRCUITO RL

Rl <- 10

ip_medidos_L <- c(750, 550, 350, 250, 225, 250, 150) * 10^-3 / Rl
ip_prelatorio_L <- c(85.78,
                   64.62,
                   33.45,
                   16.45,
                   10.95,
                   8.08,
                   6.29) * 10^-3

e <- ((ip_medidos_L - ip_prelatorio_L) / ip_prelatorio_L) * 100

def_us_medidos_L <- c(50, 60, 35, 20, 14, 10, 8) * 10^-6
def_rad_medidos_L <- def_us_medidos_L * 2 * pi * f
def_rad_calc_L <- c(0.69, 0.85, 1.24, 1.34, 1.47, 1.53, 1.65)

e_def_L <- ((def_rad_medidos_L - def_rad_calc_L) / def_rad_calc_L) * 100

# QUESTOES PARA O RELATORIO

# dataset
Z_c <- 1 / ip_medidos_C
Z_l <- 1 / ip_medidos_L
def_Zc <- def_rad_medidos_C
def_Zl <- def_rad_medidos_L

## add extra space to right margin of plot within frame
par(mar = c(5, 4, 4, 6) + 0.1)

graphTitle <- expression(paste("Impedância Circuito RL (", Omega, ") x Frequência (Hz)"))

firstDataLabel <- expression(paste("Impedância (", Omega, ")"))
firstDataColor <- "black"

secondDataColor <- "blue"
secondDataLabel <- "Defasagem (rad)"

xAxisLabel <- "Frequência (Hz)"
xAxisColor <- "black"

## Plot first set of data and draw its axis
plot(
     f,
     Z_l,
     pch = 16,
     axes = FALSE,
     ylim = c(0, max(Z_l)),
     xlab = "",
     ylab = "",
     type = "b",
     col = firstDataColor,
     main = graphTitle
)

axis(2, ylim = c(0, max(Z_l)), col = firstDataColor, col.axis = firstDataColor, las = 1) ## las=1 makes horizontal labels
mtext(firstDataLabel, side = 2, line = 3, col = firstDataColor)
box()

## Allow a second plot on the same graph
par(new = TRUE)

# Plot the second plot and put axis scale on right
plot(
     f,
     def_Zl,
     pch = 15,
     xlab = "",
     ylab = "",
     ylim = c(0, max(def_Zl)),
     axes = FALSE,
     type = "b",
     col = secondDataColor
)

mtext(secondDataLabel, side = 4, col = secondDataColor, line = 3)
axis(4, ylim = c(0, max(def_Zl)), col = secondDataColor, col.axis = secondDataColor, las = 1)

## Draw the time axis
axis(1, pretty(range(f), 10))
mtext(xAxisLabel, side = 1, col = xAxisColor, line = 2.5)

## Add Legend
legend("bottom",
     legend = c(firstDataLabel, secondDataLabel),
     text.col = c(firstDataColor, secondDataColor),
     pch = c(16, 15), 
     col = c(firstDataColor, secondDataColor)
)
