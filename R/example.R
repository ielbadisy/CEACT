
# -------------------------------
# Example Test Code (Run interactively)
# -------------------------------
set.seed(123)
control <- data.frame(cost = rnorm(200, 500, 100), effect = rnorm(200, 0.6, 0.05), group = "control")
treatment <- data.frame(cost = rnorm(200, 550, 100), effect = rnorm(200, 0.63, 0.06), group = "treatment")
data <- rbind(control, treatment)

# Test cea()
res_cea <- cea(cost + effect ~ group, data = data, ref = "control")
print(res_cea)

# Test boot_icer()
res_boot <- boot_icer(cost + effect ~ group, data = data, ref = "control", R = 300)
print(res_boot$summary)

# Test plot_ceplane()
plot_ceplane(res_boot, k = 500)

# Test plot_ceac()
plot_ceac(res_boot, wtp_range = seq(0, 2000, 100))

# Test compute_nmb_ceac()
nmb_table <- compute_nmb_ceac(data = data.frame(c = res_boot$boot_dist[, 1], e = res_boot$boot_dist[, 2]),
                              wtp_range = seq(0, 2000, 100))
print(nmb_table)
