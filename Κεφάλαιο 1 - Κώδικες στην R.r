###########################################################################################
# Κ. Μόδης, Γ. Βαλάκας, Δ. Σίδερη: Γεωστατιστική και Υπολογισμός Μεταλλευτικών Αποθεμάτων # 
###########################################################################################

# ----------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------------------------
# Κεφάλαιο 1 
# ----------------------------------------------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------------------------------------------

# ----------------------------------------
# Κώδικας 1.1. Εγκατάσταση πακέτων στην R.
# ----------------------------------------
install.packages("Rcpp") # Εγκατάσταση του πακέτου Rcpp
install.packages("RcppArmadillo")
install.packages("gstats") 
install.packages("sp") 
install.packages("moments")
install.packages("plot3D") 
install.packages("plot3Drgl") 
install.packages("scatterplot3d")
install.packages("ggplot2")
install.packages("raster")
install.packages("writexl")
install.packages("devtools")
devtools::install_github("GValakas/GeoSim") # Εγκατάσταση του πακέτου GeoSim από το github

# εναλλακτικά
# install.packages (c("Rcpp","RcppArmadillo","gstats","sp","moments","plot3D","plot3Drgl","scatterplot3d","ggplot2","raster"))
# ------------------------------------
# Κώδικας 1.2. Φόρτωση πακέτων στην R.
# ------------------------------------

library("gstats") # φόρτωση πακέτου gstats 
# …

# --------------------------------------------------
# Κώδικας 1.3. Εισαγωγή δεδομένων γεωτρήσεων στην R.
# --------------------------------------------------

# Εισαγωγή από github.com - Απαιτείται σύνδεση στο διαδίκτυο
SouthFieldDataShort <- read.csv("https://raw.githubusercontent.com/GValakas/Geostatistiki_kai_Ypologismos_Metalleutikon_Apothematon/main/Data/SouthFieldDataShort.txt", sep = "", quote = "\"'")
# Εισαγωγή και ανάγνωση δεδομένων τοπικά στον σκληρό δίσκο - Προσοχή στην διαδρομή του αρχείου
#SouthFieldDataShort <- read.csv("C:/Documents/SouthFieldDataShort.txt", header = TRUE, sep = "", quote = "\"'")

# Ορισμός ενεργού φακέλου εργασίας και εισαγωγή δεδομένων από αρχείο
# setwd("C:/Documents")
# SouthFieldDataShort <- read.csv("SouthFieldDataShort.txt", header = TRUE, sep = "", quote = "\"'")

dim(SouthFieldDataShort) # έλεγχος των διαστάσεων του πίνακα
nrow(SouthFieldDataShort) # Ο αριθμός των γραμμών του πίνακα
ncol(SouthFieldDataShort) # Ο αριθμός των στηλών του πίνακα
SouthFieldDataShort[1:20, ] # εμφάνιση των γραμμών 1 έως 20

class(SouthFieldDataShort)
# ----------------------------------------------------------
# Κώδικας 1.4.  Δεδομένα x, y, zmin, zmax για κάθε γεώτρηση.
# ----------------------------------------------------------

# Τρέξε Κώδικα 1.3
index <- which(!duplicated(SouthFieldDataShort$id) == TRUE) # φίλτρο επιλογής γεωτρήσεων με μοναδικό id 
n_drills <- length(index) # ο αριθμός των γεωτρήσεων

data_unique_id <- matrix(NaN, nrow = n_drills, ncol = 5) # δημιουργία πίνακα για εισαγωγή τιμών για κάθε γεώτρηση
colnames(data_unique_id) <- c("id", "x", "y", "zmax", "zmin")
data_unique_id <- as.data.frame(data_unique_id)
data_unique_id <- SouthFieldDataShort[index,1:3] 

# Εύρεση zmin και zmax
for (i in 1:n_drills){
data_unique_id$zmax[i] <- subset(SouthFieldDataShort, id == data_unique_id[i,1], select = c("z"))[1,]
data_unique_id$zmin[i] <- data_unique_id$zmax[i] - max(subset(SouthFieldDataShort ,id == data_unique_id[i,1], select = c("to")))
}
data_unique_id

# -------------------------------------------------
# Κώδικας 1.5: Κατακόρυφες Προβολές Γεωτρήσεων x-y.
# -------------------------------------------------

# Τρέξε Κώδικα 1.4
dev.new(width = 5, height = 5)
plot(data_unique_id$x, data_unique_id$y, main = "Κάτοψη Γεωτρήσεων", xlab = "x (m)", ylab = "y (m)", pch = 22, col = "blue", xlim = c(-15000,-12000), ylim = c(17000,20000))
text(data_unique_id$x, data_unique_id$y, data_unique_id$id, cex = 0.6, pos = 3, col = "red")

# -----------------------------------------------------
# Κώδικας 1.6. Δεδομένα γεωτρήσεων ανά 0.1 μέτρα βάθος. 
# -----------------------------------------------------

# Τρέξε Κώδικα 1.3
data_field <- c()
z_step <- 0.1
# Δομή Επανάληψης
for (i in 1:nrow(SouthFieldDataShort)){
z_transf <- SouthFieldDataShort$z[i] - seq(SouthFieldDataShort$from[i], SouthFieldDataShort$to[i], z_step) # δημιουργία μετασχηματισμένου z για την i γραμμή
idata <- as.data.frame(matrix(NaN,nrow = length(z_transf), ncol = ncol(SouthFieldDataShort))) # ορισμός πλαισίου δεδομένων
idata[1:length(z_transf),] <- SouthFieldDataShort[i,] # δεδομένα της i γραμμής που επαναλαμβάνονται
idata[1:length(z_transf),(ncol(SouthFieldDataShort)+1)] <- z_transf # δημιουργία νέας στήλης z_transf στα δεδομένα της i γραμμής 
data_field <- rbind(data_field,idata) # ενοποίηση δεδομένων των i γραμμών 
} # τέλος for
colnames(data_field) <- c(colnames(SouthFieldDataShort),"z_transf") # ονομασία στηλών
data_field <- data_field[ ,c(1:6,14,7:13)] # αλλαγή εμφάνισης σειράς των στηλών

# --------------------------------------------------------------------------------
# Κώδικας 1.7. Τρισδιάστατη απεικόνιση των γεωτρήσεων και των λιθολογικών φάσεων.
# --------------------------------------------------------------------------------

# Τρέξε Κώδικα 1.6
library(plot3D)
library(plot3Drgl)
scatter3D(x = data_field$x, y = data_field$y, z = data_field$z_transf, colvar = data_field$lithological_code, 
col = c("orange", "black", "red", "green", "blue"), 
pch = "-", cex = 1, cex.axis = 0.5, cex.lab = 1,
theta = -45, ticktype = "detailed", 
xlab = "x (m)", ylab = "y (m)", zlab = "z (m)",  main = "Γεωτρήσεις Λιγνιτωρυχείου",
colkey = list(at = c(1.4,2.2,3,3.84,4.64), side = 1, 
addlines = TRUE, length = 0.2, width = 0.7,
expand = 0.1,
labels = c("AL", "CO", "HD", "MG", "SG"), cex.axis = 1))
plotrgl(new = TRUE)  

# ----------------------------------------------
# Κώδικας 1.8. Δημιουργία τετραγωνικού καννάβου.
# ----------------------------------------------

library(gstat)
xyz_grid <- expand.grid(x = seq(from = 0, to = 100, by = 25), y = seq(from = 0, to = 100, by = 25), z = seq(from = 0, to = 100, by = 25)) 

# ---------------------------------------------
# Κώδικας 1.9. Κάτοψη των κέντρων του καννάβου.
# ---------------------------------------------

center_grid <- xyz_grid
center_grid <- xyz_grid[-which(xyz_grid[ ,1] == 100 | xyz_grid[ ,2] == 100 | xyz_grid[ ,3] == 100), ] + 12.5
plot(center_grid$x, center_grid$y, main = "Κέντρα των μπλόκ", xlab = "x (m)", ylab = "y (m)", pch = 20, col = "red", xlim = c(0,100), ylim = c(0,100))

# ---------------------------------------------------------------
# Κώδικας 1.10. Τρισδιάστατη απεικόνιση των κέντρων του καννάβου.
# ---------------------------------------------------------------

library(plot3D)
dev.new()
scatter3D(center_grid$x, center_grid$y , center_grid$z, colvar = center_grid$z, pch = 20, cex = 0.5, colkey = TRUE,
xlim = c(0,100), ylim = c(0,100),
theta = -45, phi = 10,
ticktype = "detailed", 
xlab = "x (m)", ylab = "y (m)", zlab = "z (m)", clab = "z (m)")

# --------------------------------------------------------------------------------
# Κώδικας 1.11. Τρισδιάστατη απεικόνιση των γεωτρήσεων και της θερμογόνου δύναμης.
# --------------------------------------------------------------------------------  

# Τρέξε Κώδικα 1.6
library(plot3D)
library(plot3Drgl)
scatter3D(x = data_field$x[-which(data_field$KThI == "NaN")], y = data_field$y[-which(data_field$KThI == "NaN")], 
z = data_field$z_transf[-which(data_field$KThI == "NaN")], colvar = data_field$KThI[-which(data_field$KThI == "NaN")], 
colkey = FALSE, 
pch = "-", cex = 1, cex.axis = 0.5, cex.lab = 1,
theta = -45, ticktype = "detailed",
xlab = "x (m)", ylab = "y (m)",zlab = "z (m)",  clab="KThI", main = "Γεωτρήσεις Λιγνιτωρυχείου",
add = TRUE
)
plotrgl(new = TRUE)

  