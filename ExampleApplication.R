#####################################################
# Exemplary application of the PastTwitter functions
# Author: @ChRauh (05.05.2021)
#####################################################


# Attach PastTwitter functions ####
source("PastTwitter.R")

# The Twitter handle of interest ####
handle <- "WZB_Berlin"

# Output params
datafile <- paste0("./data/", handle, ".RDS")
plotfile <- paste0("./plots/", handle, ".png")

# Store start time
start <- Sys.time()

# Get archive.org snapshots ####
snapshots <- handleSnapshots(handle)

# Extract account info ####
info <- extractAccountInfo(snapshots)

#Plot follower count ####
pl.f <- plotFollowers(info)

# Export ####
write_rds(info, datafile)
ggsave(plotfile, pl.f, width = 22, height = 14, units = "cm")


# Duration ####
Sys.time() - start