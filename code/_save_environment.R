

# install.packages("renv")

# 1. Initialize renv (if you haven't yet)
# renv::init()


# 2. Check which packages are actually used
renv::dependencies()

# 3. Snapshot only the used packages
options(renv.snapshot.prune = TRUE) # automatically remove unused packages
renv::snapshot()


