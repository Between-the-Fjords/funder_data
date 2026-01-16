library(targets)
source("other_code/load_libraries.R")

# Prune outdated metadata if you get "branches out of range" errors
# Uncomment the line below if you encounter that error:
# targets::tar_prune()
# Make all targets invalid to force rebuild (use everything() to invalidate all)
# targets::tar_invalidate(everything())
# Or force rebuild without invalidating:
# targets::tar_make(force = TRUE)

targets::tar_make()

# Load all targets (workaround for tar_load_everything() branch error)
# tar_load_everything() sometimes fails with "branches out of range" error
# Loading individually works around this issue
meta <- targets::tar_meta()
targets_list <- meta$name[meta$type == "stem"]
for (t in targets_list) {
    tryCatch(
        {
            targets::tar_load(t, envir = .GlobalEnv)
        },
        error = function(e) {
            warning("Failed to load target: ", t, " - ", e$message)
        }
    )
}
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint
