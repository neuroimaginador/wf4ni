# Create a new environment as the parent of global, with
# wf4ni versions of tempfile and tempdir.
.insert_wf4ni_shims <- function(work_dir) {

  force(work_dir)

  if ("wf4ni_shims" %in% search()) return()

  e <- new.env()

  e$old_tempfile <- base::tempfile
  e$old_tempdir <- base::tempdir
  e$work_dir <- work_dir

  base::attach(e,
               name = "wf4ni_shims",
               warn.conflicts = FALSE)

  unlockBinding("tempdir", baseenv())

  assign("tempdir",
         function(check = NULL) {

           work_dir <- get("work_dir",
                           as.environment("wf4ni_shims"))

           return(work_dir)

         }, baseenv())

}

# Remove all inserted shims
.remove_wf4ni_shims <- function() {

  if (!("wf4ni_shims" %in% search())) return()

  unlockBinding("tempdir", baseenv())

  assign("tempdir",
         get("old_tempdir", as.environment("wf4ni_shims")),
         baseenv())

  lockBinding("tempdir", baseenv())

  detach(pos = which(search() == "wf4ni_shims"))

}

# Create the folder for temporary results
.create_tempdir <- function() {

  dir.create(path = tempdir(),
             recursive = TRUE,
             showWarnings = FALSE)

}
