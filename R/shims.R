# Change the temporary directory to store results
.change_workdir <- function(work_dir) {

  force(work_dir)

  attached <- search()

  if ("wf4ni_shims" %in% attached) return()

  e <- new.env()

  e$old_tempdir <- base::tempdir()
  e$work_dir <- normalizePath(work_dir)

  setTempDir(e$work_dir)

  .create_tempdir()

  base::attach(e,
               name = "wf4ni_shims",
               warn.conflicts = FALSE)

}

# Restore the previous tempdir.
.restore_tempdir <- function() {

  if (!("wf4ni_shims" %in% search())) return()

  setTempDir(get("old_tempdir", as.environment("wf4ni_shims")))

  detach(pos = which(search() == "wf4ni_shims"))

}

# Create the folder for temporary results
.create_tempdir <- function() {

  dir.create(path = tempdir(),
             recursive = TRUE,
             showWarnings = FALSE)

}

