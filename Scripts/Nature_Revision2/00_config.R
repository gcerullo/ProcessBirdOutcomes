# =============================================================================
# Self-notes — NR2 bird pipeline
# =============================================================================
# What I'm doing: I keep one place for Nature Revision 2 output paths (`Outputs/NR2/...`)
#   and small helpers so other NR2 scripts don't hard-code folders.
#
# What I need (inputs): Nothing to run this file alone; other scripts `source()` me and pass
#   a project root if they want.
#
# What I produce (outputs): Functions return path lists; `nr2_init()` creates `models`, `rds`,
#   and `figures` under `Outputs/NR2` when I call it.
# =============================================================================

# Nature Revision 2 shared configuration
# Single setup file for paths, folder creation, and file-copy helpers.

nr2_get_paths <- function(project_root = ".") {
  base <- normalizePath(project_root, winslash = "/", mustWork = FALSE)

  outputs_root <- file.path(base, "Outputs", "NR2")
  models_dir <- file.path(outputs_root, "models")
  rds_dir <- file.path(outputs_root, "rds")
  figures_dir <- file.path(outputs_root, "figures")

  list(
    base = base,
    scripts_dir = file.path(base, "Scripts"),
    outputs_root = outputs_root,
    models_dir = models_dir,
    rds_dir = rds_dir,
    figures_dir = figures_dir
  )
}

nr2_create_output_dirs <- function(project_root = ".") {
  paths <- nr2_get_paths(project_root)

  dir.create(paths$outputs_root, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$models_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$rds_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$figures_dir, recursive = TRUE, showWarnings = FALSE)

  invisible(paths)
}

nr2_init <- function(project_root = ".", verbose = TRUE) {
  paths <- nr2_create_output_dirs(project_root)

  if (isTRUE(verbose)) {
    message("NR2 folders ready:")
    message(" - ", paths$outputs_root)
    message(" - ", paths$models_dir)
    message(" - ", paths$rds_dir)
    message(" - ", paths$figures_dir)
  }

  invisible(paths)
}

nr2_copy_if_exists <- function(from, to) {
  if (!file.exists(from)) {
    message("NR2 note: source file not found, skipped -> ", from)
    return(invisible(FALSE))
  }

  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  file.copy(from, to, overwrite = TRUE)
  message("NR2 copied: ", from, " -> ", to)
  invisible(TRUE)
}
