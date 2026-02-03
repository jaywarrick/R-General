install_python_ecosystem <- function(project_path = ".",
									 python_version = NULL,
									 packages = NULL,
									 option = c("force-if-different", "force-always", "upgrade-only")) {

	if (!requireNamespace("reticulate", quietly = TRUE)) {
		stop("Please install reticulate: install.packages('reticulate')")
	}

	# Initialize UV project if needed (creates pyproject.toml)
	# ensure_uv_project_initialized(project_path)

	option <- match.arg(option)
	project_path <- normalizePath(project_path, winslash = '/', mustWork = TRUE)
	venv_path <- file.path(project_path, ".venv")

	# Check if environment exists
	env_exists <- dir.exists(venv_path)
	if (env_exists && !is.null(python_version)) {
		# Check current Python version in the venv
		current_version <- get_venv_python_version(venv_path)

		if (!is.null(current_version)) {
			# Normalize version strings for comparison
			requested_normalized <- normalize_python_version(python_version)
			current_normalized <- normalize_python_version(current_version)

			# Check if versions match (major.minor only, ignore patch)
			versions_match <- check_python_versions_match(
				requested_normalized,
				current_normalized
			)

			if (!versions_match) {
				message(sprintf(
					"Python version mismatch! Current: %s, Requested: %s",
					current_version, python_version
				))
				message("Recreating environment with requested Python version...")

				# Remove old environment
				unlink(venv_path, recursive = TRUE)
				env_exists <- FALSE
			}
		}
	}

	# Create environment if needed
	if (!env_exists) {
		message("Creating UV environment...")
		uv_args <- c("venv", venv_path)
		if (!is.null(python_version)) {
			uv_args <- c(uv_args, "--python", python_version)
		}
		system2("uv", uv_args)
	}

	if (!"reticulate" %in% loadedNamespaces()) {
		library(reticulate, lib.loc = globalenv())
	}
	# Set up reticulate
	reticulate::use_virtualenv(venv_path, required = TRUE)

	# Install packages with appropriate flags
	if (!is.null(packages)) {
		install_python_packages(project_path=project_path, packages=packages, option=option)
	}

	# Verify Python version
	final_version <- get_venv_python_version(venv_path)
	message("✓ Environment ready: ", venv_path)
	if (!is.null(final_version)) {
		message("✓ Python version: ", final_version)
	}

	invisible(list(
		venv_path = venv_path,
		python_version = final_version,
		env_recreated = !env_exists  # TRUE if environment was (re)created
	))
}

install_python_packages <- function(project_path='.',
									packages,
									option = c("force-if-different", "force-always", "upgrade-only")) {

	option <- match.arg(option)
	project_path <- normalizePath(project_path)
	venv_path <- file.path(project_path, '.venv')

	installed_packages <- get_installed_python_packages()

	for (pkg in packages) {
		# Check if it's a file
		if (grepl("\\.(whl|tar\\.gz|zip)$", pkg, ignore.case = TRUE)) {
			# Resolve file path
			file_path <- if (file.exists(pkg)) {
				normalizePath(pkg)
			} else if (file.exists(file.path(project_path, "RorVersions", basename(pkg)))) {
				normalizePath(file.path(project_path, "RorVersions", basename(pkg)))
			} else {
				warning("File not found: ", pkg)
				next
			}

			filename <- basename(file_path)

			# Extract package name from filename
			pkg_name <- gsub("-[0-9].*$", "", filename)
			pkg_name <- gsub("\\.(whl|tar\\.gz|zip)$", "", pkg_name, ignore.case = TRUE)

			message("Processing: ", filename, " (package: ", pkg_name, ")")

			# Check if installed
			installed_info <- installed_packages[package==pkg_name] # 0 row table if not found

			# Extract requested version from filename if possible
			version_match <- regmatches(filename, regexpr("-([0-9]+\\.[0-9]+[a-zA-Z0-9._]*)", filename))
			requested_version <- if (length(version_match) > 0) {
				sub("^-", "", version_match[1])
			} else {
				NULL
			}

			# Handle based on option
			if (option == "force-always") {
				message("  Force reinstalling...")
				system2("uv", c("pip", "install", "--force-reinstall", file_path))

			} else if (option == "upgrade-only") {
				message("  Installing/upgrading...")
				system2("uv", c("pip", "install", "--upgrade", file_path))

			} else if (option == "force-if-different") {
				if (nrow(installed_info) > 0) {
					if (!is.null(requested_version) && !is.null(installed_info$version)) {
						if (installed_info$version == requested_version) {
							message("  ✓ Already installed (same version: ", installed_info$version, ")")
							next
						} else {
							message("  Version mismatch - installed: ", installed_info$version,
									", requested: ", requested_version)
							message("  Force reinstalling...")
							system2("uv", c("pip", "install", "--force-reinstall", file_path))
						}
					} else {
						# Can't compare versions, just install normally
						message("  Installing (can't compare versions)...")
						system2("uv", c("pip", "install", file_path))
					}
				} else {
					# Not installed
					message("  Installing...")
					system2("uv", c("pip", "install", file_path))
				}
			}

		} else {
			# It's a PyPI package
			message("Processing: ", pkg)

			# Parse package name (remove version specifiers)
			pkg_name <- gsub("[=<>!].*$", "", pkg)

			# Check if installed
			installed_info <- installed_packages[package==pkg_name]

			# Extract requested version if specified with ==
			if (grepl("==", pkg)) {
				requested_version <- sub(".*==", "", pkg)
			} else {
				requested_version <- NULL
			}

			if (option == "force-always") {
				message("  Force reinstalling...")
				system2("uv", c("pip", "install", "--force-reinstall", pkg))

			} else if (option == "upgrade-only") {
				message("  Installing/upgrading...")
				system2("uv", c("pip", "install", "--upgrade", pkg))

			} else if (option == "force-if-different") {
				if (nrow(installed_info) > 0) {
					if (!is.null(requested_version) && !is.null(installed_info$version)) {
						if (installed_info$version == requested_version) {
							message("  ✓ Already installed (version: ", installed_info$version, ")")
							next
						} else {
							message("  Version mismatch - installed: ", installed_info$version,
									", requested: ", requested_version)
							message("  Force reinstalling...")
							system2("uv", c("pip", "install", "--force-reinstall", pkg))
						}
					} else {
						# No exact version specified or can't compare
						message("  Installing/upgrading...")
						system2("uv", c("pip", "install", pkg))
					}
				} else {
					# Not installed
					message("  Installing...")
					system2("uv", c("pip", "install", pkg))
				}
			}
		}
	}
}

# Helper: Get Python version from virtual environment
get_venv_python_version <- function(venv_path) {
	python_executable <- if (.Platform$OS.type == "windows") {
		file.path(venv_path, "Scripts", "python.exe")
	} else {
		file.path(venv_path, "bin", "python")
	}

	if (!file.exists(python_executable)) {
		return(NULL)
	}

	tryCatch({
		version_output <- system2(python_executable, "--version", stdout = TRUE)
		# Extract version number from output like "Python 3.9.0"
		version <- gsub("^Python ", "", version_output[1])
		return(version)
	}, error = function(e) {
		return(NULL)
	})
}

# Helper: Normalize Python version string
normalize_python_version <- function(version_string) {
	# Handle different version formats
	if (is.null(version_string)) return(NULL)

	# Remove "python" prefix if present
	version_string <- gsub("^python", "", version_string, ignore.case = TRUE)

	# Extract major.minor version (ignore patch, pre-release, etc.)
	matches <- regexpr("^([0-9]+)\\.([0-9]+)", version_string)
	if (matches > 0) {
		return(regmatches(version_string, matches))
	}

	# If format is just "3" or "39", expand it
	if (grepl("^[0-9]+$", version_string)) {
		if (nchar(version_string) == 1) {
			return(paste0(version_string, ".0"))
		} else if (nchar(version_string) == 2) {
			return(paste0(
				substr(version_string, 1, 1),
				".",
				substr(version_string, 2, 2)
			))
		}
	}

	return(version_string)
}

# Helper: Check if Python versions match (major.minor only)
check_python_versions_match <- function(requested, current) {
	if (is.null(requested) || is.null(current)) return(TRUE)

	# Compare major.minor only
	requested_major_minor <- gsub("^([0-9]+\\.[0-9]+).*", "\\1", requested)
	current_major_minor <- gsub("^([0-9]+\\.[0-9]+).*", "\\1", current)

	return(requested_major_minor == current_major_minor)
}

# # Your install_python_packages function (unchanged)
# install_python_packages <- function(packages, venv_path, project_path, upgrade, force) {
# 	for (pkg in packages) {
# 		# Check if it's a file
# 		if (grepl("\\.(whl|tar\\.gz|zip)$", pkg, ignore.case = TRUE)) {
# 			# Resolve file path
# 			file_path <- if (file.exists(pkg)) {
# 				normalizePath(pkg)
# 			} else if (file.exists(file.path(project_path, "RorVersions", basename(pkg)))) {
# 				normalizePath(file.path(project_path, "RorVersions", basename(pkg)))
# 			} else {
# 				warning("File not found: ", pkg)
# 				next
# 			}
#
# 			message("Installing: ", basename(file_path))
# 			uv_args <- c("pip", "install", file_path)
# 			if (force) uv_args <- c(uv_args, "--force-reinstall")
# 			if (upgrade) uv_args <- c(uv_args, "--upgrade")
#
# 		} else {
# 			# It's a PyPI package - use uv pip install for everything
# 			message("Installing: ", pkg)
# 			uv_args <- c("pip", "install", pkg)
# 			if (force) uv_args <- c(uv_args, "--force-reinstall")
# 			if (upgrade) uv_args <- c(uv_args, "--upgrade")
# 		}
#
# 		system2("uv", uv_args)
# 	}
# }

get_installed_python_packages <- function()
{
	ret <- system2("uv", c("pip", "list"), stdout = TRUE, stderr = TRUE)
	if(length(ret) == 0 || all(ret == ''))
	{
		return(data.table(package=character(0), version=character(0)))
	}
	else
	{
		ret <- rbindlist(lapply(strsplit(ret[-(1:2)], "\\s+"), function(x){data.table(package=x[1], version=x[2])}))
	}
	return(ret)
}

# # Helper: Run uv init if no pyproject.toml exists
# ensure_uv_project_initialized <- function(project_path) {
# 	project_path <- normalizePath(project_path)
# 	pyproject_path <- file.path(project_path, "pyproject.toml")
# 	uv_toml_path <- file.path(project_path, "uv.toml")
#
# 	if (!file.exists(pyproject_path) && !file.exists(uv_toml_path)) {
# 		message("Initializing UV project...")
#
# 		# Change to project directory
# 		old_wd <- getwd()
# 		on.exit(setwd(old_wd))
# 		setwd(project_path)
#
# 		# Run uv init
# 		result <- system2("uv", c("init", "--quiet"))
#
# 		if (result != 0 || (!file.exists(pyproject_path) && !file.exists(uv_toml_path))) {
# 			# uv init failed, create minimal pyproject.toml
# 			stop("Failed to initialize uv on project directory")
# 		}
#
# 		message("✓ Project initialized")
# 	}
# }
