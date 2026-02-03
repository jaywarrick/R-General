library(shiny)
library(shinyjs)
library(later)

# --- 3. UI MODULES ---

# Dual-State Button Module
dual_state_button_ui <- function(id, width='100%') {
	ns <- NS(id)
	actionButton(ns("btn"), "Run", class = "btn-primary", style = "min-width: 100px;", width=width)
}

dual_state_button_server <- function(id, is_running_rv, is_terminating_rv) {
	moduleServer(id, function(input, output, session) {
		observe({
			if (is_terminating_rv()) {
				updateActionButton(session, "btn", label = "Stop", icon = icon("spinner", class = "fa-spin"))
				shinyjs::disable("btn")
			} else if (is_running_rv()) {
				updateActionButton(session, "btn", label = "Stop", icon = icon("stop"))
				shinyjs::removeCssClass("btn", "btn-primary")
				shinyjs::addCssClass("btn", "btn-danger")
				shinyjs::enable("btn")
			} else {
				updateActionButton(session, "btn", label = "Run", icon = icon("play"))
				shinyjs::removeCssClass("btn", "btn-danger")
				shinyjs::addCssClass("btn", "btn-primary")
				shinyjs::enable("btn")
			}
		})
		return(reactive(input$btn))
	})
}

loop <- function(fun, session, counter=1, n, delay, final_callback=function(){}, is_terminated=function(){F}, ...)
{
	if(is.null(session) || session$closed)
	{
		message("Session ended, stopping instrument polling")
		final_callback()
		return()
	}

	if(isolate(is_terminated()))
	{
		message("Loop canceled.")
		final_callback()
		return()
	}

	if(counter < n)
	{
		# Setup next call right away to keep timing as close to the desired interval as possible
		later::later(function() loop(fun=fun,
									 session=session,
									 counter=counter + 1,
									 n=n, delay=delay,
									 final_callback=final_callback,
									 is_terminated=is_terminated,
									 ...),
					 delay=delay)
		# Call the function to do the work associated with this iteration of the loop
		message(paste('Performing Loop #', counter, sep=''))
		fun(counter=counter, ...)
	}
	else
	{
		# Don't setup up another iteration
		# Call the function to do the work associated with this iteration of the loop
		fun(counter=counter, ...)
		# Finish the loop with a call to the final_callback() function
		final_callback()
	}
}

# --- 1. GLOBAL OUTPUT MIRRORING UTILITY ---
# Allows print(), message(), and warning() to be mirrored to the UI log.

.logger_env <- new.env(parent = emptyenv())
.logger_env$active_logger <- NULL

register_logger <- function(log_fun) {
	.logger_env$active_logger <- log_fun
}

unregister_logger <- function() {
	.logger_env$active_logger <- NULL
}

# Overwrites using base:: calls to prevent infinite recursion
print <- function(x, ...) {
	if (!is.null(.logger_env$active_logger)) {
		.logger_env$active_logger$add(as.character(x))
	}
	base::print(x, ...)
}

message <- function(..., domain = NULL, appendLF = TRUE) {
	msg <- paste(..., sep = "")
	if (!is.null(.logger_env$active_logger)) {
		.logger_env$active_logger$add(msg)
	}
	base::message(..., domain = domain, appendLF = appendLF)
}

warning <- function(..., call. = TRUE, immediate. = FALSE, noBreaks. = FALSE, domain = NULL) {
	msg <- paste(..., sep = "")
	if (!is.null(.logger_env$active_logger)) {
		.logger_env$active_logger$add(paste0("WARNING: ", msg))
	}
	base::warning(..., call. = call., immediate. = immediate., noBreaks. = noBreaks., domain = domain)
}

#' Log Console UI
#' @param id Module ID
#' @param height CSS height property
log_console_ui <- function(id, height = "450px") {
	ns <- NS(id)
	wellPanel(
		style = sprintf("height: %s; display: flex; flex-direction: column; overflow: hidden;", height),
		tags$style(HTML(sprintf("
      #%s {
        flex-grow: 1;
        overflow-y: auto;
        background-color: #f8f9fa;
        border: 1px solid #ced4da;
        font-size: 12px;
        padding: 10px;
        line-height: 1.4;
        margin-top: 5px;
        margin-bottom: 0;
      }
    ", ns("status_log")))),
		verbatimTextOutput(ns("status_log"))
	)
}

#' Log Console Server
#' @param id Module ID
#' @param max_lines Maximum lines to retain in the buffer
#' @return A list containing:
#'   - add: function(msg) to append a timestamped log
#'   - scroll: function(target_session) to force a scroll to the bottom
log_console_server <- function(id, max_lines = 100) {
	moduleServer(id, function(input, output, session) {
		log_buffer <- reactiveVal(character(0))
		output$status_log <- renderText({ paste(log_buffer(), collapse = "\n") })

		# Helper function to trigger the scroll
		# Accepts an optional target_session to handle async calls via later::later
		force_scroll <- function(target_session = session) {
			shiny::withReactiveDomain(target_session, {
				shinyjs::runjs(sprintf('
          setTimeout(function() {
            var elem = document.getElementById("%s");
            if (elem) {
              elem.scrollTop = elem.scrollHeight;
            }
          }, 50);
        ', target_session$ns("status_log")))
			})
		}

		# "Smart" Autoscroll Observer (runs on content update)
		observe({
			log_buffer()
			# Standard reactive context, usually safe without withReactiveDomain
			shinyjs::runjs(sprintf('
        setTimeout(function() {
          var elem = document.getElementById("%s");
          if (elem) {
            var isAtBottom = (elem.scrollHeight - elem.scrollTop - elem.clientHeight) < 100;
            if (isAtBottom) {
              elem.scrollTop = elem.scrollHeight;
            }
          }
        }, 50);
      ', session$ns("status_log")))
		})

		add_log <- function(msg) {
			current_log <- isolate(log_buffer())
			time_stamp <- format(Sys.time(), "%H:%M:%S")
			timestamped_msg <- paste0("[", time_stamp, "] ", msg)
			new_log <- c(current_log, timestamped_msg)
			if (length(new_log) > max_lines) new_log <- tail(new_log, max_lines)
			log_buffer(new_log)
		}

		return(list(
			add = add_log,
			scroll = force_scroll
		))
	})
}

#' Simplified Sequential Task Scheduler with Manager Pattern
#'
#' This version implements a 'Task Manager' and an object-oriented
#' 'task_list' that uses methods to add tasks, simplifying the API
#' and retaining all cancellation logic.


#' Create a new task list
#' @param ... Optional default parameters to store in the chain
#' @return A task_list object with methods: add_task, get_tasks, and get_params.
task_list <- function(...) {
	# Internal state container
	state <- new.env(parent = emptyenv())
	state$tasks <- list()
	state$params <- list(...)

	# Create the object structure
	self <- list()

	#' Add a task to the chain
	#' @param name Optional name for logging
	#' @param expr Code to run for this task. Call mark_done() to proceed.
	self$add_task <- function(name = NULL, expr = {mark_done()}, enabled=T) {
		e_expr <- substitute(expr)

		task <- list(
			expr = e_expr,
			name = name %||% paste0("Task_", length(state$tasks) + 1),
			enabled = enabled
		)

		state$tasks[[length(state$tasks) + 1]] <- task
		# Return self invisibly to allow chaining: chain$add_task()$add_task()
		return(invisible(self))
	}

	#' Internal accessors for the runner
	self$get_tasks <- function() state$tasks
	self$get_params <- function() state$params

	return(self)
}

#' Helper for default names
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' Create a Task Manager
#' Captures Shiny session state and shared reactive values once.
#' @return A list containing a 'run' function.
create_task_manager <- function(session,
								stop_flag_rv = function(new_val=FALSE){return(new_val)},
								is_running_rv = function(new_val=TRUE){return(new_val)},
								is_terminating_rv = function(new_val=FALSE){return(new_val)}) {

	list(
		reset = function(){
			stop_flag_rv(FALSE)
			is_running_rv(FALSE)
			is_terminating_rv(FALSE)
		},
		run = function(task_list, ..., on_terminated = {}, final_callback = {}) {

			on_termination_expr <- substitute(on_terminated)
			final_expr <- substitute(final_callback)

			# Extract data from the task_list object
			tasks <- task_list$get_tasks()

			# --- Internal Detection Helpers ---

			# Recursively check if the expression contains the symbol 'mark_done'
			has_mark_done <- function(expr) {
				if (is.symbol(expr)) return(identical(expr, quote(mark_done)))
				if (is.call(expr)) {
					return(any(vapply(as.list(expr), has_mark_done, logical(1))))
				}
				return(FALSE)
			}

			# Print a hint if the user forgot mark_done
			check_for_mark_done <- function(expr, name) {
				if (!has_mark_done(expr)) {
					message(sprintf("[### Note: No call to mark_done() was detected in %s. ###]", name))
				}
			}

			# 1. State Tracking (Local to this specific run execution)
			state <- new.env(parent = emptyenv())
			state$is_active <- TRUE
			state$current_step_idx <- 1

			run_params <- utils::modifyList(task_list$get_params(), list(...))
			# Doing this is useful if we want final_callback to default to {stop_flag_rv(FALSE); is_running_rv(FALSE);	is_terminating_rv(FALSE)}
			# run_params$stop_flag_rv <- stop_flag_rv
			# run_params$is_running_rv <- is_running_rv
			# run_params$is_terminating_rv <- is_terminating_rv
			run_params$is_terminated <- function() !state$is_active

			caller_env <- parent.frame()

			# 2. Note to the user which tasks don't call mark_done
			for (t in tasks) {
				check_for_mark_done(t$expr, t$name)
			}

			# 3. Lifecycle Helpers
			check_termination <- function() {
				if (!state$is_active || session$isClosed()) return(TRUE)
				res <- tryCatch({ shiny::isolate(stop_flag_rv()) }, error = function(e) FALSE)
				return(isTRUE(res))
			}

			handle_termination <- function(reason = "User interrupted") {
				if (!state$is_active) return(NULL)
				state$is_active <- FALSE
				message(sprintf("[### Task List Terminated: %s ###]", reason))

				if (!session$isClosed()) {
					tryCatch({
						eval(on_termination_expr, envir = run_params, enclos = caller_env)
						# is_running_rv(FALSE)
						# is_terminating_rv(FALSE)
					}, error = function(e) base::warning("[### Cancel Error ###]", e$message))
					tryCatch({
						eval(final_expr, envir = run_params, enclos = caller_env)
					}, error = function(e) base::warning("[### Final Callback Error ###]", e$message))
				}
				return(NULL)
			}

			# 4. Step Executor
			execute_task <- function(idx) {
				if (!state$is_active || check_termination()) return(handle_termination())

				if (idx > length(tasks)) {
					state$is_active <- FALSE
					if(tasks[[length(tasks)]]$enabled)
					{
						message(sprintf("[--- %s Complete ---]", tasks[[length(tasks)]]$name))
					}
					message("[### Completed Task List ###]")

					if (!session$isClosed()) {
						tryCatch({
							eval(final_expr, envir = run_params, enclos = caller_env)
						}, error = function(e) base::warning("[### Final Callback Error ###]", e$message))
					}
					return(invisible(NULL))
				}

				task <- tasks[[idx]]
				state$current_task_idx <- idx

				# Communicate transitions between tasks
				# Mark the previous task complete if it was enabled
				if(idx > 1 && tasks[[idx-1]]$enabled)
				{
					message(sprintf("[--- %s Complete ---]", tasks[[idx-1]]$name))
				}

				# Inject 'mark_done' function for this specific task
				run_params$mark_done <- function() {
					if (state$is_active && state$current_task_idx == idx) {
						execute_task(idx + 1)
					}
				}

				tryCatch({
					if(task$enabled)
					{
						message(sprintf("[--- %s Started ---]", task$name))
						eval(task$expr, envir = run_params, enclos = caller_env)
					} else {
						message(sprintf("[--- %s Skipped ---]", task$name))
						run_params$mark_done()
					}
				}, error = function(e) {
					warning(sprintf("[### Error in %s: %s ###]", task$name, e$message))
					run_params$mark_done() # Proceed anyway to prevent hanging
				})
			}

			# Watchdog for termination
			watchdog <- function() {
				if (!state$is_active) return(NULL)
				if (check_termination()) {
					handle_termination("Termination detected by watchdog")
				} else {
					later::later(watchdog, delay = 2)
				}
			}

			message("[### Starting Task List ###]")
			watchdog()
			execute_task(1)
		}
	)
}

# --- EXAMPLE SHINY APP MAIN ---

ui <- fluidPage(
	theme = shinythemes::shinytheme("spacelab"),

	##### CRITICAL STEP FOR CONSOLE #####
	useShinyjs(),

	titlePanel("Integrated Task Chain & Global Mirroring"),
	sidebarLayout(
		sidebarPanel(
			dual_state_button_ui("task_btn"),
			hr(),
			actionButton('trigger_scroll', "Trigger Scroll"),
			hr(),
			log_console_ui("my_log", height = "200px"),
			helpText("This demo app uses a global output mirror. Standard R print(),
               message(), and warning() calls within tasks are automatically
               captured by the log widget below.")
		),
		mainPanel(

		)
	)
)

# tl2 <- task_list()
# tl2$add_task(
# 	"Inner Task 1", {
# 		print('Message 1')
# 		later::later(mark_done, 4)
# 	}
# )$add_task(
# 	"Inner Task 2", {
# 		print('Message 2')
# 		later::later(mark_done, 4)
# 	}
# )
#
# server <- function(input, output, session) {
# 	# Shared State
# 	is_running_rv     <- reactiveVal(FALSE)
# 	is_terminating_rv <- reactiveVal(FALSE)
# 	stop_flag_rv      <- reactiveVal(FALSE)
#
# 	tm <- create_task_manager(session=session,
# 							  stop_flag_rv=stop_flag_rv,
# 							  is_running_rv=is_running_rv,
# 							  is_terminating_rv=is_terminating_rv)
#
# 	# 1. Initialize log console module
# 	logger_ctrl <- log_console_server("my_log", max_lines = 100)
#
# 	# 2. Register the module's log function globally
# 	register_logger(logger_ctrl)
#
# 	# 3. Module logic for button
# 	btn_trigger <- dual_state_button_server("task_btn", is_running_rv, is_terminating_rv)
#
# 	# Clean up global registration when session ends
# 	session$onSessionEnded(function() {
# 		unregister_logger()
# 		stopApp()
# 	})
#
# 	observeEvent(input$trigger_scroll, {
# 		logger_ctrl$scroll()
# 	})
#
# 	observeEvent(btn_trigger(), {
# 		if (!is_running_rv()) {
# 			is_running_rv(TRUE)
#
# 			# Define a chain with parameters (comms)
# 			my_chain <- task_list()
# 			my_chain$add_task(
# 				"Initialization",
# 				{
# 					print("Task 1: Loading resources...")
# 					later::later(function(){message('Hi'); mark_done()}, 4)
# 				}
# 			)$add_task(
# 				"Simulation",
# 				{
# 					print("Starting first simulation...")
# 					later::later(mark_done, 4)
# 				},
# 				TRUE
# 			)$add_task(
# 				"TESTING", {
# 					tm$run(tl2, final_callback={mark_done()})
# 				}
# 			)$add_task(
# 				"Simulation",
# 				{
# 					print("Starting other simulation...")
# 					later::later(mark_done, 4)
# 				},
# 				TRUE
# 			)$add_task(
# 				"Cleanup",
# 				{
# 					# browser()
# 					loop(function(counter){print(counter)},
# 						 session=session,
# 						 n=5,
# 						 delay=3,
# 						 final_callback = mark_done,
# 						 is_terminated = is_terminated)
# 				}
# 			)
#
# 			# Start execution
# 			tm$run(my_chain,
# 				   on_terminated = {print("TERMINATED: Cleaning up session state...")},
# 				   final_callback = {tm$reset()} ### This resets the tm flags for managing termination (only do this on the outermost run() function, not inner calls to run())
# 			)
# 		} else {
# 			# Trigger cancellation
# 			print("User requested to stop Execution.")
# 			is_terminating_rv(TRUE)
# 			stop_flag_rv(TRUE)
# 		}
# 	})
# }
#
# shinyApp(ui, server)
