#' @title Build a local crew controller for a target pipeline
#' @description
#' Return a `{crew}` controller that runs targets on local worker
#' processes, or `NULL` for a sequential run, for
#' `targets::tar_option_set(controller = ...)`.
#' @param n_workers
#' Positive whole number. Number of local worker processes; `1` means a
#' sequential run in the main pipeline process.
#' @param seconds_idle
#' Positive number. Seconds a worker may stay idle before it exits.
#' Default `30`.
#' @return
#' `NULL` when `n_workers` is `1`, otherwise a controller from
#' `crew::crew_controller_local()` with `n_workers` workers that collects
#' garbage before each task.
#' @details
#' Building the controller starts no worker; `targets::tar_make()` starts
#' and stops them. Each worker is a new R session started in the project
#' folder, so it uses the project library.
#' @seealso [crew::crew_controller_local()], [run_target_pipeline()]
#' @examples
#' build_crew_controller(n_workers = 1)
build_crew_controller <- function(
  n_workers,
  seconds_idle = 30
) {
  assertthat::assert_that(
    base::is.numeric(n_workers),
    base::length(n_workers) == 1L,
    !base::is.na(n_workers),
    n_workers >= 1,
    n_workers == base::round(n_workers),
    msg = "`n_workers` must be one positive whole number."
  )

  assertthat::assert_that(
    base::is.numeric(seconds_idle),
    base::length(seconds_idle) == 1L,
    base::is.finite(seconds_idle),
    seconds_idle > 0,
    msg = "`seconds_idle` must be one positive number."
  )

  if (
    n_workers == 1
  ) {
    return(NULL)
  }

  res <-
    crew::crew_controller_local(
      workers = base::as.integer(n_workers),
      seconds_idle = seconds_idle,
      garbage_collection = TRUE
    )

  return(res)
}
