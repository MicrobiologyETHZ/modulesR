pkg.env <- new.env(parent = emptyenv())
assign("modules", c(), pkg.env)

#' @export
system <- function (command, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL, show.output.on.console = TRUE, minimized = FALSE, invisible = TRUE, timeout = 0) 
{
    if (!missing(show.output.on.console) || !missing(minimized) || !missing(invisible)) 
        message("arguments 'show.output.on.console', 'minimized' and 'invisible' are for Windows only")
    if (!is.logical(intern) || is.na(intern)) 
        stop("'intern' must be TRUE or FALSE")
    if (!is.logical(ignore.stdout) || is.na(ignore.stdout)) 
        stop("'ignore.stdout' must be TRUE or FALSE")
    if (!is.logical(ignore.stderr) || is.na(ignore.stderr)) 
        stop("'ignore.stderr' must be TRUE or FALSE")
    if (!is.logical(wait) || is.na(wait)) 
        stop("'wait' must be TRUE or FALSE")
    if (ignore.stdout) 
        command <- paste(command, ">/dev/null")
    if (ignore.stderr) 
        command <- paste(command, "2>/dev/null")
    if (!is.null(input)) {
        if (!is.character(input)) 
            stop("'input' must be a character vector or 'NULL'")
        f <- tempfile()
        on.exit(unlink(f))
        writeLines(input, f)
        command <- paste("<", shQuote(f), command)
    }
    if (!wait && !intern) 
        command <- paste(command, "&")
    command <- paste("module load", paste(pkg.env$modules, collapse=" "), ";", command)
    .Internal(system(command, intern, timeout))
}

#' Load module
#' 
#' Load a module from your system's software stack
#' @param   module    The name of the module you want to load, as normally given to the 'module load' command
#' @return  None
#' @author  Chris Field <fieldc@ethz.ch>
#' @details
#' modulesR does not strictly load any modules into the session that R is running in, but instead creates a hidden list of modules you have loaded or unloaded with the package's commands. The system() function is overwritten so that any modules are loaded before the requested command is executed. This is not efficient for a large number of system calls, where modules should be loaded ahead of R if possible.
#' @export
module_load <- function(module){
    pkg.env$modules <- c(pkg.env$modules, module)
}

#' Unload module
#'
#' Unload a module from your system's software stack
#' @param   module    The name of the module you want to unload, as normally given to the 'module unload' command
#' @return  None
#' @author  Chris Field <fieldc@ethz.ch>
#' @export
module_unload <- function(module){
    if(!module%in%pkg.env$modules){
        stop(paste(module, "is not loaded"))
    }
    pkg.env$modules <- pkg.env$modules[pkg.env$modules!=module]
}

#' Unload all modules
#'
#' Unload all modules from your system's software stack
#' @param   None
#' @return  None
#' @author  Chris Field <fieldc@ethz.ch>
#' @export
module_purge <- function(){
    pkg.env$modules <- c()
}

#' List loaded modules
#'
#' List currently loaded modules from your system's software stack
#' @param   None
#' @return  None
#' @author  Chris Field <fieldc@ethz.ch>
#' @export
module_list <- function(){
    system("module list")
}
