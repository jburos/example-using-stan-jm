
CACHE_DIR <- '.Rcache'
with_filecache <- function(expr, filename, cache_dir=CACHE_DIR, parse=NULL) {
    ## prepare expr for evaluation
    expr <- substitute(expr)
    if (is.null(parse)) {
        if ("character" %in% class(expr)) {
            parse=TRUE;
            warning("Detected a character expr; consider wrapping in {} instead of quotes.");
        } else {
            parse=FALSE;
        }
    }
    
    cache_file <- file.path(cache_dir, filename)
    if (!dir.exists(cache_dir)) {
        warning('Cache directory does not exist. Creating one.')
        dir.create(cache_dir, recursive=TRUE)
    }
    if (file.exists(cache_file)) {
        try({obj <- readRDS(cache_file)})
        if (!inherits(obj, 'try-error')) {
            return(obj)
        } else {
            warning('Error reading RDS file -- re-executing expr')
        }
    }
    
    ## evaluate expr
    if (parse) {
        obj = eval(parse(text=expr), envir = parent.frame());	
    } else {
        obj = eval(expr, envir = parent.frame())
    }
    
    saveRDS(obj, file = cache_file, compress = TRUE)
    return(obj)
}

