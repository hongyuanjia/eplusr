library(R6)
IDF <- R6Class(classname = "IDF",
               public = list(
                   path = NULL,
                   content = NULL,
                   ver = NULL,
                   initialize = function(path = NULL) {
                       if (is.null(path)) {
                           stop("No idf path given.", call. = FALSE)
                       }
                       self$content <- eplusr::read_idf_lines(path)
                       self$ver <- get_idf_ver(idf_lines = self$content)
                   },

                   parse = function() {
                       get_idf_object(self$content)
                   }
               )
)
