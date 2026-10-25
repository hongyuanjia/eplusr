#' @importFrom R6 R6Class
#' @include impl-viewer-webgl.R
NULL

#' Provide On-Demand Result Frames for an `IdfViewer`
#'
#' `IdfViewerFrameProvider` keeps result queries and their DuckDB connections
#' independent from an HTTP framework. Use `$metadata()` to build the browser
#' payload, `$fetch()` in an HTTP adapter to obtain one or more aggregated
#' Float32 frames, and `$close()` when the application no longer serves the
#' viewer.
#'
#' Frame and variable indices are zero-based so an HTTP adapter can pass browser query
#' parameters directly to `$fetch()`. Invalid requests raise an
#' `eplusr_error_viewer_frame` condition with an integer `status` field suitable
#' for the HTTP response status.
#'
#' @docType class
#' @name IdfViewerFrameProvider
#' @seealso [IdfViewer]
#' @author Hongyuan Jia
#' @examples
#' \dontrun{
#' viewer <- idf_viewer(idf)
#' viewer$results(sql, mode = "provider")
#' # The optional loopback connection serves frames without an independent App.
#' widget <- viewer$widget(live = TRUE)
#' viewer$close()
#' }

NULL

# IdfViewerFrameProvider {{{
# Keep the public lifecycle deliberately small so HTTP frameworks remain adapters, not dependencies.
#' @export
IdfViewerFrameProvider <- R6Class(
    "IdfViewerFrameProvider",
    cloneable = FALSE,
    public = list(
        # initialize {{{
        #' @description Create a provider from result specifications and viewer objects.
        #' @param specs Internal result specifications produced by [IdfViewer].
        #' @param objects Internal stable viewer object metadata.
        initialize = function(specs, objects) {
            sources <- list()
            complete <- FALSE
            # Close sources already opened if a later variable cannot be prepared.
            on.exit(
                {
                    if (!complete) {
                        lapply(sources, function(source) source$close())
                    }
                },
                add = TRUE
            )

            for (spec in specs) {
                source <- viewer__frame_source(spec, objects)
                if (!is.null(source)) sources[[length(sources) + 1L]] <- source
            }
            if (!length(sources)) {
                abort("No result variables could be matched to viewer objects.")
            }

            private$m_sources <- sources
            complete <- TRUE
        },
        # }}}

        # metadata {{{
        #' @description Return browser metadata for an HTTP frame endpoint.
        #' @param endpoint URL served by the application for frame requests.
        #' @return A list suitable for the viewer's `results` payload.
        metadata = function(endpoint) {
            private$assert_open()
            assert_string(endpoint, min.chars = 1L)
            variables <- lapply(seq_along(private$m_sources), function(index) {
                source <- private$m_sources[[index]]
                utils::modifyList(
                    source$metadata,
                    list(
                        mode = "provider",
                        endpoint = endpoint,
                        variable = index - 1L,
                        values = ""
                    )
                )
            })
            list(active = 0L, variables = variables)
        },
        # }}}

        # fetch {{{
        #' @description Fetch one frame or the mean of several frames.
        #' @param variable Zero-based variable index, as an integer or query string.
        #' @param frames Zero-based frame indices, supplied as an integer vector or
        #'   comma-separated query string.
        #' @return A raw vector containing one little-endian Float32 value per
        #'   viewer object.
        fetch = function(variable = 0L, frames = 0L) {
            private$assert_open()
            variable <- viewer__parse_viewer_variable_index(
                variable,
                length(private$m_sources)
            )
            source <- private$m_sources[[variable]]
            frames <- viewer__parse_viewer_frame_indices(
                frames,
                source$metadata$shape[[1L]]
            )
            values <- source$fetch(frames)
            # an HTTP adapter can send this raw vector directly as application/octet-stream.
            writeBin(as.numeric(values), raw(), size = 4L, endian = "little")
        },
        # }}}

        # close {{{
        #' @description Close DuckDB connections owned by this provider.
        #' @return The provider, invisibly.
        close = function() {
            private$close_sources()
            invisible(self)
        },
        # }}}

        # print {{{
        #' @description Print a concise provider summary.
        print = function() {
            cli::cat_rule("EnergyPlus Viewer Frame Provider", line = 1)
            cli::cat_line(sprintf(
                " %s %s variable%s (%s)",
                cli::symbol$bullet,
                length(private$m_sources),
                if (length(private$m_sources) == 1L) "" else "s",
                if (private$m_closed) "closed" else "open"
            ))
            invisible(self)
        }
        # }}}
    ),

    private = list(
        m_sources = list(),
        m_closed = FALSE,

        # assert_open {{{
        # Reject requests after cleanup so stale HTTP adapters fail predictably.
        assert_open = function() {
            if (private$m_closed) {
                viewer__abort_viewer_frame(
                    "The viewer frame provider is closed.",
                    410L
                )
            }
            invisible()
        },
        # }}}

        # close_sources {{{
        # Make explicit close calls and garbage-collection finalization idempotent.
        close_sources = function() {
            if (private$m_closed) {
                return(invisible())
            }
            private$m_closed <- TRUE
            lapply(private$m_sources, function(source) source$close())
            invisible()
        },
        # }}}

        # finalize {{{
        # Release analytical connections even if the application drops its last provider reference.
        finalize = function() private$close_sources()
        # }}}
    )
)
# }}}

# vim: set fdm=marker:
