#' @include eplusr.R
# for cloning Idd and Idf objects
.globals$env_cloned <- list()
.globals$is_env_cloned <- list()
.globals$is_gen_cloned <- list()
.globals$is_env_assigned <- list()

# reset_clone_indicator {{{
reset_clone_indicator <- function () {
    for (type in c("Idf", "Idd", "IddObject", "IdfObject")) {
        env <- name_env_shared(type)
        gen <- name_gen_shared(type)

        .globals$env_cloned[[type]] <- list()
        .globals$is_env_cloned[[type]] <- list()
        .globals$is_env_assigned[[type]] <- list()
        .globals$is_gen_cloned[[type]] <- list()

        for (nm in env) {
            .globals$env_cloned[[type]][[nm]] <- NULL
            .globals$is_env_cloned[[type]][[nm]] <- FALSE
            .globals$is_env_assigned[[type]][[nm]] <- FALSE
        }

        for (nm in gen) {
            .globals$is_gen_cloned[[type]][[nm]] <- FALSE
        }
    }
}
# }}}
# name_env_shared {{{
name_env_shared <- function (type = c("Idf", "Idd", "IdfObject", "IddObject", "Epw")) {
    type <- match.arg(type)
    switch(type,
        Idf = c("m_idd_env", "m_idf_env", "m_log"),
        IdfObject = c("m_idd_env", "m_idf_env", "m_log"),
        Idd = c("m_idd_env"),
        IddObject = c("m_idd_env", "m_idf_env"),
        Epw = c("m_log")
    )
}
# }}}
# name_gen_shared {{{
name_gen_shared <- function (type = c("Idf", "Idd", "IdfObject", "IddObject")) {
    type <- match.arg(type)
    switch(type,
        Idf       = c("m_iddobj_gen", "m_idfobj_gen"),
        IdfObject = c("m_iddobj_gen", "m_idfobj_gen"),
        IddObject = c("m_iddobj_gen", "m_idfobj_gen"),
        Idd       = c("m_iddobj_gen")
    )
}
# }}}
# deep_clone {{{
deep_clone <- function (env, name, value, type) {
    env_shared <- name_env_shared(type)

    # normal_copy {{{
    normal_copy <- function (name, value) {
        if (inherits(value, "R6")) {
            value$clone(deep = TRUE)
        } else if (is.environment(value)) {
            list2env(as.list.environment(value, all.names = TRUE), parent = emptyenv())
        } else {
            value
        }
    }
    # }}}

    # copy_to_globals {{{
    copy_to_globals <- function (name, value) {
        if (name %in% env_shared) {
            if (!.globals$is_env_cloned[[type]][[name]]) {
                val <- normal_copy(name, value)
                .globals$env_cloned[[type]][[name]] <- val
                .globals$is_env_cloned[[type]][[name]] <- TRUE
            } else {
                val <- .globals$env_cloned[[type]][[name]]
            }
            val
        }
    }
    # }}}

    # assign_shared {{{
    assign_shared <- function (name, gen) {
        for (nm in env_shared) {
            gen$self$private_fields[[nm]] <- .globals$env_cloned[[type]][[nm]]
            gen$private_fields[[nm]] <- .globals$env_cloned[[type]][[nm]]
        }

        # self reference
        gen$self$private_fields[[name]] <- gen
        gen$self$self$private_fields[[name]] <- gen

        gen
    }
    # }}}

    # done_copy {{{
    done_copy <- function () {
        all(unlist(.globals$is_env_cloned[[type]], use.names = FALSE),
            unlist(.globals$is_gen_cloned[[type]], use.names = FALSE)
        )
    }
    # }}}

    # done_assign {{{
    done_assign <- function () all(unlist(.globals$is_env_assigned[[type]]), use.names = FALSE)
    # }}}

    if (inherits(value, "R6ClassGenerator")) {
        copied <- unlist(.globals$is_env_cloned[[type]], use.names = TRUE)
        if (any(!copied)) {
            for (nm in names(!copied)) {
                copy_to_globals(nm, env[[nm]])
            }
            deep_clone(env, name, value, type)
        } else {
            # clone the R6ClassGenerator
            value <- clone_generator(value)
            value <- assign_shared(name, value)
            .globals$is_gen_cloned[[type]][[name]] <- TRUE
            if (done_copy() && done_assign()) reset_clone_indicator()
            value
        }
    } else {
        if (name %in% env_shared) {
            value <- copy_to_globals(name, value)
            .globals$is_env_assigned[[type]][[name]] <- TRUE
            if (done_copy() && done_assign()) reset_clone_indicator()
            value
        } else {
            normal_copy(name, value)
        }
    }
}
# }}}
