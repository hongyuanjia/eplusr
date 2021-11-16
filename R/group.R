#' @importFrom R6 R6Class
#' @importFrom cli cat_boxx cat_line cat_rule
#' @importFrom data.table rbindlist set setattr setcolorder
#' @importFrom tools file_path_sans_ext
NULL

#' Create and Run Parametric Analysis, and Collect Results
#'
#' `EplusGroupJob` class is a wrapper of [run_multi()] and provides an interface
#' to group multiple EnergyPlus simulations together for running and collecting
#' outputs.
#'
#' @docType class
#' @name EplusGroupJob
#' @author Hongyuan Jia
NULL

#' @export
# EplusGroupJob {{{
EplusGroupJob <- R6::R6Class(classname = "EplusGroupJob", cloneable = FALSE,
    public = list(

        # INITIALIZE {{{
        #' @description
        #' Create an `EplusGroupJob` object
        #'
        #' @param idfs Paths to EnergyPlus IDF files or a list of IDF files and
        #'        [Idf] objects. If only one IDF supplied, it will be used for
        #'        simulations with all EPWs.
        #' @param epws Paths to EnergyPlus EPW files or a list of EPW files and
        #'        [Epw] objects. Each element in the list can be `NULL`, which
        #'        will force design-day-only simulation. Note this needs at
        #'        least one `Sizing:DesignDay` object exists in that [Idf]. If
        #'        `epws` is `NULL`, design-day-only simulation will be conducted
        #'        for all input models. If only one EPW supplied, it will be
        #'        used for simulations with all IDFs.
        #'
        #' @return An `EplusGroupJob` object.
        #'
        #' @examples
        #' \dontrun{
        #' if (is_avail_eplus(8.8)) {
        #'     dir <- eplus_config(8.8)$dir
        #'     path_idfs <- list.files(file.path(dir, "ExampleFiles"), "\\.idf",
        #'         full.names = TRUE)[1:5]
        #'     path_epws <- list.files(file.path(dir, "WeatherData"), "\\.epw",
        #'         full.names = TRUE)[1:5]
        #'
        #'     # create from local files
        #'     group <- group_job(path_idfs, path_epws)
        #'
        #'     # create from Idfs and Epws object
        #'     group_job(lapply(path_idfs, read_idf), lapply(path_epws, read_epw))
        #' }
        #' }
        #'
        initialize = function (idfs, epws) {
            # add Output:SQLite and Output:VariableDictionary if necessary
            input <- get_epgroup_input(idfs, epws, sql = TRUE, dict = TRUE)

            private$m_idfs <- input$idfs
            private$m_epws_path <- input$epws
            # log if the input idf has been changed
            private$m_log <- new.env(hash = FALSE, parent = emptyenv())
            private$m_log$unsaved <- input$sql | input$dict

            # save uuid
            private$log_idf_uuid()
            private$log_new_uuid()
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        # run {{{
        #' @description
        #' Run grouped simulations
        #'
        #' @details
        #' `$run()` runs all grouped simulations in parallel. The number of
        #' parallel EnergyPlus process can be controlled by
        #' `eplusr_option("num_parallel")`. If `wait` is FALSE, then the job
        #' will be run in the background. You can get updated job status by just
        #' printing the `EplusGroupJob` object.
        #'
        #' @param dir The parent output directory for specified simulations.
        #'        Outputs of each simulation are placed in a separate folder
        #'        under the parent directory.
        #' @param wait If `TRUE`, R will hang on and wait all EnergyPlus
        #'        simulations finish. If `FALSE`, all EnergyPlus simulations are
        #'        run in the background.  Default: `TRUE`.
        #' @param force Only applicable when the last simulation runs with
        #'        `wait` equals to `FALSE` and is still running. If `TRUE`,
        #'        current running job is forced to stop and a new one will
        #'        start. Default: `FALSE`.
        #' @param copy_external If `TRUE`, the external files that current `Idf`
        #'        object depends on will also be copied into the simulation
        #'        output directory. The values of file paths in the Idf will be
        #'        changed automatically. This ensures that the output directory
        #'        will have all files needed for the model to run. Default is
        #'        `FALSE`.
        #' @param echo Only applicable when `wait` is `TRUE`. Whether to
        #'        simulation status. Default: same as `wait`.
        #' @param separate If `TRUE`, all models are saved in a separate folder
        #'        with each model's name under `dir` when simulation. If `FALSE`,
        #'        all models are saved in `dir` when simulation. Default:
        #'        `TRUE`.
        #' @param readvars If `TRUE`, the `ReadVarESO` post-processor will run
        #'        to generate CSV files from the ESO output. Since those CSV
        #'        files are never used when extracting simulation data in eplusr,
        #'        setting it to `FALSE` can speed up the simulation if there are
        #'        hundreds of output variables or meters. Default: `TRUE`.
        #'
        #' @return The `EplusGroupJob` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # only run design day
        #' group$run(NULL)
        #'
        #' # do not show anything in the console
        #' group$run(echo = FALSE)
        #'
        #' # specify output directory
        #' group$run(tempdir(), echo = FALSE)
        #'
        #' # run in the background
        #' group$run(wait = TRUE, echo = FALSE)
        #' # see group job status
        #' group$status()
        #'
        #' # force to kill background group job before running the new one
        #' group$run(force = TRUE, echo = FALSE)
        #'
        #' # copy external files used in the model to simulation output directory
        #' group$run(copy_external = TRUE, echo = FALSE)
        #' }
        #'
        run = function (dir = NULL, wait = TRUE, force = FALSE, copy_external = FALSE,
                        echo = wait, separate = TRUE, readvars = TRUE)
            epgroup_run(self, private, dir, wait, force, copy_external, echo, separate, readvars),
        # }}}

        # kill {{{
        #' @description
        #' Kill current running jobs
        #'
        #' @details
        #' `$kill()` kills all background EnergyPlus processes that are current
        #' running if possible. It only works when simulations run in
        #' non-waiting mode.
        #'
        #' @return A single logical value of `TRUE` or `FALSE`, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' group$kill()
        #' }
        #'
        kill = function ()
            epgroup_kill(self, private),
        # }}}

        # status {{{
        #' @description
        #' Get the group job status
        #'
        #' @details
        #' `$status()` returns a named list of values indicates the status of the job:
        #'
        #'   * `run_before`: `TRUE` if the job has been run before. `FALSE` otherwise.
        #'   * `alive`: `TRUE` if the job is still running in the background. `FALSE`
        #'     otherwise.
        #'   * `terminated`: `TRUE` if the job was terminated during last
        #'      simulation. `FALSE` otherwise. `NA` if the job has not been run yet.
        #'   * `successful`: `TRUE` if all simulations ended successfully. `FALSE` if
        #'     there is any simulation failed. `NA` if the job has not been run yet.
        #'   * `changed_after`: `TRUE` if the models has been modified since last
        #'      simulation. `FALSE` otherwise.
        #'   * `job_status`: A [data.table::data.table()] contains meta data
        #'     for each simulation job. For details, please see [run_multi()]. If the
        #'     job has not been run before, a [data.table::data.table()]
        #'     with 4 columns is returned:
        #'     - `index`: The index of simulation
        #'     - `status`: The status of simulation. As the simulation has not been run,
        #'       `status` will always be "idle".
        #'     - `idf`: The path of input IDF file.
        #'     - `epw`: The path of input EPW file. If not provided, `NA` will be
        #'       assigned.
        #'
        #' @return A named list of 6 elements.
        #'
        #' @examples
        #' \dontrun{
        #' group$status()
        #' }
        #'
        status = function ()
            epgroup_status(self, private),
        # }}}

        # errors {{{
        #' @description
        #' Read group simulation errors
        #'
        #' @details
        #' $errors() returns a list of [ErrFile][read_err()] objects which
        #' contain all contents of the simulation error files (`.err`). If
        #' `info` is `FALSE`, only warnings and errors are printed.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #' @param info If `FALSE`, only warnings and errors are printed.
        #'        Default: `FALSE`.
        #'
        #' @return A list of [ErrFile][read_err()] objects.
        #'
        #' @examples
        #' \dontrun{
        #' group$errors()
        #'
        #' # show all information
        #' group$errors(info = TRUE)
        #' }
        #'
        errors = function (which = NULL, info = FALSE)
            epgroup_output_errors(self, private, which, info),
        # }}}

        # output_dir {{{
        #' @description
        #' Get simulation output directory
        #'
        #' @details
        #' `$output_dir()` returns the output directory of simulation results.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get output directories of all simulations
        #' group$output_dir()
        #'
        #' # get output directories of specified simulations
        #' group$output_dir(c(1, 4))
        #' }
        #'
        output_dir = function (which = NULL)
            epgroup_output_dir(self, private, which),
        # }}}

        # list_files {{{
        #' @description
        #' List all output files in simulations
        #'
        #' @details
        #' `$list_files()` returns all input and output files for the grouped
        #' EnergyPlus simulations.
        #'
        #' Description of all possible outputs from EnergyPlus can be found in
        #' EnergyPlus documentation "Output Details and Examples".
        #'
        #' Below gives a brief summary on the meaning of elements in the
        #' returned list.
        #'
        #' | #   | Element      | Description                                                           |
        #' | --- | ---          | ---                                                                   |
        #' | 1   | `ads`        | EnergyPlus AirflowNetwork related output                              |
        #' | 2   | `audit`      | EnergyPlus inputs echo                                                |
        #' | 3   | `bnd`        | EnergyPlus branch node details                                        |
        #' | 4   | `bsmt_audit` | Basement input Echo                                                   |
        #' | 5   | `bsmt_csv`   | Basement CSV output                                                   |
        #' | 6   | `bsmt_idf`   | Basement IDF output                                                   |
        #' | 7   | `bsmt_out`   | Basement Output                                                       |
        #' | 8   | `cbor`       | Energyplus CBOR binary output introduced since v9.5                   |
        #' | 9   | `dbg`        | Energyplus debug output                                               |
        #' | 10  | `delight`    | EnergyPlus DElight simulation inputs and outputs                      |
        #' | 11  | `dfs`        | EnergyPlus daylighting factor for exterior windows                    |
        #' | 12  | `dxf`        | EnergyPlus surface drawing output                                     |
        #' | 13  | `edd`        | EnergyPlus EMS report                                                 |
        #' | 14  | `eio`        | EnergyPlus standard and optional reports                              |
        #' | 15  | `end`        | EnergyPlus simulation status in one line                              |
        #' | 16  | `epjson`     | EnergyPlus epJSON input converted from IDF                            |
        #' | 17  | `epmdet`     | EPMacro inputs echo                                                   |
        #' | 18  | `epmidf`     | EPMacro IDF output                                                    |
        #' | 19  | `epw`        | EnergyPlus Weather File input                                         |
        #' | 20  | `err`        | EnergyPlus error summarry                                             |
        #' | 21  | `eso`        | EnergyPlus standard output                                            |
        #' | 22  | `experr`     | ExpandObjects error summary                                           |
        #' | 23  | `expidf`     | ExpandObjects IDF output                                              |
        #' | 24  | `glhe`       | EnergyPlus ground heat exchange file                                  |
        #' | 25  | `idf`        | EnergyPlus IDF input                                                  |
        #' | 26  | `imf`        | EPMacro IMF input                                                     |
        #' | 27  | `iperr`      | convertESOMTR error summary                                           |
        #' | 28  | `ipeso`      | convertESOMTR standard output in IP units                             |
        #' | 29  | `ipmtr`      | convertESOMTR meter output in IP units                                |
        #' | 30  | `json`       | EnergyPlus JSON time series output introduced since v9.5              |
        #' | 31  | `log`        | EnergyPlus log output                                                 |
        #' | 32  | `map`        | EnergyPlus daylighting intensity map output                           |
        #' | 33  | `mdd`        | EnergyPlus meter list                                                 |
        #' | 34  | `meter`      | EnergyPlus meter CSV output                                           |
        #' | 35  | `msgpack`    | EnergyPlus MessagePack binary output introduced since v9.5            |
        #' | 36  | `mtd`        | EnergyPlus meter details                                              |
        #' | 37  | `mtr`        | EnergyPlus meter output                                               |
        #' | 38  | `perflog`    | EnergyPlus log for `PerformancePrecisionTradeoffs                     |
        #' | 39  | `rdd`        | EnergyPlus report variable names                                      |
        #' | 40  | `rvaudit`    | ReadVarsESO input echo                                                |
        #' | 41  | `sci`        | EnergyPlus cost benefit calculation information                       |
        #' | 42  | `screen`     | EnergyPlus window scrren transmittance map output                     |
        #' | 43  | `shading`    | EnergyPlus surface shading CSV output                                 |
        #' | 44  | `shd`        | EnergyPlus surface shading combination report                         |
        #' | 45  | `slab_ger`   | Slab error summary                                                    |
        #' | 46  | `slab_gtp`   | Slab ground temperature output                                        |
        #' | 47  | `slab_out`   | Slab IDF output                                                       |
        #' | 48  | `sln`        | EnergyPlus `Output:Surfaces:List, Lines` output                       |
        #' | 49  | `sqlite`     | EnergyPlus SQLite output                                              |
        #' | 50  | `sqlite_err` | EnergyPlus SQLite error summary                                       |
        #' | 51  | `ssz`        | EnergyPlus system sizing outputs in CSV, TAB or TXT format            |
        #' | 52  | `svg`        | HVAC-Diagram HVAC diagram output                                      |
        #' | 53  | `table`      | EnergyPlus tabular outputs in CSV, TAB, TXT, HTM, or XML format       |
        #' | 54  | `variable`   | EnergyPlus report variable CSV output                                 |
        #' | 55  | `wrl`        | EnergyPlus `Output:Surfaces:List, VRML` output                        |
        #' | 56  | `zsz`        | EnergyPlus system sizing outputs in CSV, TAB or TXT format            |
        #' | 57  | `resource`   | External file resources used for the simulation, e.g. `Schedule:File` |
        #'
        #' @param which An integer vector of the indexes or a character vector
        #' or names of parametric simulations. If `NULL`, results of all
        #' parametric simulations are returned. Default: `NULL`.
        #'
        #' @param simplify If `TRUE`, a list of character vectors of EnergyPlus
        #' input and output file names in the output directory for each
        #' simulation is given. If `FALSE`, a
        #' [data.table][data.table::data.table()] giving all possible input and
        #' output types is given. `NA` is returned if no input or output files
        #' are found for that type. Default: `FALSE`.
        #'
        #' @param full If `TRUE`, the full file paths in the output directory
        #' are returned. Otherwise, only the file names are returned. Default:
        #' `FALSE`.
        #'
        #' @return If simplify is `TRUE`, a list. Otherwise, a
        #' [data.table][data.table::data.table()] of 3 columns:
        #'
        #' * `index`: Integer type. Simulation indices.
        #' * `type`: Character type. Input or output types. See table above for
        #'   the meaning
        #' * `file`: List type. File names if `full` is `FALSE`. Full file paths
        #'   if `full` is `TRUE`
        #'
        #' @examples
        #' \dontrun{
        #' # list all files in the output directory
        #' group$list_files(simplify = TRUE)
        #'
        #' # get a data.table that contains a full list of all possible inputs
        #' # and outputs even though they may not exist for current simulation
        #' group$list_files()
        #'
        #' # return the full paths instead of just file names
        #' group$locate_output(full = TRUE)
        #' }
        #'
        list_files = function (which = NULL, simplify = FALSE, full = FALSE)
            epgroup_list_files(self, private, which, simplify, full),
        # }}}

        # locate_output {{{
        #' @description
        #' Get paths of output file
        #'
        #' @details
        #' `$locate_output()` returns the path of a single output file of specified
        #' simulations.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #' @param suffix A string that indicates the file extension of
        #'        simulation output. Default: `".err"`.
        #' @param strict If `TRUE`, it will check if the simulation was
        #'        terminated, is still running or the file exists or not.
        #'        Default: `TRUE`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' # get the file path of the error file
        #' group$locate_output(c(1, 4), ".err", strict = FALSE)
        #'
        #' # can detect if certain output file exists
        #' group$locate_output(c(1, 4), ".expidf", strict = TRUE)
        #' }
        #'
        locate_output = function (which = NULL, suffix = ".err", strict = TRUE)
            epgroup_locate_output(self, private, which, suffix, strict),
        # }}}

        # list_table {{{
        #' @description
        #' List all table names in EnergyPlus SQL outputs
        #'
        #' @details
        #' `$list_table()` returns a list of character vectors that contain all
        #' available table and view names in the EnergyPlus SQLite files for
        #' group simulations. The list is named using IDF names.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @return A named list of character vectors.
        #'
        #' @examples
        #' \dontrun{
        #' group$list_table(c(1, 4))
        #' }
        #'
        list_table = function (which = NULL)
            epgroup_list_table(self, private, which),
        # }}}

        # read_table {{{
        #' @description
        #' Read the same table from EnergyPlus SQL outputs
        #'
        #' @details
        #' `$read_table()` takes a simulation index and a valid table `name` of
        #' those from
        #' \href{../../eplusr/html/EplusGroupJob.html#method-list_table}{\code{$list_table()}}
        #' and returns that table data in a [data.table::data.table()] format.
        #' The two column will always be `index` and `case` which can be used to
        #' distinguish output from different simulations. `index` contains the
        #' indices of simulated models and `case` contains the model names
        #' without extensions.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #' @param name A single string specifying the name of table to read.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' # read a specific table
        #' group$read_table(c(1, 4), "Zones")
        #' }
        #'
        read_table = function (which = NULL, name)
            epgroup_read_table(self, private, which, name),
        # }}}

        # read_rdd {{{
        #' @description
        #' Read Report Data Dictionary (RDD) files
        #'
        #' @details
        #' `$read_rdd()` return the core data of Report Data Dictionary (RDD)
        #' files. For details, please see [read_rdd()].
        #' The two column will always be `index` and `case` which can be used to
        #' distinguish output from different simulations. `index` contains the
        #' indices of simulated models and `case` contains the model names
        #' without extensions.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' group$read_rdd(c(1, 4))
        #' }
        #'
        read_rdd = function (which = NULL)
            epgroup_read_rdd(self, private, which),
        # }}}

        # read_mdd {{{
        #' @description
        #' Read Meter Data Dictionary (MDD) files
        #'
        #' @details
        #' `$read_mdd()` return the core data of Meter Data Dictionary (MDD)
        #' files. For details, please see [read_mdd()].
        #' The two column will always be `index` and `case` which can be used to
        #' distinguish output from different simulations. `index` contains the
        #' indices of simulated models and `case` contains the model names
        #' without extensions.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' group$read_mdd(c(1, 4))
        #' }
        #'
        read_mdd = function (which = NULL)
            epgroup_read_mdd(self, private, which),
        # }}}

        # report_data_dict {{{
        #' @description
        #' Read report data dictionary from EnergyPlus SQL outputs
        #'
        #' @details
        #' `$report_data_dict()` returns a [data.table::data.table()] which
        #' contains all information about report data.
        #'
        #' For details on the meaning of each columns, please see "2.20.2.1
        #' ReportDataDictionary Table" in EnergyPlus "Output Details and
        #' Examples" documentation.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @return A [data.table::data.table()] of 10 columns:
        #'
        #' * `index`: The index of simulated model. This column can be used
        #'   to distinguish output from different simulations
        #' * `case`: The model name without extension. This column can be used
        #'   to distinguish output from different simulations
        #' * `report_data_dictionary_index`: The integer used to link the
        #'   dictionary data to the variable data. Mainly useful when joining
        #'   different tables
        #' * `is_meter`: Whether report data is a meter data. Possible values:
        #'   `0` and `1`
        #' * `timestep_type`: Type of data timestep. Possible values: `Zone` and
        #'   `HVAC System`
        #' * `key_value`: Key name of the data
        #' * `name`: Actual report data name
        #' * `reporting_frequency`:
        #' * `schedule_name`: Name of the the schedule that controls reporting
        #'     frequency.
        #' * `units`: The data units
        #'
        #' @examples
        #' \dontrun{
        #' group$report_data_dict(c(1, 4))
        #' }
        #'
        report_data_dict = function (which = NULL)
            epgroup_report_data_dict(self, private, which),
        # }}}

        # report_data {{{
        #' @description
        #' Read report data
        #'
        #' @details
        #' `$report_data()` extracts the report data in a
        #' [data.table::data.table()] using key values, variable names and other
        #' specifications.
        #'
        #' `$report_data()` can also directly take all or subset output from
        #' `$report_data_dict()` as input, and extract all data specified.
        #'
        #' The returned column numbers varies depending on `all` argument.
        #'
        #' * `all` is `FALSE`, the returned [data.table::data.table()] has 6 columns:
        #'   * `index`: The index of simulated model. This column can be used
        #'     to distinguish output from different simulations
        #'   * `case`: The model name. This column can be used to distinguish
        #'     output from different simulations
        #'   * `datetime`: The date time of simulation result
        #'   * `key_value`: Key name of the data
        #'   * `name`: Actual report data name
        #'   * `units`: The data units
        #'   * `value`: The data value
        #' * `all` is `TRUE`, besides columns described above, extra columns are also
        #'   included:
        #'   * `month`: The month of reported date time
        #'   * `day`: The day of month of reported date time
        #'   * `hour`: The hour of reported date time
        #'   * `minute`: The minute of reported date time
        #'   * `dst`: Daylight saving time indicator. Possible values: `0` and `1`
        #'   * `interval`: Length of reporting interval
        #'   * `simulation_days`: Day of simulation
        #'   * `day_type`: The type of day, e.g. `Monday`, `Tuesday` and etc.
        #'   * `environment_period_index`: The indices of environment.
        #'   * `environment_name`: A text string identifying the environment.
        #'   * `is_meter`: Whether report data is a meter data. Possible values: `0` and
        #'     `1`
        #'   * `type`: Nature of data type with respect to state. Possible values: `Sum`
        #'     and `Avg`
        #'   * `index_group`: The report group, e.g. `Zone`, `System`
        #'   * `timestep_type`: Type of data timestep. Possible values: `Zone` and `HVAC
        #'     System`
        #'   * `reporting_frequency`: The reporting frequency of the variable, e.g.
        #'   `HVAC System Timestep`, `Zone Timestep`.
        #'   * `schedule_name`: Name of the the schedule that controls reporting
        #'     frequency.
        #'
        #' With the `datetime` column, it is quite straightforward to apply time-series
        #' analysis on the simulation output. However, another painful thing is that
        #' every simulation run period has its own `Day of Week for Start Day`. Randomly
        #' setting the `year` may result in a date time series that does not have
        #' the same start day of week as specified in the RunPeriod objects.
        #'
        #' eplusr provides a simple solution for this. By setting `year` to `NULL`,
        #' which is the default behavior, eplusr will calculate a year value (from
        #' year 2017 backwards) for each run period that compliances with the start
        #' day of week restriction.
        #'
        #' It is worth noting that EnergyPlus uses 24-hour clock system where 24 is only
        #' used to denote midnight at the end of a calendar day. In EnergyPlus output,
        #' "00:24:00" with a time interval being 15 mins represents a time period from
        #' "00:23:45" to "00:24:00", and similarly "00:15:00" represents a time period
        #' from "00:24:00" to "00:15:00" of the next day. This means that if current day
        #' is Friday, day of week rule applied in schedule time period "00:23:45" to
        #' "00:24:00" (presented as "00:24:00" in the output) is also Friday, but not
        #' Saturday. However, if you try to get the day of week of time "00:24:00" in R,
        #' you will get Saturday, but not Friday. This introduces inconsistency and may
        #' cause problems when doing data analysis considering day of week value.
        #'
        #' With `wide` equals `TRUE`, `$report_data()` will format the simulation output
        #' in the same way as standard EnergyPlus csv output file. Sometimes this can be
        #' useful as there may be existing tools/workflows that depend on this format.
        #' When both `wide` and `all` are `TRUE`, columns of runperiod environment names
        #' and date time components are also returned, including:
        #' `environment_period_index", "environment_name`, `simulation_days`,
        #' `datetime`, `month`, `day`, `hour`, `minute`, `day_type`.
        #'
        #' For convenience, input character arguments matching in
        #' `$report_data()` are **case-insensitive**.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @param key_value A character vector to identify key values of the
        #'        data. If `NULL`, all keys of that variable will be returned.
        #'        `key_value` can also be data.frame that contains `key_value`
        #'        and `name` columns. In this case, `name` argument in
        #'        `$report_data()` is ignored. All available `key_value` for
        #'        current simulation output can be obtained using
        #'        \href{../../eplusr/html/EplusGroupJob.html#method-report_data_dict}{\code{$report_data_dict()}}.
        #'        Default: `NULL`.
        #'
        #' @param name A character vector to identify names of the data. If
        #'        `NULL`, all names of that variable will be returned. If
        #'        `key_value` is a data.frame, `name` is ignored. All available
        #'        `name` for current simulation output can be obtained using
        #'        \href{../../eplusr/html/EplusGroupJob.html#method-report_data_dict}{\code{$report_data_dict()}}.
        #'        Default: `NULL`.
        #'
        #' @param year Year of the date time in column `datetime`. If `NULL`, it
        #'        will calculate a year value that meets the start day of week
        #'        restriction for each environment. Default: `NULL`.
        #'
        #' @param tz Time zone of date time in column `datetime`. Default:
        #'        `"UTC"`.
        #'
        #' @param case If not `NULL`, a character column will be added indicates
        #'        the case of this simulation. If `"auto"`, the name of the IDF
        #'        file without extension is used.
        #'
        #' @param all If `TRUE`, extra columns are also included in the returned
        #'        [data.table::data.table()].
        #'
        #' @param wide If `TRUE`, the output is formatted in the same way as
        #'        standard EnergyPlus csv output file.
        #'
        #' @param period A Date or POSIXt vector used to specify which time
        #'        period to return. The year value does not matter and only
        #'        month, day, hour and minute value will be used when
        #'        subsetting. If `NULL`, all time period of data is returned.
        #'        Default: `NULL`.
        #'
        #' @param month,day,hour,minute Each is an integer vector for month,
        #'        day, hour, minute subsetting of `datetime` column when
        #'        querying on the SQL database. If `NULL`, no subsetting is
        #'        performed on those components. All possible `month`, `day`,
        #'        `hour` and `minute` can be obtained using
        #'        \href{../../eplusr/html/EplusGroupJob.html#method-read_table}{\code{$read_table(NULL, "Time")}}.
        #'        Default: `NULL`.
        #'
        #' @param interval An integer vector used to specify which interval
        #'        length of report to extract. If `NULL`, all interval will be
        #'        used. Default: `NULL`.
        #'
        #' @param simulation_days An integer vector to specify which simulation
        #'        day data to extract. Note that this number resets after warmup
        #'        and at the beginning of an environment period. All possible
        #'        `simulation_days` can be obtained using
        #'        \href{../../eplusr/html/EplusGroupJob.html#method-read_table}{\code{$read_table(NULL, "Time")}}.
        #'        If `NULL`, all simulation days will be used. Default: `NULL`.
        #'
        #' @param day_type A character vector to specify which day type of data
        #'        to extract. All possible day types are: `Sunday`, `Monday`,
        #'        `Tuesday`, `Wednesday`, `Thursday`, `Friday`, `Saturday`,
        #'        `Holiday`, `SummerDesignDay`, `WinterDesignDay`, `CustomDay1`,
        #'        and `CustomDay2`. All possible values for current simulation
        #'        output can be obtained using
        #'        \href{../../eplusr/html/EplusGroupJob.html#method-read_table}{\code{$read_table(NULL, "Time")}}.
        #'        A few grouped options are also provided:
        #'
        #' - `"Weekday"`: All working days, i.e. from Monday to Friday
        #' - `"Weekend"`: Saturday and Sunday
        #' - `"DesignDay"`: Equivalent to `"SummerDesignDay"` plus `"WinterDesignDay"`
        #' - `"CustomDay"`: CustomDay1 and CustomDay2
        #' - `"SpecialDay"`: Equivalent to `"DesignDay"` plus `"CustomDay"`
        #' - `"NormalDay"`: Equivalent to `"Weekday"` and `"Weekend"` plus `"Holiday"`
        #'
        #' @param environment_name A character vector to specify which
        #'        environment data to extract. If `NULL`, all environment data
        #'        are returned. Default: `NULL`. All possible
        #'        `environment_name` for current simulation output can be
        #'        obtained using:
        #' ```
        #' $read_table(NULL, "EnvironmentPeriods")
        #' ```
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' # read report data
        #' group$report_data(c(1, 4))
        #'
        #' # specify output variables using report data dictionary
        #' dict <- group$report_data_dict(1)
        #' group$report_data(c(1, 4), dict[units == "C"])
        #'
        #' # specify output variables using 'key_value' and 'name'
        #' group$report_data(c(1, 4), "environment", "site outdoor air drybulb temperature")
        #'
        #' # explicitly specify year value and time zone
        #' group$report_data(c(1, 4), dict[1], year = 2020, tz = "Etc/GMT+8")
        #'
        #' # get all possible columns
        #' group$report_data(c(1, 4), dict[1], all = TRUE)
        #'
        #' # return in a format that is similar as EnergyPlus CSV output
        #' group$report_data(c(1, 4), dict[1], wide = TRUE)
        #'
        #' # return in a format that is similar as EnergyPlus CSV output with
        #' # extra columns
        #' group$report_data(c(1, 4), dict[1], wide = TRUE, all = TRUE)
        #'
        #' # only get data at the working hour on the first Monday
        #' group$report_data(c(1, 4), dict[1], hour = 8:18, day_type = "monday", simulation_days = 1:7)
        #' }
        #'
        report_data = function (which = NULL, key_value = NULL, name = NULL,
                                year = NULL, tz = "UTC", all = FALSE, wide = FALSE,
                                period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                interval = NULL, simulation_days = NULL, day_type = NULL,
                                environment_name = NULL)
            epgroup_report_data(self, private, which,
                key_value = key_value, name = name, year = year, tz = tz, all = all, wide = wide,
                period = period, month = month, day = day, hour = hour, minute = minute,
                interval = interval, simulation_days = simulation_days, day_type = day_type,
                environment_name = environment_name
            ),
        # }}}

        # tabular_data {{{
        #' @description
        #' Read tabular data
        #'
        #' @details
        #' `$tabular_data()` extracts the tabular data in a
        #' [data.table::data.table()] using report, table, column and row name
        #' specifications. The returned [data.table::data.table()] has
        #' 9 columns:
        #'
        #' * `index`: The index of simulated model. This column can be used
        #'   to distinguish output from different simulations
        #' * `case`: The model name. This column can be used to distinguish
        #'   output from different simulations
        #' * `index`: Tabular data index
        #' * `report_name`: The name of the report that the record belongs to
        #' * `report_for`: The `For` text that is associated with the record
        #' * `table_name`: The name of the table that the record belongs to
        #' * `column_name`: The name of the column that the record belongs to
        #' * `row_name`: The name of the row that the record belongs to
        #' * `units`: The units of the record
        #' * `value`: The value of the record **in string format** by default
        #'
        #' For convenience, input character arguments matching in
        #' `$tabular_data()` are **case-insensitive**.
        #'
        #' @param which An integer vector of the indexes or a character vector
        #'        or names of parametric simulations. If `NULL`, results of all
        #'        parametric simulations are returned. Default: `NULL`.
        #'
        #' @param report_name,report_for,table_name,column_name,row_name Each is
        #'        a character vector for subsetting when querying the SQL
        #'        database.  For the meaning of each argument, please see the
        #'        description above.
        #'
        #' @param wide If `TRUE`, each table will be converted into the similar
        #'        format as it is shown in EnergyPlus HTML output file. Default:
        #'        `FALSE`.
        #'
        #' @param string_value Only applicable when `wide` is `TRUE`. If
        #'        `string_value` is `FALSE`, instead of keeping all values as
        #'        characters, values in possible numeric columns are converted
        #'        into numbers. Default: the opposite of `wide`. Possible
        #'        numeric columns indicate column that:
        #' * columns that have associated units
        #' * columns that contents numbers
        #'
        #' @return A [data.table::data.table()] with 9 columns (when `wide` is
        #' `FALSE`) or a named list of [data.table::data.table()]s where the
        #' names are the combination of `report_name`, `report_for` and
        #' `table_name`.
        #'
        #' @examples
        #' \dontrun{
        #' # read all tabular data
        #' group$tabular_data(c(1, 4))
        #'
        #' # explicitly specify data you want
        #' str(group$tabular_data(c(1, 4),
        #'     report_name = "AnnualBuildingUtilityPerformanceSummary",
        #'     table_name = "Site and Source Energy",
        #'     column_name = "Total Energy",
        #'     row_name = "Total Site Energy"
        #' ))
        #'
        #' # get tabular data in wide format and coerce numeric values
        #' str(group$tabular_data(c(1, 4),
        #'     report_name = "AnnualBuildingUtilityPerformanceSummary",
        #'     table_name = "Site and Source Energy",
        #'     column_name = "Total Energy",
        #'     row_name = "Total Site Energy",
        #'     wide = TRUE, string_value = FALSE
        #' ))
        #' }
        #'
        tabular_data = function(which = NULL, report_name = NULL, report_for = NULL,
                                table_name = NULL, column_name = NULL, row_name = NULL,
                                wide = FALSE, string_value = !wide)
            epgroup_tabular_data(self, private, which, report_name = report_name,
                report_for = report_for, table_name = table_name,
                column_name = column_name, row_name = row_name,
                wide = wide, string_value = string_value),
        # }}}

        # print {{{
        #' @description
        #' Print `EplusGroupJob` object
        #'
        #' @details
        #' `$print()` shows the core information of this `EplusGroupJob`, including the
        #' path of IDFs and EPWs and also the simulation job status.
        #'
        #' `$print()` is quite useful to get the simulation status, especially when
        #' `wait` is `FALSE` in `$run()`. The job status will be updated and printed
        #' whenever `$print()` is called.
        #'
        #' @return The `EplusGroupJob` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' group$print()
        #' }
        #'
        print = function ()
            epgroup_print(self, private)
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_idfs = NULL,
        m_epws_path = NULL,
        m_job = NULL,
        m_log = NULL,
        # }}}
        # PRIVATE FUNCTIONS {{{
        uuid = function () private$m_log$uuid,
        log_new_uuid = function () log_new_uuid(private$m_log),

        idf_uuid = function (which = NULL) {
            idfs <- if (is.null(which)) private$m_idfs else private$m_idfs[which]
            vcapply(idfs, function (idf) get_priv_env(idf)$uuid())
        },
        log_idf_uuid = function (which = NULL) {
            if (is.null(which)) which <- seq_along(private$m_idfs)
            private$m_log$idf_uuid[which] <- private$idf_uuid(which)
        },
        cached_idf_uuid = function (which = NULL) {
            if (is.null(which)) which <- seq_along(private$m_log$idf_uuid)
            private$m_log$idf_uuid[which]
        },

        is_unsaved = function () private$m_log$unsaved,
        log_saved = function (which = NULL) log_saved(private$m_log, which),
        log_unsaved = function (which = NULL) log_unsaved(private$m_log, which)
        # }}}
    )
)
# }}}

#' Create An EnergyPlus Parametric Simulation Job
#'
#' `group_job()` takes IDFs and EPWs as input and returns a `EplusGroupJob`.
#'
#' @param idfs Paths to EnergyPlus IDF files or a list of IDF files and [Idf]
#'        objects.
#' @param epws Paths to EnergyPlus EPW files or a list of EPW files and [Epw]
#'        objects. Each element in the list can be `NULL`, which will force
#'        design-day-only simulation when [`$run()`][EplusGroupJob] method is
#'        called. Note this needs at least one `Sizing:DesignDay` object exists
#'        in that [Idf]. If `epws` is `NULL`, design-day-only simulation will be
#'        conducted for all input models.
#' @return A `EplusGroupJob` object.
#' @seealso [eplus_job()] for creating an EnergyPlus single simulation job.
#' @export
#' @name EplusGroupJob
# group_job {{{
group_job <- function (idfs, epws) {
    EplusGroupJob$new(idfs, epws)
}
# }}}

# epgroup_run {{{
epgroup_run <- function (self, private, output_dir = NULL, wait = TRUE,
                         force = FALSE, copy_external = FALSE, echo = wait,
                         separate = TRUE, readvars = TRUE) {
    # check if generated models have been modified outside
    uuid <- private$idf_uuid()
    if (any(i <- uuid != private$cached_idf_uuid())) {
        warn(paste0(
            "Some of the grouped models have been modified. ",
            "Running these models will result in simulation outputs that may be not reproducible. ",
            paste0(" # ", seq_along(uuid)[i], " | ", names(uuid)[i], collapse = "\n")
        ), "group_model_modified")
        private$log_unsaved(which(i))
    }

    private$log_new_uuid()

    epgroup_run_models(self, private, output_dir, wait, force, copy_external, echo, separate, readvars)
}
# }}}
# epgroup_run_models {{{
#' @importFrom checkmate test_names
epgroup_run_models <- function (self, private, output_dir = NULL, wait = TRUE,
                                force = FALSE, copy_external = FALSE, echo = wait,
                                separate = TRUE, readvars = TRUE) {
    assert_flag(wait)
    assert_flag(echo)
    assert_flag(separate)
    assert_flag(readvars)

    path_idf <- vcapply(private$m_idfs, function (idf) idf$path())

    if (checkmate::test_names(names(private$m_idfs))) {
        # for parametric job
        nms <- paste0(make_filename(names(private$m_idfs)), ".idf")
    } else {
        nms <- basename(path_idf)
    }

    if (is.null(private$m_epws_path)) {
        path_epw <- NULL
        design_day <- TRUE
    } else {
        path_epw <- private$m_epws_path
        if (length(path_epw) == 1L) {
            path_epw <- rep(path_epw, length(path_idf))
        }
        design_day <- is.na(path_epw)
    }

    if (is.null(output_dir))
        output_dir <- dirname(path_idf)
    else if (length(output_dir) == 1L) {
        output_dir <- rep(output_dir, length(path_idf))
    } else {
        assert_same_len(path_idf, output_dir)
    }
    output_dir <- normalizePath(output_dir, mustWork = FALSE)

    if (any(!dir.exists(uniq_dir <- unique(output_dir)))) {
        dir_to_create <- uniq_dir[!dir.exists(uniq_dir)]
        create_dir <- vlapply(dir_to_create, dir.create, showWarnings = FALSE, recursive = TRUE)
        # nocov start
        if (any(!create_dir)) {
            abort(paste0("Failed to create output directory: ", collapse(dir_to_create)[!create_dir]))
        }
        # nocov end
    }

    # check if the model is still running
    proc <- private$m_job
    if (!is.null(proc)) {
        # check if running in non-waiting mode
        if (inherits(proc, "process") && proc$is_alive()) {
            pid <- proc$get_pid()
            if (force) {
                verbose_info("Force to kill all current running parametric simulations (",
                    "Parent R Process PID: ", pid, ") and restart...")
                suppressMessages(self$kill())
            } else {
                abort(paste0("Current parametric simulations are still running (Parent R Process PID: ",
                    pid, "). Please set `force` to TRUE if you want ",
                    "to kill the running process and restart."))
            }
        }
    }

    if (separate) {
        path_group <- normalizePath(file.path(output_dir, tools::file_path_sans_ext(nms), nms), mustWork = FALSE)
    } else {
        path_group <- normalizePath(file.path(output_dir, nms), mustWork = FALSE)
    }

    if (any(to_save <- path_group != path_idf | private$is_unsaved())) {
        # remove duplications
        dup <- duplicated(path_group)
        apply2(private$m_idfs[to_save & !dup], path_group[to_save & !dup],
            function (x, y) x$save(y, overwrite = TRUE, copy_external = copy_external)
        )
        private$log_idf_uuid(which(to_save))
        private$log_saved(which(to_save))
    }

    # reset status
    private$m_log$start_time <- current()
    private$m_log$killed <- NULL
    private$m_job <- NULL

    ver <- vcapply(private$m_idfs, function (idf) as.character(idf$version()))

    # init job table
    jobs <- pre_job_inputs(path_group, path_epw, NULL, design_day, FALSE, ver)
    if (!copy_external) {
        set(jobs, NULL, "resources", list())
    } else {
        # check if external file dependencies are found
        resrc <- lapply(private$m_idfs, function(idf) {
            deps <- idf$external_deps()
            if (!length(deps)) deps <- NULL
            deps
        })
        set(jobs, NULL, "resources", resrc)
    }

    options <- list(num_parallel = eplusr_option("num_parallel"), echo = echo,
        expand_obj = TRUE, readvars = readvars)
    state <- list(jobs = jobs, options = options)

    if (wait) {
        private$m_job <- run_sim_event_loop(state)
    } else {
        # always echo in order to catch standard output and error
        state$options$echo <- TRUE
        private$m_job <- callr::r_bg(
            function(state) run_sim_event_loop(state),
            args = list(state = state), package = TRUE
        )
    }

    private$log_new_uuid()
    if (wait) private$m_log$end_time <- current()

    self
}
# }}}
# epgroup_kill {{{
epgroup_kill <- function (self, private) {
    if (is.null(private$m_job)) {
        verbose_info("The parametric job is not running.")
        return(invisible(FALSE))
    }

    if (!inherits(private$m_job, "process")) {
        verbose_info("The parametric job is not running.")
        return(invisible(FALSE))
    }

    proc <- private$m_job

    if (!proc$is_alive()) {
        verbose_info("The parametric job is not running.")
        return(invisible(FALSE))
    }

    k <- tryCatch(proc$kill(), error = function (e) FALSE)

    if (isTRUE(k)) {
        verbose_info("The parametric job has been successfully killed.")
        private$m_log$killed <- TRUE
        return(invisible(TRUE))
    } else {
        verbose_info("Failed to kill parametric job, because it was already finished/dead.")
        return(invisible(FALSE))
    }
}
# }}}
# epgroup_status {{{
epgroup_status <- function (self, private) {
    status <- list(
        run_before = FALSE, # if the model has been run before
        alive = FALSE, # if simulation is still running
        terminated = NA, # if last simulation was terminated
        successful = NA, # if last simulation was successful
        changed_after = NA, # if the seed model has been changed after last simulation
        job_status = data.table() # if no simulation has been run
    )

    proc <- private$m_job

    if (is.null(private$m_job)) {
        if (!is.null(private$m_idfs)) {
            status$job_status <- data.table(
                index = seq_along(private$m_idfs),
                status = "idle",
                idf = vcapply(private$m_idfs, function (idf) idf$path())
            )
            if (is.null(private$m_epws_path)) {
                epw <- NA_character_
            } else {
                epw <- vcapply(private$m_epws_path, function (epw) if (is.null(epw)) NA_character_ else epw)
            }
            set(status$job_status, NULL, "epw", epw)
        }

        return(status)
    }

    status$run_before <- TRUE

    if (isTRUE(private$m_log$killed)) {
        status$terminated <- TRUE
    } else {
        status$terminated <- FALSE
    }

    status$changed_after <- FALSE
    uuid <- private$idf_uuid()
    if (any(private$cached_idf_uuid() != uuid)) {
        status$changed_after <- TRUE
    }

    # for parametric job
    if (is_idf(private$m_seed) && !identical(private$seed_uuid(), get_priv_env(private$m_seed)$uuid())) {
        status$changed_after <- TRUE
    }

    if (inherits(proc, "r_process")) {
        if (proc$is_alive()) {
            status$alive <- TRUE
        } else {
            status$alive <- FALSE

            proc$wait()
            exit_status <- proc$get_exit_status()

            # retrieve results
            res <- tryCatch(proc$get_result(), error = function (e) data.table())

            if (!is.null(res)) {
                # update job
                private$m_job <- res

                # process the raw table
                status$job_status <- post_process_sim_state(res)
            }

            # only if all simulation ran successfully
            if (!is.na(exit_status) && exit_status == 0L &&
                nrow(status$job_status) && all(status$job_status$exit_status == 0L)) {
                status$successful <- TRUE
            } else {
                status$successful <- FALSE
            }
        }

    } else {
        status$alive <- FALSE
        status$successful <- all(proc$jobs$exit_status == 0L)
        status$job_status <- post_process_sim_state(proc)
    }

    status
}
# }}}
# epgroup_output_dir {{{
epgroup_output_dir <- function (self, private, which = NULL) {
    epgroup_job_from_which(self, private, which, keep_unsucess = TRUE)$output_dir
}
# }}}
# epgroup_list_files {{{
epgroup_list_files <- function (self, private, which = NULL, simplify = FALSE, full = FALSE) {
    assert_flag(simplify)
    assert_flag(full)

    jobs <- epgroup_job_from_which(self, private, which, keep_unsucess = TRUE)

    jobs <- jobs[, .SD, .SDcols = c("index", "output_dir", "result")]
    set(jobs, NULL, "file", lapply(jobs$result, "[[", "file"))
    set(jobs, NULL, "result", NULL)

    if (simplify) {
        files <- lapply(jobs$file, function(f) {
            files <- unlist(f, FALSE, FALSE)
            files[!is.na(files)]
        })

        if (full) {
            files <- apply2(jobs$output_dir, files, use.names = FALSE,
                function(dir, file) normalizePath(file.path(dir, file), mustWork = FALSE)
            )
        }
    } else {
        if (!full) {
            files <- jobs[, by = "index", {
                list(type = names(file[[1L]]), file = file[[1L]])
            }]
        } else {
            files <- jobs[, by = "index", {
                file <- file[[1L]]
                type <- names(file)
                file <- lapply(file, function(f) {
                    if (all(is.na(f))) {
                        f
                    } else {
                        normalizePath(file.path(output_dir, f), mustWork = FALSE)
                    }
                })

                list(type = type, file = file)
            }]
        }
    }

    files
}
# }}}
# epgroup_locate_output {{{
epgroup_locate_output <- function (self, private, which = NULL, suffix = ".err", strict = TRUE, keep_unsucess = FALSE) {
    job <- epgroup_job_from_which(self, private, which, keep_unsucess = keep_unsucess)

    out <- paste0(tools::file_path_sans_ext(job$model), suffix)

    if (strict && any(!file.exists(out))) {
        msg <- job[!file.exists(out), get_sim_status_string("MISSING", index, model, weather)]
        stop("Path does not exist for job:\n", paste0(msg, collapse = "\n"), call. = FALSE)
    }

    out
}
# }}}
# epgroup_output_errors {{{
epgroup_output_errors <- function (self, private, which, info = FALSE) {
    # continue to parse err file for jobs having non-zero exits (#24)
    path_err <- epgroup_locate_output(self, private, which, ".err", keep_unsucess = TRUE)

    err <- lapply(path_err, parse_err_file)

    names(err) <- epgroup_case_from_which(self, private, which, name = TRUE)

    if (!info) err <- lapply(err, function (x) x[!(level == "Info")])

    err
}
# }}}
# epgroup_list_table {{{
epgroup_list_table <- function (self, private, which = NULL) {
    cases <- epgroup_case_from_which(self, private, which, name = TRUE)
    lists <- lapply(epgroup_sql_path(self, private, which), list_sql_table)
    setattr(lists, "names", cases)[]
}
# }}}
# epgroup_read_table {{{
epgroup_read_table <- function (self, private, which = NULL, table) {
    tables <- lapply(epgroup_sql_path(self, private, which), read_sql_table, table)
    epgroup_combine_data(self, private, which, tables)[]
}
# }}}
# epgroup_read_rdd {{{
epgroup_read_rdd <- function (self, private, which = NULL) {
    rdds <- lapply(epgroup_rdd_path(self, private, which, "rdd"), read_rdd)
    epgroup_combine_data(self, private, which, rdds)[]
}
# }}}
# epgroup_read_mdd {{{
epgroup_read_mdd <- function (self, private, which = NULL) {
    mdds <- lapply(epgroup_rdd_path(self, private, which, "mdd"), read_mdd)
    epgroup_combine_data(self, private, which, mdds)[]
}
# }}}
# epgroup_report_data_dict {{{
epgroup_report_data_dict <- function (self, private, which) {
    dicts <- lapply(epgroup_sql_path(self, private, which), get_sql_report_data_dict)
    epgroup_combine_data(self, private, which, dicts)[]
}
# }}}
# epgroup_report_data {{{
epgroup_report_data <- function (self, private, which = NULL, key_value = NULL,
                                 name = NULL, year = NULL, tz = "GMT", all = FALSE, wide = FALSE,
                                 period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                 interval = NULL, simulation_days = NULL, day_type = NULL,
                                 environment_name = NULL) {
    rbindlist(Map(get_sql_report_data,
        sql = epgroup_sql_path(self, private, which),
        index = epgroup_case_from_which(self, private, which, name = FALSE),
        case = epgroup_case_from_which(self, private, which, name = TRUE),
        MoreArgs = list(
            key_value = key_value, name = name, all = all, wide = wide, year = year,
            tz = tz, period = period, month = month, day = day, hour = hour, minute = minute,
            interval = interval, simulation_days = simulation_days, day_type = day_type,
            environment_name = environment_name
        ),
        USE.NAMES = FALSE
    ), fill = TRUE)
}
# }}}
# epgroup_tabular_data {{{
epgroup_tabular_data <- function (self, private, which = NULL, report_name = NULL, report_for = NULL,
                                  table_name = NULL, column_name = NULL, row_name = NULL,
                                  wide = FALSE, string_value = !wide) {
    l <- Map(get_sql_tabular_data,
        sql = epgroup_sql_path(self, private, which),
        index = epgroup_case_from_which(self, private, which, name = FALSE),
        case = epgroup_case_from_which(self, private, which, name = TRUE),
        MoreArgs = list(
            report_name = report_name, report_for = report_for,
            table_name = table_name, column_name = column_name, row_name = row_name,
            wide = wide, string_value = string_value
        )
    )

    if (!wide) return(rbindlist(l, fill = TRUE))

    nm_all <- unique(unlist(lapply(l, names)))
    names(nm_all) <- nm_all

    lapply(nm_all, function (nm) {
        rbindlist(lapply(l, function (lst) lst[[nm]]), fill = TRUE)
    })
}
# }}}
# epgroup_print {{{
epgroup_print <- function (self, private) {
    cli::cat_rule("EnergPlus Group Simulation Job", col = "green")
    cli::cat_line(paste0("Grouped Jobs [", length(private$m_idfs), "]: "))

    epgroup_print_status(self, private)
}
# }}}

# helper
# get_epgroup_input {{{
get_epgroup_input <- function (idfs, epws, sql = TRUE, dict = TRUE) {
    # check idf {{{
    if (is_idf(idfs)) {
        idfs <- list(get_init_idf(idfs, sql = sql, dict = dict))
    } else {
        init_idf <- function (...) {
            tryCatch(get_init_idf(...),
                eplusr_error_idf_not_local = function (e) e,
                eplusr_error_idf_path_not_exist = function (e) e,
                eplusr_error_idf_not_saved = function (e) e
            )
        }
        idfs <- lapply(idfs, init_idf, sql = sql, dict = dict)
    }

    err <- c("eplusr_error_idf_not_local", "eplusr_error_idf_path_not_exist", "eplusr_error_idf_not_saved")
    if (any(invld <- vlapply(idfs, inherits, err))) {
        abort(paste0("Invalid IDF input found:\n",
            paste0(lpad(paste0("  #", which(invld))), ": ", vcapply(idfs[invld], conditionMessage),
                collapse = "\n"
            )
        ))
    }

    sql <- vlapply(idfs, attr, "sql")
    dict <- vlapply(idfs, attr, "sql")
    # }}}

    # check epw paths {{{
    get_epw <- function (epw) if (is.null(epw)) NA_character_ else get_init_epw(epw)

    epws <- lapply(epws, function (x) {
        tryCatch(get_epw(x),
            eplusr_error_epw_not_local = function (e) e,
            eplusr_error_epw_path_not_exist = function (e) e,
            eplusr_error_epw_not_saved = function (e) e
        )
    })

    err <- c("eplusr_error_epw_not_local", "eplusr_error_epw_path_not_exist", "eplusr_error_epw_not_saved")
    if (any(invld <- vlapply(epws, inherits, err))) {
        abort(paste0("Invalid EPW input found:\n",
            paste0(lpad(paste0("  #", which(invld))), ": ", vcapply(epws[invld], conditionMessage),
                collapse = "\n"
            )
        ))
    }

    if (!length(epws)) epws <- NULL
    # }}}

    # check length
    if (!is.null(epws)) {
        epws <- vcapply(epws, `%||%`, NA_character_)
        if (length(epws) == 1L) epws <- replicate(length(idfs), epws)
        if (length(idfs) == 1L) {
            idfs <- replicate(length(epws), idfs[[1L]]$clone())
            sql <- rep(sql, length(epws))
            dict <- rep(dict, length(epws))
        }
        assert_same_len(idfs, epws)
    }

    list(idfs = idfs, epws = epws, sql = sql, dict = dict)
}
# }}}
# epgroup_retrieve_data {{{
epgroup_retrieve_data <- function (self, private, status) {
    if (!status$run_before) return(invisible())

    if (status$alive) {
        private$m_log$stdout <- c(private$m_log$stdout, private$m_job$read_output_lines(10000))
        private$m_log$stderr <- c(private$m_log$stderr, private$m_job$read_error_lines(10000))
    } else {
        if (inherits(private$m_job, "r_process") & !status$terminated) {
            private$m_log$stdout <- c(private$m_log$stdout, private$m_job$read_all_output_lines())
            private$m_log$stderr <- c(private$m_log$stderr, private$m_job$read_all_error_lines())
        }
        if (status$successful) {
            if (inherits(private$m_job, "r_process")) {
                private$m_job <- tryCatch(private$m_job$get_result(),
                    error = function (e) {
                        stop("Failed to retrieve output of parametric job. ", e, "\n",
                            private$m_log$stderr, call. = FALSE)
                    }
                )
            }
            if (is.null(private$m_log$end_time)) {
                end_times <- private$m_job[!is.na(end_time), end_time]
                if (length(end_times)) private$m_log$end_time <- max(end_times)
            }
        }
    }
}
# }}}
# epgroup_job_from_which {{{
epgroup_job_from_which <- function (self, private, which, keep_unsucess = FALSE) {
    status <- epgroup_status(self, private)

    if (!isTRUE(status$run_before))
        stop("Parametric job did not run before. Please run it using `$run()` ",
            "before collect output", call. = FALSE)

    if (isTRUE(status$terminated))
        stop("Parametric job was terminated before. Please solve ",
            "the problems and re-run it before collect output.", call. = FALSE)

    if (isTRUE(status$alive))
        stop("Parametric job is still running. Please wait it ",
            "to finish before collecting results.", call. = FALSE)

    if (isTRUE(status$changed_after))
        warning("The seed model has been changed since last run. ",
            "The job output may not be correct.", call. = FALSE)

    # if success, retrieve data
    epgroup_retrieve_data(self, private, status)

    jobs <- private$m_job$jobs

    idx <- epgroup_case_from_which(self, private, which, name = FALSE)

    job <- jobs[idx]

    # setting `keep_unsucess` to TRUE makes it possible to continue to parse
    # some output files such like .err files. (#24)
    if (nrow(job[status != "completed"])) {
        incomplete <- job[status != "completed"]
        msg <- incomplete[, get_sim_status_string(rpad(toupper(status)), index, model, weather)]
        if (keep_unsucess) {
            warn(paste0("Some of jobs failed to complete. ",
                "Simulation results may not be correct:\n",
                paste0(msg, collapse = "\n")
            ), "job_error")
        } else {
            abort(paste0("Some of jobs failed to complete. ",
                "Please fix the problems and re-run it before collecting output:\n",
                paste0(msg, collapse = "\n")
            ), "job_error")
        }
    }

    job
}
# }}}
# epgroup_case_from_which {{{
#' @importFrom checkmate test_names
epgroup_case_from_which <- function (self, private, which = NULL, name = FALSE) {
    if (checkmate::test_named(private$m_idfs)) {
        nms <- names(private$m_idfs)
    } else {
        nms <- vcapply(private$m_idfs, function(idf) tools::file_path_sans_ext(basename(idf$path())))
    }

    if (is.null(which)) {
        if (name) return(nms) else return(seq_along(nms))
    }

    if (is.character(which)) {
        valid <- chmatch(stri_trans_tolower(which), stri_trans_tolower(nms))
        if (anyNA(valid))
            stop("Invalid job name found: ",
                collapse(which[is.na(valid)]), ".", call. = FALSE)

        idx <- valid
    } else if (checkmate::test_integerish(which, lower = 1L, any.missing = FALSE)) {
        valid <- which <= length(nms)
        if (any(!valid))
            stop("Invalid job index found for current parametric job: ",
                collapse(which[!valid]), ".", call. = FALSE)
        idx <- which
    } else {
        stop("'which' should either be a character or an integer vector.",
            call. = FALSE)
    }

    if (name) nms[idx] else idx
}
# }}}
# epgroup_sql_path {{{
epgroup_sql_path <- function (self, private, which) {
    epgroup_locate_output(self, private, which, ".sql")
}
# }}}
# epgroup_rdd_path {{{
epgroup_rdd_path <- function (self, private, which, type = c("rdd", "mdd")) {
    type <- match.arg(type)
    epgroup_locate_output(self, private, which, paste0(".", type))
}
# }}}
# epgroup_combine_data {{{
epgroup_combine_data <- function (self, private, which, data, fill = TRUE) {
    index <- epgroup_case_from_which(self, private, which, name = FALSE)
    cases <- epgroup_case_from_which(self, private, which, name = TRUE)

    # add case
    for (idx in seq_along(cases)) {
        set(data[[idx]], NULL, "index", index[idx])
        set(data[[idx]], NULL, "case", cases[idx])
        setcolorder(data[[idx]], c("index", "case"))
    }

    rbindlist(data, fill = fill)
}
# }}}
# epgroup_print_status {{{
epgroup_print_status <- function (self, private, epw = TRUE) {
    status <- epgroup_status(self, private)
    epgroup_retrieve_data(self, private, status)

    if (!is.null(names(private$m_idfs))) {
        nm_idf <- paste0(names(private$m_idfs), ".idf")
    } else {
        nm_idf <- vcapply(private$m_idfs, function (x) basename(x$path()))
    }
    if (!epw) {
        nm <- str_trunc(paste0(
            "[", lpad(seq_along(private$m_idfs), 0), "]: ", surround(nm_idf)
        ))
    } else {
        nm_idf <- str_trunc(paste0(
            "[", lpad(seq_along(private$m_idfs), 0), "]: ",
            paste0("[IDF] ", surround(nm_idf))
        ))

        if (is.null(private$m_epws_path)) {
            nm_epw <- "[EPW] << Not specified >>"
        } else {
            nm_epw <- basename(private$m_epws_path)
            nm_epw[!is.na(nm_epw)] <- surround(nm_epw[!is.na(nm_epw)])
            nm_epw[is.na(nm_epw)] <- "<< Not specified >>"
            nm_epw <- paste0("[EPW] ", nm_epw)
        }

        nm <- paste0(rpad(nm_idf), " + ", nm_epw)
    }

    if (!status$run_before) {
        cli::cat_line(paste0(str_trunc(nm), collapse = "\n"))
        cli::cat_line("<< Job has not been run before >>",
            col = "white", background_col = "blue")
        return(invisible())
    }

    # each job status {{{
    if (status$alive) {
        if (length(private$m_log$stderr)) {
            stderr <- private$m_log$stderr
            # keep the latest status
            job_status <- as.data.table(stri_split_fixed(stderr, "|", n = 2L, simplify = TRUE))
            job_status <- unique(job_status, fromLast = TRUE, by = "V1")
            # get index
            set(job_status, NULL, "index", as.integer(job_status$V1))
            # order by index
            setorder(job_status, "index")
            # make sure all models are included
            job_status <- job_status[J(seq_along(nm)), on = "index"]
            # for models that are idle
            job_status[J(NA_character_), on = "V2", V2 := paste0(
                "IDLE       --> [IDF]", surround(names(private$m_idfs)[index]))]
            stderr <- paste0(lpad(job_status$index, "0"), "|" ,job_status$V2)
            safe_width <- getOption("width") - 2L
            stderr_trunc <- vcapply(stderr, function (l) {
                if (nchar(l) > safe_width) {
                    paste0(substr(l, 1, safe_width), "...")
                } else {
                    l
                }
            })

            cli::cat_boxx(stderr_trunc, col = "green", border_col = "green",
                padding = 0)
        }
    } else {
        if (isTRUE(status$terminated)) {
            cli::cat_line(paste0(str_trunc(rpad(nm), width = getOption("width", 60L) - 15L),
                " <-- TERMINATED", collapse = "\n"))
        } else {
            nm <- private$m_job$jobs[, paste0(
                ifelse(exit_status == 0L,
                    paste0(str_trunc(rpad(nm), getOption("width", 60L) - 14L), " <-- SUCCEEDED"),
                    paste0(str_trunc(rpad(nm), getOption("width", 60L) - 11L), " <-- FAILED")
                )
            )]
            cli::cat_line(paste0(nm, collapse = "\n"))
        }
    }
    # }}}

    # print summary status {{{
    if (isTRUE(status$terminated)) {
        cli::cat_line(" Job was terminated before.",
            col = "white", background_col = "red")
    } else if (status$alive) {
        cli::cat_line(" Job started at ",
            surround(private$m_log$start_time), " and is still running...",
            col = "black", background_col = "green"
        )
    } else if (!isTRUE(status$successful)) {
        cli::cat_line(" Job started at ",
            surround(private$m_log$start_time), " and ended unsuccessfully...",
            col = "white", background_col = "red"
        )
    } else {
        if (!is.null(private$m_log$end_time)) {
            run_time <- format(round(difftime(
                private$m_log$end_time, private$m_log$start_time), digits = 2L)
            )
            cli::cat_line(" Simulation started at ",
                surround(private$m_log$start_time), " and completed successfully after ",
                run_time, ".",
                col = "black", background_col = "green"
            )
        } else {
            cli::cat_line(" Simulation started at ",
                surround(private$m_log$start_time), " and completed successfully.",
                col = "black", background_col = "green"
            )
        }
    }
    # }}}
}
# }}}

# S3 EplusGroupJob methods {{{
#' @export
str.EplusGroupJob <- function (object, ...) {
    object$print()
}

#' @export
format.EplusGroupJob <- function (x, ...) {
    paste0(utils::capture.output(x$print()), collapse = "\n")
}

#' @export
`==.EplusGroupJob` <- function (e1, e2) {
    if (!inherits(e2, "EplusGroupJob")) return(FALSE)
    identical(get_priv_env(e1)$uuid(), get_priv_env(e2)$uuid())
}

#' @export
`!=.EplusGroupJob` <- function (e1, e2) {
    Negate(`==.EplusGroupJob`)(e1, e2)
}
# }}}
