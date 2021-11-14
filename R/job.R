#' @importFrom R6 R6Class
#' @importFrom cli cat_bullet cat_line cat_rule
#' @importFrom tools file_path_sans_ext
NULL

#' Run EnergyPlus Simulation and Collect Outputs
#'
#' `EplusJob` class wraps the EnergyPlus command line interface and provides
#' methods to extract simulation outputs.
#'
#' @details
#' eplusr uses the EnergyPlus SQL output for extracting simulation outputs.
#'
#' `EplusJob` has provide some wrappers that do SQL query to get report data
#' results, i.e. results from `Output:Variable` and `Output:Meter*`. But for
#' `Output:Table` results, you have to be familiar with the structure of the
#' EnergyPlus SQL results, especially for table *"TabularDataWithStrings"*. For
#' details, please see *"2.20 eplusout.sql"*, especially *"2.20.4.4 TabularData
#' Table"* in EnergyPlus *"Output Details and Examples"* documentation. An
#' object in `Output:SQLite` with `Option Type` value of `SimpleAndTabular` will
#' be automatically created if it does not exists, to ensure that the output
#' collection functionality works successfully.
#'
#' In order to make sure `.rdd` (Report Data Dictionary) and `.mdd` (Meter Data
#' Dictionary) files are created during simulation, an object in
#' `Output:VariableDictionary` class with `Key Field` value being `IDF` will be
#' automatically created if it does not exists.
#'
#' @docType class
#' @name EplusJob
#' @seealso [ParametricJob] class for EnergyPlus parametric simulations.
#' @author Hongyuan Jia
NULL

#' @export
# EplusJob {{{
EplusJob <- R6::R6Class(classname = "EplusJob", cloneable = FALSE,
    public = list(

        # INITIALIZE {{{
        #' @description
        #' Create an `EplusJob` object
        #'
        #' @param idf Path to an local EnergyPlus IDF file or an [Idf] object.
        #' @param epw Path to an local EnergyPlus EPW file or an [Epw] object.
        #'
        #' @return An `EplusJob` object.
        #'
        #' @examples
        #' \dontrun{
        #' if (is_avail_eplus(8.8)) {
        #'     name_idf <- "1ZoneUncontrolled.idf"
        #'     name_epw <-  "USA_CA_San.Francisco.Intl.AP.724940_TMY3.epw"
        #'
        #'     path_idf <- path_eplus_example(8.8, name_idf)
        #'     path_epw <- path_eplus_weather(8.8, name_epw)
        #'
        #'     # create from local files
        #'     job <- eplus_job(path_idf, path_epw)
        #'
        #'     # create from an Idf and an Epw object
        #'     job <- eplus_job(read_idf(path_idf), read_epw(path_epw))
        #' }
        #' }
        initialize = function (idf, epw) {
            job_initialize(self, private, idf, epw)
        },
        # }}}

        # PUBLIC FUNCTIONS {{{
        # version {{{
        #' @description
        #' Get the version of IDF in current job
        #'
        #' @details
        #' `$version()` returns the version of IDF that current `EplusJob` uses.
        #'
        #' @return A [base::numeric_version()] object.
        #'
        #' @examples
        #' \dontrun{
        #' job$version()
        #' }
        #'
        version = function ()
            job_version(self, private),
        # }}}

        # path {{{
        #' @description
        #' Get the paths of file that current `EpwSql` uses
        #'
        #' @details
        #' `$path()` returns the path of IDF or EPW of current job.
        #'
        #' @param type If `"all"`, both the [Idf] path and [Epw] path are
        #'        returned. If `"idf"`, only IDF path is returned. If `"epw"`,
        #'        only EPW path is returned. If `epw` is `NULL`, `NA` is
        #'        returned for EPW path. Default: `"all"`.
        #'
        #' @return A character vector.
        #'
        #' @examples
        #' \dontrun{
        #' job$path()
        #' job$path("idf")
        #' job$path("epw")
        #' }
        #'
        path = function (type = c("all", "idf", "epw"))
            job_path(self, private, type),
        # }}}

        # run {{{
        #' @description
        #' Run simulationA
        #'
        #' @details
        #' `$run()` runs the simulation using input IDF and EPW file. If `wait`
        #' is `FALSE`, the job is run in the background. You can get updated job
        #' status by just
        #' \href{../../eplusr/html/EplusJob.html#method-print}{printing}
        #' the `EplusJob` object.
        #'
        #' Parameter `epw` can be used to reset the EPW file to use for
        #' simulation. If not given, the `epw` input used when creating
        #' this `EplusJob` object will be used.
        #'
        #' @param epw A path to an `.epw` file or an [Epw] object. `epw` can
        #'        also be `NULL` which will force design-day-only simulation.
        #'        Note this needs EnergyPlus v8.3 and later, and at least one
        #'        `Sizing:DesignDay` object exists in the `Idf`. If not given,
        #'        the `epw` input used when creating this `EplusJob` object will
        #'        be used.
        #' @param dir The directory to save the simulation results. If `NULL`,
        #'        the input `idf` folder will be used. Default: `NULL`.
        #' @param wait If `TRUE`, R will hang on and wait for the simulation to
        #'        complete. EnergyPlus standard output (stdout) and error
        #'        (stderr) is printed to R console. If `FALSE`, simulation will
        #'        be run in a background process.  Default: `TRUE`.
        #' @param echo Only applicable when `wait` is `TRUE`. Whether to show
        #'        standard output and error from EnergyPlus. Default: same as
        #'        `wait`.
        #' @param force Only applicable when the last job runs with `wait`
        #'        equals to `FALSE` and is still running. If `TRUE`, current
        #'        running job is forced to stop and a new one will start.
        #'        Default: `FALSE`.
        #' @param copy_external If `TRUE`, the external files that current `Idf`
        #'        object depends on will also be copied into the simulation
        #'        output directory. The values of file paths in the Idf will be
        #'        changed automatically. This ensures that the output directory
        #'        will have all files needed for the model to run. Default is
        #'        `FALSE`.
        #' @param readvars If `TRUE`, the `ReadVarESO` post-processor will run
        #'        to generate CSV files from the ESO output. Since those CSV
        #'        files are never used when extracting simulation data in eplusr,
        #'        setting it to `FALSE` can speed up the simulation if there are
        #'        hundreds of output variables or meters. Default: `TRUE`.
        #'
        #' @return The `EplusJob` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' # only run design day
        #' job$run(NULL)
        #'
        #' # specify output directory
        #' job$run(dir = tempdir())
        #'
        #' # run in the background
        #' job$run(wait = TRUE)
        #' # see job status
        #' job$status()
        #'
        #' # force to kill background job before running the new one
        #' job$run(force = TRUE)
        #'
        #' # do not show anything in the console
        #' job$run(echo = FALSE)
        #'
        #' # copy external files used in the model to simulation output directory
        #' job$run(copy_external = TRUE)
        #' }
        #'
        run = function (epw, dir = NULL, wait = TRUE, force = FALSE, echo = wait, copy_external = FALSE)
            job_run(self, private, epw, dir, wait, force, echo, copy_external),
        # }}}

        # kill {{{
        #' @description
        #' Kill current running job
        #'
        #' @details
        #' `$kill()` kills the background EnergyPlus process if possible. It
        #' only works when simulation runs in non-waiting mode.
        #'
        #' @return A single logical value of `TRUE` or `FALSE`, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' job$kill()
        #' }
        #'
        kill = function ()
            job_kill(self, private),
        # }}}

        # status {{{
        #' @description
        #' Get the job status
        #'
        #' @details
        #' `$status()` returns a named list of values that indicates the status of the
        #' job:
        #'
        #' * `run_before`: `TRUE` if the job has been run before. `FALSE` otherwise.
        #' * `alive`: `TRUE` if the simulation is still running in the background.
        #'   `FALSE` otherwise.
        #' * `terminated`: `TRUE` if the simulation was terminated during last
        #'    simulation. `FALSE` otherwise. `NA` if the job has not been run yet.
        #' * `successful`: `TRUE` if last simulation ended successfully. `FALSE`
        #'   otherwise. `NA` if the job has not been run yet.
        #' * `changed_after`: `TRUE` if the IDF file has been changed since last
        #'    simulation. `FALSE` otherwise. `NA` if the job has not been run yet.
        #'
        #' @return A named list of 5 elements.
        #'
        #' @examples
        #' \dontrun{
        #' job$status()
        #' }
        #'
        status = function ()
            job_status(self, private),
        # }}}

        # errors {{{
        #' @description
        #' Read simulation errors
        #'
        #' @details
        #' $errors() returns an [ErrFile][read_err()] object which contains all
        #' contents of the simulation error file (`.err`). If `info` is `FALSE`,
        #' only warnings and errors are printed.
        #'
        #' @param info If `FALSE`, only warnings and errors are printed.
        #'        Default: `FALSE`.
        #'
        #' @return An [ErrFile][read_err()] object.
        #'
        #' @examples
        #' \dontrun{
        #' job$errors()
        #'
        #' # show all information
        #' job$errors(info = TRUE)
        #' }
        #'
        errors = function (info = FALSE)
            job_output_errors(self, private, info),
        # }}}

        # output_dir {{{
        #' @description
        #' Get simulation output directory
        #'
        #' @details
        #' `$output_dir()` returns the output directory of simulation results.
        #'
        #' @param open If `TRUE`, the output directory will be opened.
        #'
        #' @examples
        #' \dontrun{
        #' job$output_dir()
        #'
        #' # Below will open output directory
        #' # job$output_dir(open = TRUE)
        #' }
        #'
        output_dir = function (open = FALSE)
            job_output_dir(self, private, open),
        # }}}

        # list_files {{{
        #' @description
        #' List all output files in current simulation
        #'
        #' @details
        #' `$list_files()` returns all input and output files for current
        #' EnergyPlus simulation.
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
        #' @param simplify If `TRUE`, a character vector of EnergyPlus input
        #' and output file names in the output directory is given. If `FALSE`, a
        #' full named list of all possible input and output types is given. `NA`
        #' is returned if no input or output files are found for that type.
        #' Default: `FALSE`.
        #'
        #' @param full If `TRUE`, the full file paths in the output directory
        #' are returned. Otherwise, only the file names are returned. Default:
        #' `FALSE`.
        #'
        #' @return If `FALSE`, a character vector. Otherwise, a named list.
        #'
        #' @examples
        #' \dontrun{
        #' # list all files in the output directory
        #' job$list_files(simplify = TRUE)
        #'
        #' # get a full list of all possible inputs and outputs even though they
        #' # may not exist for current simulation
        #' job$list_files()
        #'
        #' # return the full paths instead of just file names
        #' job$locate_output(full = TRUE)
        #' }
        #'
        list_files = function (simplify = FALSE, full = FALSE)
            job_list_files(self, private, simplify, full),
        # }}}

        # locate_output {{{
        #' @description
        #' Get path of output file
        #'
        #' @details
        #' `$locate_output()` returns the path of a single output file specified
        #' by file suffix.
        #'
        #' @param suffix A string that indicates the file extension of
        #'        simulation output. Default: `".err"`.
        #' @param strict If `TRUE`, it will check if the simulation was
        #'        terminated, is still running or the file exists or not.
        #'        Default: `TRUE`.
        #'
        #' @examples
        #' \dontrun{
        #' # get the file path of the error file
        #' job$locate_output(".err", strict = FALSE)
        #'
        #' # can use to detect if certain output file exists
        #' job$locate_output(".expidf", strict = TRUE)
        #' }
        #'
        locate_output = function (suffix = ".err", strict = TRUE)
            job_locate_output(self, private, suffix, strict, must_exist = strict),
        # }}}

        # read_rdd {{{
        #' @description
        #' Read Report Data Dictionary (RDD) file
        #'
        #' @details
        #' `$read_rdd()` return the core data of Report Data Dictionary (RDD)
        #' file. For details, please see [read_rdd()].
        #'
        #' @return An [RddFile][read_rdd()] object.
        #'
        #' @examples
        #' \dontrun{
        #' job$read_rdd()
        #' }
        #'
        read_rdd = function ()
            job_read_rdd(self, private),
        # }}}

        # read_mdd {{{
        #' @description
        #' Read Report Data Dictionary (RDD) file
        #'
        #' @details
        #' `$read_mdd()` return the core data of Meter Data Dictionary (MDD)
        #' file. For details, please see [read_mdd()].
        #'
        #' @return An [MddFile][read_mdd()] object.
        #'
        #' @examples
        #' \dontrun{
        #' job$read_mdd()
        #' }
        #'
        read_mdd = function ()
            job_read_mdd(self, private),
        # }}}

        # list_table {{{
        #' @description
        #' List all table names in EnergyPlus SQL output
        #'
        #' @details
        #' `$list_table()` returns all available table and view names in the
        #' EnergyPlus SQLite file.
        #'
        #' @return A character vector
        #'
        #' @examples
        #' \dontrun{
        #' job$list_table()
        #' }
        #'
        list_table = function ()
            job_list_table(self, private),
        # }}}

        # read_table {{{
        #' @description
        #' Read a single table from EnergyPlus SQL output
        #'
        #' @details
        #' `$read_table()` takes a valid table `name` of those from
        #' \href{../../eplusr/html/EplusJob.html#method-list_table}{\code{$list_table()}}
        #' and returns that table data in a [data.table::data.table()] format.
        #'
        #' @param name A single string specifying the name of table to read.
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' # read a specific table
        #' job$read_table("Zones")
        #' }
        #'
        read_table = function (name)
            job_read_table(self, private, name),
        # }}}

        # report_data_dict {{{
        #' @description
        #' Read report data dictionary from EnergyPlus SQL output
        #'
        #' @details
        #' `$report_data_dict()` returns a [data.table::data.table()] which
        #' contains all information about report data.
        #'
        #' For details on the meaning of each columns, please see "2.20.2.1
        #' ReportDataDictionary Table" in EnergyPlus "Output Details and
        #' Examples" documentation.
        #'
        #' @return A [data.table::data.table()] of 10 columns:
        #'
        #' * `report_data_dictionary_index`: The integer used to link the
        #'   dictionary data to the variable data. Mainly useful when joining
        #'   different tables
        #' * `is_meter`: Whether report data is a meter data. Possible values:
        #'   `0` and `1`
        #' * `timestep_type`: Type of data timestep. Possible values: `Zone` and
        #'   `HVAC System`
        #' * `key_value`: Key name of the data
        #' * `name`: Actual report data name
        #' * `reporting_frequency`: Data reporting frequency
        #' * `schedule_name`: Name the the schedule that controls reporting
        #'     frequency.
        #' * `units`: The data units
        #'
        #' @examples
        #' \dontrun{
        #' job$report_data_dict()
        #' }
        #'
        report_data_dict = function ()
            job_report_data_dict(self, private),
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
        #'   * `case`: Simulation case specified using `case` argument
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
        #' @param key_value A character vector to identify key values of the
        #'        data. If `NULL`, all keys of that variable will be returned.
        #'        `key_value` can also be data.frame that contains `key_value`
        #'        and `name` columns. In this case, `name` argument in
        #'        `$report_data()` is ignored. All available `key_value` for
        #'        current simulation output can be obtained using
        #'        \href{../../eplusr/html/EplusSql.html#method-report_data_dict}{\code{$report_data_dict()}}.
        #'        Default: `NULL`.
        #'
        #' @param name A character vector to identify names of the data. If
        #'        `NULL`, all names of that variable will be returned. If
        #'        `key_value` is a data.frame, `name` is ignored. All available
        #'        `name` for current simulation output can be obtained using
        #'        \href{../../eplusr/html/EplusSql.html#method-report_data_dict}{\code{$report_data_dict()}}.
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
        #'        \href{../../eplusr/html/EplusSql.html#method-read_table}{\code{$read_table("Time")}}.
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
        #'        \href{../../eplusr/html/EplusSql.html#method-read_table}{\code{$read_table("Time")}}.
        #'        If `NULL`, all simulation days will be used. Default: `NULL`.
        #'
        #' @param day_type A character vector to specify which day type of data
        #'        to extract. All possible day types are: `Sunday`, `Monday`,
        #'        `Tuesday`, `Wednesday`, `Thursday`, `Friday`, `Saturday`,
        #'        `Holiday`, `SummerDesignDay`, `WinterDesignDay`, `CustomDay1`,
        #'        and `CustomDay2`. All possible values for current simulation
        #'        output can be obtained using
        #'        \href{../../eplusr/html/EplusSql.html#method-read_table}{\code{$read_table("Time")}}.
        #'
        #' @param environment_name A character vector to specify which
        #'        environment data to extract. If `NULL`, all environment data
        #'        are returned. Default: `NULL`. All possible
        #'        `environment_name` for current simulation output can be
        #'        obtained using:
        #' ```
        #' $read_table("EnvironmentPeriods")
        #' ```
        #'
        #' @return A [data.table::data.table()].
        #'
        #' @examples
        #' \dontrun{
        #' # read all report data
        #' job$report_data()
        #'
        #' # specify output variables using report data dictionary
        #' dict <- job$report_data_dict()
        #' job$report_data(dict[units == "C"])
        #'
        #' # specify output variables using 'key_value' and 'name'
        #' job$report_data("environment", "site outdoor air drybulb temperature")
        #'
        #' # explicitly specify year value and time zone
        #' job$report_data(dict[1], year = 2020, tz = "Etc/GMT+8")
        #'
        #' # explicitly specify case name
        #' job$report_data(dict[1], case = "example")
        #'
        #' # get all possible columns
        #' job$report_data(dict[1], all = TRUE)
        #'
        #' # return in a format that is similar as EnergyPlus CSV output
        #' job$report_data(dict[1], wide = TRUE)
        #'
        #' # return in a format that is similar as EnergyPlus CSV output with
        #' # extra columns
        #' job$report_data(dict[1], wide = TRUE, all = TRUE)
        #'
        #' # only get data at the working hour on the first Monday
        #' job$report_data(dict[1], hour = 8:18, day_type = "monday", simulation_days = 1:7)
        #'
        #' # only get specified run period data
        #' job$read_table("EnvironmentPeriods") # possible environment name
        #' job$report_data(dict[1], environment_name = "San Francisco Intl Ap CA USA TMY3 WMO#=724940")
        #' # can also be done using 'environment_period_index' column
        #' job$report_data(dict[1], all = TRUE)[environment_period_index == 3L]
        #' }
        #'
        report_data = function (key_value = NULL, name = NULL, year = NULL,
                                tz = "UTC", case = "auto", all = FALSE, wide = FALSE,
                                period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                                interval = NULL, simulation_days = NULL, day_type = NULL,
                                environment_name = NULL)
            job_report_data(self, private, key_value = key_value, name = name, year = year,
                tz = tz, case = case, all = all, wide = wide,
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
        #' * `case`: Simulation case specified using `case` argument
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
        #' @param report_name,report_for,table_name,column_name,row_name Each is
        #'        a character vector for subsetting when querying the SQL
        #'        database.  For the meaning of each argument, please see the
        #'        description above.
        #' @param wide If `TRUE`, each table will be converted into the similar
        #'        format as it is shown in EnergyPlus HTML output file. Default:
        #'        `FALSE`.
        #' @param string_value Only applicable when `wide` is `TRUE`. If
        #'        `string_value` is `FALSE`, instead of keeping all values as
        #'        characters, values in possible numeric columns are converted
        #'        into numbers. Default: the opposite of `wide`. Possible
        #'        numeric columns indicate column that:
        #' * columns that have associated units
        #' * columns that contents numbers
        #'
        #' @return A [data.table::data.table()] with 8 columns (when `wide` is
        #' `FALSE`) or a named list of [data.table::data.table()]s where the
        #' names are the combination of `report_name`, `report_for` and
        #' `table_name`.
        #'
        #' @examples
        #' \dontrun{
        #' # read all tabular data
        #' job$tabular_data()
        #'
        #' # explicitly specify data you want
        #' str(job$tabular_data(
        #'     report_name = "AnnualBuildingUtilityPerformanceSummary",
        #'     table_name = "Site and Source Energy",
        #'     column_name = "Total Energy",
        #'     row_name = "Total Site Energy"
        #' ))
        #'
        #' # get tabular data in wide format and coerce numeric values
        #' str(job$tabular_data(
        #'     report_name = "AnnualBuildingUtilityPerformanceSummary",
        #'     table_name = "Site and Source Energy",
        #'     column_name = "Total Energy",
        #'     row_name = "Total Site Energy",
        #'     wide = TRUE, string_value = FALSE
        #' ))
        #' }
        #'
        tabular_data = function(report_name = NULL, report_for = NULL, table_name = NULL,
                                column_name = NULL, row_name = NULL, wide = FALSE, string_value = !wide)
            job_tabular_data(self, private, report_name = report_name,
                report_for = report_for, table_name = table_name,
                column_name = column_name, row_name = row_name,
                wide = wide, string_value = string_value),
        # }}}

        # print {{{
        #' @description
        #' Print `EplusSql` object
        #'
        #' @details
        #' `$print()` shows the core information of this `EplusJob` object,
        #' including the path of model and weather, the version and path of
        #' EnergyPlus used to run simulations, and the simulation job status.
        #'
        #' `$print()` is quite useful to get the simulation status, especially
        #' when `wait` is `FALSE` in `$run()`. The job status will be updated
        #' and printed whenever `$print()` is called.
        #'
        #' @return The `EplusSql` object itself, invisibly.
        #'
        #' @examples
        #' \dontrun{
        #' job$print()
        #' }
        #'
        print = function ()
            job_print(self, private)
        # }}}
        # }}}
    ),

    private = list(
        # PRIVATE FIELDS {{{
        m_idf = NULL,
        m_epw_path = NULL,
        m_job = NULL,
        m_log = NULL,
        # }}}

        # PRIVATE FUNCTIONS {{{
        uuid = function () private$m_log$uuid,
        log_new_uuid = function () log_new_uuid(private$m_log),

        seed_uuid = function () get_priv_env(private$m_idf)$m_log$uuid,
        log_seed_uuid = function () private$m_log$seed_uuid <- get_priv_env(private$m_idf)$m_log$uuid,
        cached_seed_uuid = function () private$m_log$seed_uuid,

        is_unsaved = function () private$m_log$unsaved,
        log_saved = function () log_saved(private$m_log),
        log_unsaved = function () log_unsaved(private$m_log)
        # }}}
    )
)
# }}}

#' Create an EnergyPlus Simulation Job
#'
#' `eplus_job()` takes an IDF and EPW as input, and returns an `EplusJob` object
#' for running EnergyPlus simulation and collecting outputs.
#'
#' @param idf A path to an local EnergyPlus IDF file or an `Idf` object.
#' @param epw A path to an local EnergyPlus EPW file or an `Epw` object. `epw`
#'        can also be `NULL` which will force design-day-only simulation when
#'        [`$run()`][EplusJob] method is called. Note this needs at least one
#'        `Sizing:DesignDay` object exists in the [Idf].
#' @return An `EplusJob` object.
#' @seealso [param_job()] for creating an EnergyPlus parametric job.
#' @name EplusJob
#' @export
# eplus_job {{{
eplus_job <- function (idf, epw) {
    EplusJob$new(idf, epw)
}
# }}}

# job_initialize {{{
job_initialize <- function (self, private, idf, epw) {
    # add Output:SQLite and Output:VariableDictionary if necessary
    private$m_idf <- get_init_idf(idf)
    if (!is.null(epw)) private$m_epw_path <- get_init_epw(epw)

    # log if the input idf has been changed
    private$m_log <- new.env(hash = FALSE, parent = emptyenv())
    private$m_log$unsaved <- attr(private$m_idf, "sql") || attr(private$m_idf, "dict")

    # save uuid
    private$log_seed_uuid()
    private$log_new_uuid()
}
# }}}
# job_version {{{
job_version <- function (self, private) {
    private$m_idf$version()
}
# }}}
# job_path {{{
job_path <- function (self, private, type = c("all", "idf", "epw")) {
    type <- match.arg(type)

    path_epw <- if (is.null(private$m_epw_path)) NA_character_ else private$m_epw_path
    switch(type,
        all = c(idf = private$m_idf$path(), epw = path_epw),
        idf = private$m_idf$path(), epw = path_epw
    )
}
# }}}
# job_run {{{
job_run <- function (self, private, epw, dir = NULL, wait = TRUE, force = FALSE,
                     echo = wait, copy_external = FALSE, readvars = TRUE) {
    # stop if idf object has been changed accidentally
    if (!identical(private$seed_uuid(), private$cached_seed_uuid())) {
        abort(paste0("The Idf has been modified after job was created. ",
            "Running this Idf will result in simulation outputs that may be not reproducible.",
            "Please recreate the job using new Idf and then run it."
        ))
    }

    if (missing(epw)) epw <- private$m_epw_path

    if (is.null(epw)) {
        private$m_epw_path <- NULL
        path_epw <- NULL
    } else {
        private$m_epw_path <- get_init_epw(epw)
        path_epw <- private$m_epw_path
    }

    path_idf <- private$m_idf$path()
    if (is.null(dir))
        run_dir <- dirname(path_idf)
    else {
        run_dir <- dir
        path_idf <- normalizePath(file.path(run_dir, basename(path_idf)), mustWork = FALSE)
    }

    # if necessary, resave the model
    if (private$is_unsaved() || !is.null(dir)) {
        path_idf <- private$m_idf$save(path_idf, overwrite = TRUE, copy_external = copy_external)
        private$log_seed_uuid()
        private$log_saved()
    }

    # when no epw is given, at least one design day object should exists
    if (is.null(private$m_epw_path)) {
        if (!private$m_idf$is_valid_class("SizingPeriod:DesignDay")) {
            abort(paste0("When no weather file is given, input IDF should contain ",
                "at least one 'SizingPeriod:DesignDay' object to enable ",
                "Design-Day-only simulation."
            ))
        }
    }

    # check if the model is still running
    proc <- private$m_job
    if (!is.null(proc)) {
        if (inherits(proc, "process") && proc$is_alive()) {
            pid <- proc$get_pid()
            if (force) {
                verbose_info("Force to kill current running simulation (PID: ", pid,
                    ") and start a new simulation...")
                suppressMessages(self$kill())
            } else {
                abort(paste0("The simulation of current Idf is still running (PID: ",
                    pid, "). Please set `force` to TRUE if you want ",
                    "to kill the running process and start a new simulation."
                ))
            }
        }
    }

    # reset status
    private$m_log$start_time <- current()
    private$m_log$killed <- NULL

    resrc <- NULL
    if (copy_external) {
        # check if external file dependencies are found
        resrc <- private$m_idf$external_deps()
        if (!length(resrc)) resrc <- NULL
    }

    private$m_job <- energyplus(
        model = path_idf, weather = path_epw, design_day = is.null(private$m_epw_path),
        eso_to_ip = FALSE, readvars = readvars, echo = echo, wait = wait,
        eplus = private$m_idf$version(), resources = resrc
    )

    private$log_new_uuid()
    if (wait) private$m_log$end_time <- current()

    self
}
# }}}
# job_kill {{{
job_kill <- function (self, private) {
    if (is.null(private$m_job)) {
        verbose_info("The job has not been run yet.")
        return(invisible(FALSE))
    }

    proc <- private$m_job

    if (!inherits(proc, "process") || !proc$is_alive()) {
        verbose_info("The job is not running.")
        return(invisible(FALSE))
    }

    k <- tryCatch(proc$kill(), error = function (e) FALSE)

    if (isTRUE(k)) {
        private$m_log$killed <- TRUE
        verbose_info("The job has been successfully killed.")
        return(invisible(TRUE))
    } else {
        verbose_info("Failed to kill the job, because it was already finished/dead.")
        return(invisible(FALSE))
    }
}
# }}}
# job_status {{{
job_status <- function (self, private) {
    # init
    status <- list(
        run_before = FALSE, # if the model has been run before
        alive = FALSE, # if simulation is still running
        terminated = NA, # if last simulation was terminated
        successful = NA, # if last simulation was successful
        changed_after = NA # if the model has been changed after last simulation
    )

    proc <- private$m_job

    # if the model has not been run before
    if (is.null(proc)) {
        if (!file.exists(private$m_idf$path())) {
            warn(sprintf("Could not find local idf file '%s'.", surround(private$m_idf$path())))
        }
        return(status)
    }

    status$run_before <- TRUE

    if (isTRUE(private$m_log$killed)) {
        status$terminated <- TRUE
    } else {
        status$terminated <- FALSE
    }

    # check if the model is still running
    if (inherits(proc, "process") && proc$is_alive()) {
        status$alive <- TRUE
    } else {
        status$alive <- FALSE

        # in waiting mode
        if (!inherits(proc, "process")) {
            exit_status <- proc$exit_status
        # in non-waiting mode
        } else {
            proc$wait()
            private$m_job <- proc$get_result()
            exit_status <- private$m_job$exit_status
        }

        if (!is.na(exit_status) && exit_status == 0L) {
            status$successful <- TRUE
        } else {
            status$successful <- FALSE
        }
    }

    status$changed_after <- FALSE
    if (!identical(private$cached_seed_uuid(), private$seed_uuid())) {
        status$changed_after <- TRUE
    }

    status
}
# }}}
# job_output_dir {{{
job_output_dir <- function (self, private, open = FALSE) {
    dir <- normalizePath(dirname(private$m_idf$path()), mustWork = FALSE)
    if (!open) return(dir)
    if (open) {
        if (is.null(dir)) {
            verbose_info("No simulation has been run yet.")
            return(invisible(dir))
        }

        # Reference:
        # http://r.789695.n4.nabble.com/R-command-to-open-a-file-quot-browser-quot-on-Windows-and-Mac-td4710688.html
        if (is_windows()) {
            shell.exec(dir)
        } else if (is_macos()) {
            system2("open", dir)
        } else if (is_linux()) {
            system(paste0("xdg-open ", dir))
        } else {
            verbose_info("Current platform not supported.")
        }
    }
    dir
}
# }}}
# job_list_files {{{
job_list_files <- function (self, private, simplify = FALSE, full = FALSE) {
    assert_flag(simplify)
    assert_flag(full)

    status <- job_status(self, private)

    if (!isTRUE(status$run_before)) {
        stop("Simulation did not run before. Please run it using `$run()` ",
            "before collect output", call. = FALSE)
    }

    if (isTRUE(status$terminated))
        stop("Simulation was terminated before. Please solve ",
            "the problems and re-run the simulation before collect ",
            "output", call. = FALSE)

    if (isTRUE(status$alive))
        stop("Simulation is still running. Please wait simulation ",
            "to finish before collecting results.", call. = FALSE)

    files <- private$m_job$file
    if (simplify) {
        files <- unlist(files, FALSE, FALSE)
        files <- files[!is.na(files)]

        if (full) {
            files <- file.path(job_output_dir(self, private), files)
            files <- normalizePath(files, mustWork = FALSE)
        }
    } else if (full) {
        files <- lapply(files, function(f) {
            if (all(is.na(f))) {
                f
            } else {
                normalizePath(file.path(job_output_dir(self, private), f), mustWork = FALSE)
            }
        })
    }

    files
}
# }}}
# job_locate_output {{{
job_locate_output <- function (self, private, suffix = ".err", strict = TRUE, must_exist = TRUE) {
    out <- paste0(tools::file_path_sans_ext(private$m_idf$path()), suffix)

    if (strict) {
        status <- job_status(self, private)

        if (!isTRUE(status$run_before)) {
            stop("Simulation did not run before. Please run it using `$run()` ",
                "before collect output", call. = FALSE)
        }

        if (isTRUE(status$terminated))
            stop("Simulation was terminated before. Please solve ",
                "the problems and re-run the simulation before collect ",
                "output", call. = FALSE)

        if (isTRUE(status$alive))
            stop("Simulation is still running. Please wait simulation ",
                "to finish before collecting results.", call. = FALSE)

        if (isTRUE(status$changed_after))
            warning("The Idf has been changed since last simulation. ",
                "The simulation output may not be correct.", call. = FALSE)

        if (isTRUE(status$run_before) && !isTRUE(status$successful))
            warning("Simulation ended with errors. Simulation results ",
                "may not be correct.", call. = FALSE)

    }

    if (must_exist) checkmate::assert_file_exists(out, "r", .var.name = "output file")

    out
}
# }}}
# job_output_errors {{{
job_output_errors <- function (self, private, info = FALSE) {
    path_err <- job_locate_output(self, private, ".err")

    err <- parse_err_file(path_err)

    if (!info) err[!J("Info"), on = "level"] else err
}
# }}}
# job_sql_path {{{
job_sql_path <- function (self, private) {
    path_sql <- job_locate_output(self, private, ".sql", must_exist = FALSE)
    checkmate::assert_file_exists(path_sql, "r", .var.name = "Simulation SQL output")
    path_sql
}
# }}}
# job_rdd_path {{{
job_rdd_path <- function (self, private, type = c("rdd", "mdd")) {
    type <- match.arg(type)
    path <- job_locate_output(self, private, paste0(".", type), must_exist = FALSE)
    name <- switch(type,
        rdd = "Report Data Dictionary (RDD) file",
        mdd = "Meter Data Dictionary (MDD) file"
    )

    checkmate::assert_file_exists(path, "r", .var.name = name)

    path
}
# }}}
# job_list_table {{{
job_list_table <- function (self, private) {
    list_sql_table(job_sql_path(self, private))
}
# }}}
# job_read_rdd {{{
job_read_rdd <- function (self, private) {
    read_rdd(job_rdd_path(self, private, "rdd"))
}
# }}}
# job_read_mdd {{{
job_read_mdd <- function (self, private) {
    read_mdd(job_rdd_path(self, private, "mdd"))
}
# }}}
# job_read_table {{{
job_read_table <- function (self, private, table) {
    read_sql_table(job_sql_path(self, private), table)
}
# }}}
# job_report_data_dict {{{
job_report_data_dict <- function (self, private) {
    get_sql_report_data_dict(job_sql_path(self, private))
}
# }}}
# job_report_data {{{
job_report_data <- function (self, private, key_value = NULL, name = NULL, year = NULL,
                             tz = "UTC", case = "auto", all = FALSE, wide = FALSE,
                             period = NULL, month = NULL, day = NULL, hour = NULL, minute = NULL,
                             interval = NULL, simulation_days = NULL, day_type = NULL,
                             environment_name = NULL) {
    if (identical(case, "auto")) case <- tools::file_path_sans_ext(basename(job_sql_path(self, private)))
    get_sql_report_data(job_sql_path(self, private),
        key_value = key_value, name = name, year = year,
        tz = tz, case = case, all = all, wide = wide,
        period = period, month = month, day = day, hour = hour, minute = minute,
        interval = interval, simulation_days = simulation_days, day_type = day_type,
        environment_name = environment_name
    )
}
# }}}
# job_tabular_data {{{
job_tabular_data <- function (self, private, report_name = NULL, report_for = NULL,
                              table_name = NULL, column_name = NULL, row_name = NULL,
                              case = "auto", wide = FALSE, string_value = !wide) {
    if (identical(case, "auto")) case <- tools::file_path_sans_ext(basename(job_sql_path(self, private)))
    get_sql_tabular_data(job_sql_path(self, private), report_name = report_name, report_for = report_for,
        table_name = table_name, column_name = column_name, row_name = row_name,
        case = case, wide = wide, string_value = string_value)
}
# }}}
# job_print {{{
job_print <- function (self, private) {
    path_epw <- if (is.null(private$m_epw_path)) NULL else private$m_epw_path
    print_job_header(title = "EnergPlus Simulation Job",
        path_idf = private$m_idf$path(),
        path_epw = path_epw,
        eplus_ver = private$m_idf$version(),
        name_idf = "Model", name_epw = "Weather"
    )
    status <- job_status(self, private)

    if (!status$run_before) {
        cli::cat_line("< Simulation has not been run before >",
            col = "white", background_col = "blue")
    } else if (isTRUE(status$terminated)) {
        cli::cat_line(" Simulation was terminated before.",
            col = "white", background_col = "red")
    } else if (status$alive) {
        cli::cat_line(" Simulation started at ",
            surround(private$m_log$start_time), " and is still running...",
            col = "black", background_col = "green")
    } else if (!isTRUE(status$successful)) {
        cli::cat_line(" Simulation started at ",
            surround(private$m_log$start_time), " and ended unsuccessfully...",
            col = "white", background_col = "red")
    } else {
        if (is.null(private$m_log$end_time)) {
            private$m_log$end_time <- private$m_job$end_time
        }

        run_time <- format(round(difftime(
            private$m_log$end_time, private$m_log$start_time), digits = 2L)
        )

        cli::cat_line(" Simulation started at ",
            surround(private$m_log$start_time), " and completed successfully after ",
            run_time, ".", col = "black", background_col = "green"
        )
    }
}
# }}}

# S3 EplusJob methods {{{
#' @export
str.EplusSql <- function (object, ...) {
    object$print()
}

#' @export
format.EplusSql <- function (x, ...) {
    paste0(utils::capture.output(x$print()), collapse = "\n")
}

#' @export
`==.EplusJob` <- function (e1, e2) {
    if (!inherits(e2, "EplusJob")) return(FALSE)
    identical(get_priv_env(e1)$uuid(), get_priv_env(e2)$uuid())
}

#' @export
`!=.EplusJob` <- function (e1, e2) {
    Negate(`==.EplusJob`)(e1, e2)
}
# }}}

# helper
# get_init_idf {{{
get_init_idf <- function (idf, sql = TRUE, dict = TRUE) {
    idf <- if (!is_idf(idf)) read_idf(idf) else idf$clone(deep = TRUE)

    if (is.null(idf$path())) {
        abort("The Idf object is not created from local file. Please save it using `$save()` before running.", "idf_not_local")
    }

    if (!utils::file_test("-f", idf$path())) {
        abort(paste0("Failed to locate the local IDF file of input Idf object. ",
            "Path: ", surround(idf$path()), " ",
            "Please re-save it to disk using `$save()` before running."
        ), "idf_path_not_exist")
    }

    if (idf$is_unsaved()) {
        abort("Idf has been modified since read or last saved. Please save it using `$save()` before running.", "idf_not_saved")
    }

    # add Output:SQLite if necessary
    if (sql) sql <- idf_add_output_sqlite(idf) || idf_set_output_files(idf, sql = TRUE)
    setattr(idf, "sql", sql)

    # add Output:VariableDictionary if necessary
    if (dict) dict <- idf_add_output_vardict(idf) || idf_set_output_files(idf, dict = TRUE)
    setattr(idf, "dict", dict)

    idf
}
# }}}
# get_init_epw {{{
#' @importFrom checkmate test_string
get_init_epw <- function (epw) {
    if (checkmate::test_string(epw)) {
        if (!utils::file_test("-f", epw)) {
            abort(paste0("Input EPW file does not exist. ",
                "Path: ", surround(normalizePath(epw, mustWork = FALSE))
            ), "epw_path_not_exist")
        }
        path <- epw
    } else {
        epw <- if (!is_epw(epw)) read_epw(epw) else epw$clone(deep = TRUE)

        if (is.null(epw$path())) {
            abort("The Epw object is not created from local file. Please save it using `$save()` before running.", "epw_not_local")
        }

        if (!utils::file_test("-f", epw$path())) {
            abort(paste0("Failed to locate the local EPW file of input Epw object. ",
                "Path: ", surround(epw$path()), " ",
                "Please re-save it to disk using `$save()` before running."
            ), "epw_path_not_exist")
        }

        if (epw$is_unsaved()) {
            abort("Epw has been modified since read or last saved. Please save it using `$save()` before running.", "epw_not_saved")
        }

        path <- epw$path()
    }

    normalizePath(path)
}
# }}}
# print_job_header {{{
print_job_header <- function (title = "EnergyPlus Simulation Job", path_idf, path_epw,
                              eplus_ver = read_idf(path_idf)$version(),
                              name_idf = "Model", name_epw = "Weather") {
    cli::cat_rule(title, col = "green")
    config <- eplus_config(eplus_ver)
    if (is.null(config)) {
        path_eplus <- "<< Not Found >>"
        eplus_ver <- surround(eplus_ver)
    } else {
        path_eplus <- surround(normalizePath(config$dir))
        eplus_ver <- surround(config$version)
    }

    path_idf <- normalizePath(path_idf, mustWork = FALSE)
    path_epw <- if (is.null(path_epw)) {
        "<< Not specified >>"
    } else {
        normalizePath(path_epw, mustWork = FALSE)
    }

    cli::cat_line(c(
        str_trunc(paste0("* ", name_idf, ": ", surround(path_idf))),
        str_trunc(paste0("* ", name_epw, ": ", surround(path_epw))),
        paste0("* EnergyPlus Version: ", eplus_ver),
        str_trunc(paste0("* EnergyPlus Path: ", path_eplus))
    ))
}
# }}}
