* Object reference:
  - Classes like `Branch` and `ZoneHVAC:EquipmentList` reference other objects
    in fields "Component X Object Type" and "Zone Equipment 1 Object Type", but
    they do not have `\object-list` attributes. This makes it impossible to
    detect invalid field references.
  - One possible way is to use `$search_value()` to detect if the values in
    those fields could be targeted or not.

* Model version conversion

* Fix extensible field names when they should be "A123" or "N123" but not
  self-explainary

* Apply multiple weather file to a seed in `ParametricJob` class

* Add external file dependency (e.g. file existance in `Schedule:File` class)
  checking in `$validate()`.

* Add function to reorder objects in a class

* Remove crayon dep

* Add functionality to check location differences between epw and idf.
  ```cpp
  if ( std::abs( Latitude - WeatherFileLatitude ) > 1.0 || std::abs( Longitude - WeatherFileLongitude ) > 1.0 || std::abs( TimeZoneNumber - WeatherFileTimeZone ) > 0.0 || std::abs( Elevation - WeatherFileElevation ) / max( Elevation, 1.0 ) > 0.10 ) {
      ShowWarningError( "Weather file location will be used rather than entered (IDF) Location object." );
      ShowContinueError( "..Location object=" + LocationTitle );
      ShowContinueError( "..Weather File Location=" + WeatherFileLocationTitle );
      ShowContinueError( "..due to location differences, Latitude difference=[" + RoundSigDigits( std::abs( Latitude - WeatherFileLatitude ), 2 ) + "] degrees, Longitude difference=[" + RoundSigDigits( std::abs( Longitude - WeatherFileLongitude ), 2 ) + "] degrees." );
      ShowContinueError( "..Time Zone difference=[" + RoundSigDigits( std::abs( TimeZoneNumber - WeatherFileTimeZone ), 1 ) + "] hour(s), Elevation difference=[" + RoundSigDigits( std::abs( ( Elevation - WeatherFileElevation ) / max( Elevation, 1.0 ) ) * 100.0, 2 ) + "] percent, [" + RoundSigDigits( std::abs( Elevation - WeatherFileElevation ), 2 ) + "] meters." );
  }
  ```

* Add a vignette about simulation data extraction and modifying models using
  functions.

* Use Rcpp to unlock environment. Reference: https://stackoverflow.com/questions/25910778/unlockenvironment-implemented-via-rcpp-instead-of-inline/25922051#25922051

* Add `IdfObjectsClass` class to handle `IdfObject`s in same class.

* Use `rc.options()` to register custom completor. Reference: https://github.com/jimhester/completeme

* Add validation to check if character field value input contains comma or semi-colon.

* Add validation to check if character field value input length excess 100
  characters

* Create a RStudio Addin to insert field names in `$add()`, `$set()`.
