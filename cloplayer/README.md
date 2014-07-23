# cloplayer

Clojure Apache log file replayer.

## Installation

Download from http://github.com/jimm/clojure/cloplayer

## Building

    lein uberjar

## Usage

    # Without uberjar
    lein run apache-log-file hostname

    # With uberjar
    java -jar cloplayer-0.1.0-standalone.jar logfile host

## Options

* logfile - Apache log file
* host - Host to which log file should be replayed

## License

None. Go for it.
