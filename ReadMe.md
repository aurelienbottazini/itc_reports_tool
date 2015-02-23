### It is a tool to download and analyze itunes connect reports.

First project in haskell. Please be tolerant :)

## Requirements:
1. [Haskell Platform](https://www.haskell.org/platform/)
2. _MissingH_ library • `cabal install MissingH`
3. Java • On Debian: `apt-get install default-jre`

## Tutorial

1. Compile • `ghc --make itc_reports_tool.hs`
2. rename `Autoingestion/autoingestion.properties.template` to `Autoingestion/autoingestion.properties` and put your credentials for itunes connect
3. First run with `./itc_reports_tool help` to get a list of commands

You can download your monthly and daily reports
You can view your sales for each app for a given period
You can group your app sales through a regular expression for a given period
