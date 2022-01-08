# Rescuetime
## Setup
The `setup` function is made for convenience to add your Rescuetime api key as an environment variable. The default usage below will add your key to an _.Renviron_ file in the working directory. If you would like to add it to the user level _.Renviron_ use `"~/.Renviron"` for `.Renviron_path`. See `?setup` for details

```
setup("YOUR KEY")
```
See the following API endpoint documentation for specifics:

`?daily_summary`
`?analytic`
`?alerts`
`?highlights`
`?highlights_post`
`?start_focustime`
`?end_focustime`
`?focustime_started`
`?focustime_ended`
`?offline`
