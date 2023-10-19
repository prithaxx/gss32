# gss32

## Setting Up the Server

### Running Locally in the Shell

1.  Start by installing R
1.  Install the Shiny library \
    ```
    $ sudo R
    > update.packages(ask = FALSE, checkBuilt = TRUE)
    > install.packages("shiny")
    > install.packages("shinyjs")
    ```
1.  Run the server: `sudo Rscript app.R`
1.  The server may complain about certain libraries not being available and stop. If this is the case, you might just have to keep following the dependency chain, installing the next round of packages and trying to run again until it stops complaining about missing libraries. Why there isn't better dependency management, I jsut don't know...

#### Environment Configs

You might need to specify the host IP and port by setting the `GSS32_HOST` and `GSS32_PORT` environment variables.

If that doesn't work for you, you can update the server settings directly in `app.R` (just make sure not to commit any of those changes!):

```r
options <- list(
  "host" = "123.456.789.0",
  "port" = 4085
)

shinyApp(ui = ui, server = server, options = options)
```

### Running the App Using Shiny Server

Shiny Server is exactly what it sounds like, a web server for hosting Shiny apps if you are only working on this app, this might be overkill, but it does run as a background service.

Install instructions are at https://docs.posit.co/shiny-server. Once the server is installed, download or clone the GSS32 app code to the appropriate directory (`/srv/shiny-server/` by default), and then navigate to `http://<your_ip_or_hostname>:3838/gss32`. If you want this to be an externally visible server, you can set the Shiny Server port to 80 (433 if SSL is configured) or use a proxy like nginx to proxy for the locally-bound server. 