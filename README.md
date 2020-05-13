
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TPL Shiny server

The TPL Shiny server package contains functions which can be used on the
shiny server to help move files from development to production.

## Deploy App

The `deploy_app(url)` function takes a url and deploys the applications
folder structure and files to TPL’s shiny server. The shiny server is
currently set up such that any files which are moved to the location
/srv/shiny-server/ will be rendered on the server.

``` r
tplshinyserver::deploy_app(url)
```

## Staging and Production

Once a application is “live” in can be dangerous to update the
application without testing any changes to the application first. The
current working solution is to have a staging or development application
and a production application. Changes get push to the staging
application and then they are checked. Once it is verified that all the
changes look good, the changes can be pushed to the production
application.

The `deploy_main(deploy)` helps the user to deploy the application to
the development application and then to the production application. The
function takes a list called deploy which contains parameters. The
configuration file has to be set up as demonstrated below:

``` yaml
deploy:
  prod:
    url: "prod-url"
    run: FALSE
  dev:
    url: "dev-url"
    run: TRUE
```

``` r
tplshinyserver::deploy_main(deploy)
```

In the above scenario, the application will be deployed to the
development url since run: TRUE for development and FALSE for
production. The production run variable can never equal the development
run variable, they must be TRUE and FALSE or FALSE and TRUE.

To deploy to production change the file to look like this:

``` yaml
deploy:
  prod:
    url: "prod-url"
    run: TRUE
  dev:
    url: "dev-url"
    run: FALSE
```

### Sample config file

``` r

config <- yaml::read_yaml("./config.yaml")
deploy <- config$deploy
```
