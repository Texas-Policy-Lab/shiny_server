#' @title Creates the directory structure on the shiny server
#' @inheritParams deploy_app
#' @param dirs vector. A vector of directories to create on the shiny server.
#' @export
create_dir_str <- function(shiny_server_pth, url, dirs) {
  
  lapply(dirs, function(dir) {
    
    pth <- file.path(shiny_server_pth, url, dir)
    
    if (!dir.exists(pth)) {
      dir.create(pth)
    }
  })
  
  assertthat::assert_that(all(dir.exists(file.path(shiny_server_pth, url, dirs))))
  
}

#' @title Create fl structure on the shiny server
#' @inheritParams deploy_app
#' @param fls vector. A list of files to copy to the shiny server.
#' @export
create_fl_str <- function(shiny_server_pth, url, fls) {
  
  lapply(fls, function(fl) {
    
    to <- file.path(shiny_server_pth, url, 
                    fl)
    
    file.copy(from = fl,
              to = to, 
              overwrite = TRUE,
              recursive = FALSE, 
              copy.mode = TRUE)
  })
  
  assertthat::assert_that(all(file.exists(file.path(shiny_server_pth, url, fls))))
  
}

#' @title Remove old files
#' @inheritParams create_fl_str
#' @export
remove_old_fls <- function(shiny_server_pth, url, fls) {

  old_fls <- list.files(file.path(shiny_server_pth, url), recursive = TRUE)
  
  fls_to_rm <- setdiff(old_fls, fls)

  lapply(fls_to_rm, function(fl) {
    file.remove(fl)
  })

}

#' @title Deploy app
#' @description Copies folder and file structure to the shiny server
#' @param url string. A string indicating which directory to place the files to.
#' @param shiny_server_pth string. A string indicating the path to the shiny server.
#' @export
deploy_app <- function(url, shiny_server_pth = "/srv/shiny-server") {

  create_dir_str(shiny_server_pth = shiny_server_pth,
                 url = url,
                 dirs = list.dirs(".", recursive = TRUE, full.names = F)[-1])

  fls <- list.files(".", recursive = TRUE)

  remove_old_fls(shiny_server_pth = shiny_server_pth,
                 url = url,
                 fls = fls)

  create_fl_str(shiny_server_pth = shiny_server_pth,
                url = url,
                fls = fls)

}

#' @title Test config
#' @inheritParams deploy_main
#' @export
test_config <- function(deploy) {
  
  assertthat::assert_that(all(sapply(deploy %>% 
                                       purrr::map(purrr::pluck("run")), is.null) == FALSE),
                          msg = "Run is null. Should only take on the values TRUE or FALSE")

  run <- deploy %>% 
    purrr::map(magrittr::extract("run"))
  
  assertthat::assert_that(run$prod != run$dev,
                          msg = "Development and production must be set to TRUE and FALSE or FALSE and TRUE, they cannot be equal")
 
}

#' @title Prompt for production question
#' @export
prompt_prod_q <- function() {
  tolower(readline(prompt="Are you sure you want to deploy to production, enter 'y' or 'n':"))
}

#' @title Copy files
#' @param deploy list. A list containing the urls and run values for development and production.
#' @importFrom magrittr "%>%"
#' @export
deploy_main <- function(deploy) {
  
  test_config(deploy)
  
  deploy <- plyr::compact(lapply(deploy, function(x)
  { if(x$run) {return(x)}}))
  
  url <- deploy %>% 
    .[[1]] %>% 
    purrr::pluck("url")  
  
  if (names(deploy) == "prod") {
    
    x <- prompt_prod_q()    
    
    while (x %in% c('y', 'n') == F) {
      
      x <- prompt_prod_q()
    }
    
    if (x == "y") {
      deploy_app(url = url)
    }
    
  } else {
    
    deploy_app(url = url)
    
  }
  
}
