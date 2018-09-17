#' List members the local agent sees in the cluster gossip pool
#' @return \code{data.table} with a member in each row
#' @export
#' @importFrom httr GET content
#' @importFrom data.table rbindlist as.data.table setnames
#' @references \url{https://www.consul.io/api/agent.html#list-members}
consular_agent_members <- function() {
    members <- content(GET(file.path(consular$agent_url, consular$version, 'agent', 'members')))
    tags <- rbindlist(lapply(members, function(member) {
        tags <- as.data.table(member$Tag)
        setnames(tags, names(tags), new = paste('tag', names(tags), sep = '_'))
    }), fill = TRUE)
    members <- rbindlist(lapply(members, function(member) {
        member$Tags <- NULL
        as.data.table(member)
    }), fill = TRUE)
    cbind(members, tags)
}


#' Show the configuration of the local agent
#' @return \code{list}
#' @export
#' @importFrom httr GET content
#' @references \url{https://www.consul.io/api/agent.html#read-configuration}
consular_agent_configuration <- function() {
    content(GET(file.path(consular$agent_url, consular$version, 'agent', 'self')))
}


#' Reloads the configuration of the local agent
#' @export
#' @importFrom httr PUT content
#' @references \url{https://www.consul.io/api/agent.html#reaload-configuration}
consular_agent_reload <- function() {
    invisible(content(PUT(file.path(consular$agent_url, consular$version, 'agent', 'reload'))))
}


#' List services registered with the local agent
#' @return \code{data.table} with a service in each row
#' @export
#' @importFrom httr GET content
#' @importFrom data.table rbindlist as.data.table
#' @references \url{https://www.consul.io/api/agent/service.html#list-services}
#' @examples \dontrun{
#' consular_agent_services()
#' }
consular_agent_services <- function() {
    services <- content(GET(file.path(consular$agent_url, consular$version, 'agent', 'services')))
    rbindlist(lapply(services, function(service) {
        suppressWarnings(as.data.table(service))
    }), fill = TRUE)
}


#' Register a new service to the local agent
#' @export
#' @importFrom httr PUT content
#' @importFrom jsonlite toJSON
#' @references \url{https://www.consul.io/api/agent/service.html#register-service}
#' @param payload list describing the service
#' @examples \dontrun{
#' consular_agent_register_service(list(
#'   Name = 'ssh',
#'   ID = 'ssh-on-devbox',
#'   Address = 'localhost',
#'   Port = 22L
#' ))
#'
#' ## also set a health-check eg for gRPC
#' port <- 35829
#' consular_agent_register_service(list(
#'   Name = 'test-grpc',
#'   ID = 'test-grpc-on-localhost',
#'   Address = 'localhost',
#'   Port = port,
#'   Check = list(
#'     id = 'test-grpc-health-check',
#'     name = 'test-grpc health check',
#'     grpc = paste('127.0.0.1', port, sep = ':'),
#'     interval = '5s',
#'     grpc_use_tls = FALSE,
#'     service_id = 'test-grpc-on-localhost'
#'   )
#' ))
#' }
consular_agent_register_service <- function(payload) {
    invisible(content(PUT(
        file.path(consular$agent_url, consular$version, 'agent', 'service', 'register'),
        body = toJSON(payload, auto_unbox = TRUE))))
}


#' Deregister a new service from the local agent
#' @export
#' @importFrom httr PUT content
#' @references \url{https://www.consul.io/api/agent/service.html#deregister-service}
#' @param id string
#' @examples \dontrun{
#' consular_agent_deregister_service('ssh-on-devbox')
#' }
consular_agent_deregister_service <- function(id) {
    invisible(content(PUT(
        file.path(consular$agent_url, consular$version, 'agent', 'service', 'deregister', id))))
}
