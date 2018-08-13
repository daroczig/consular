#' List members the local agent sees in the cluster gossip pool
#' @return \code{data.table} with a member in each row
#' @export
#' @importFrom httr GET content
#' @importFrom data.table rbindlist as.data.table
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
