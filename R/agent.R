#' List consul agent members
#' @return \code{data.table} with a member in each row
#' @export
#' @importFrom httr GET content
#' @importFrom data.table rbindlist as.data.table
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

## curl http://localhost:8500/v1/agent/services
