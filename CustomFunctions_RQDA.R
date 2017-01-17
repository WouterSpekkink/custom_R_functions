getCodingTableWithMemos <- function () {
    o.table <- getCodingTable()

    c.id <- RQDAQuery("SELECT rowid FROM coding")
    c.mem <- RQDAQuery("SELECT memo FROM coding")
    c.table <- cbind(c.id, c.mem)
    colnames(c.table) <- c("id", "memo")
    memos <- rep("", nrow(o.table))

    for (i in 1:nrow(o.table)) {
        for (j in 1:nrow(c.table)) {
            if (o.table[i, "rowid"] == c.table[j, "id"]) {
                memos[i] <- c.table[j, "memo"]
            }
        }
    }
    o.table <- cbind(o.table, memos)
    return(o.table)
}
