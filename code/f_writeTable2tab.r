writeTable2tab  <- function(tablename, filename,
                            append = FALSE,
                            quote = FALSE,
                            sep = "\t",
                            eol = "\n",
                            na = "",
                            dec = ".",
                            row.names = FALSE,
                            col.names = TRUE,
                            fileEncoding = "UTF-8"){
    write.table(tablename,
                file =filename,
                append = append,
                quote = quote,
                sep = sep,
                eol = eol,
                na = na,
                dec = dec,
                row.names = row.names,
                col.names = col.names,
                fileEncoding = fileEncoding)

}