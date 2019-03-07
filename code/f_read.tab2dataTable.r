# this functions reads a tab-delimited file into a data.table object

read.tab2dataTable <- function(filename, 
                        header = TRUE, 
                        sep = "\t", 
                        quote = "", 
                        na.strings = "", 
                        stringsAsFactor = FALSE, 
                        encoding = "UTF-8", 
                        comment.char =""){

    as.data.table(read.table(filename, 
                                header = header, 
                                sep = sep,
                                quote = quote,
                                na.strings = na.strings, 
                                stringsAsFactors = stringsAsFactor,
                                encoding = encoding,
                                comment.char = comment.char))
                            
}