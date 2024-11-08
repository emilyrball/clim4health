table.extract <- function(data, years.table, region.names, smonth){
    table.format <- as.data.frame.table(data)
    names(table.format) <- c(names(dim(data)), 'values') # variable.name[var]
    levels(table.format$region) <- region.names
    levels(table.format$syear) <- years.table
    levels(table.format$time) <- 1:6
    levels(table.format$ensemble) <- 1:dim(data)['ensemble']
    table.format$fmonth <- ymd(as.Date(paste0(table.format$syear, '-', ifelse(smonth<10,'0',''), smonth, '-01'))) %m+% months(as.integer(table.format$time) - 1)
    table.format$values <- round(table.format$values, 2)
    order.format <- c('ensemble', 'syear', 'region', 'time', 'values', 'fmonth')
    table.order <- table.format[,match(order.format, names(table.format))]
    table.order <- table.order[with(table.order, order(time,region,syear,ensemble)), ]
    return(table.order)
}
