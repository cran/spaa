deg2dec <-
function(h,m,s){
    if(h < 0){
        m = -1.0 * m
        s = -1.0 * s
    }
    res = h + m/60 + s/3600
    return(res)
}
