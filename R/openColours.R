openColours <- function(scheme = "default", n = 100) {

#heat
heat <- colorRampPalette(brewer.pal(9, "YlOrRd"), interpolate = "spline")

jet <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                      "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

default.col <- colorRampPalette(brewer.pal(11, "Spectral"), interpolate = "spline")

brewer1 <- suppressWarnings(brewer.pal(n, "Set1"))

increment <- colorRampPalette(c("#B0FFF1", "#9CFFC7", "#87FF8E", "#A0FF73",
              "#B4FF69", "#CCFF60", "#E7FF56", "#FFF84D", "#FFCB46", "#FF9C40",
              "#FF6939", "#FF3333", "#CC1B62", "#990A7C", "#520066"))


h = c(0, 360) + 15
l = 65
c = 100

if ((diff(h) %% 360) < 1) {
    h[2] <- h[2] - 360 / n
}

hue <- grDevices::hcl(
                      h = seq(h[1], h[2], length = n),
                      c = c,
                      l = l)

if (length(scheme) == 1) {
    if (scheme == "increment") cols <- increment(n)
    if (scheme == "default") cols <- rev(default.col(n))
    if (scheme == "brewer1") cols <- brewer1
    if (scheme == "heat") cols <- heat(n)
    if (scheme == "jet") cols <- jet(n)
    if (scheme == "hue") cols <- hue
}

schemes <- c("increment", "default", "brewer1", "heat", "jet", "hue")

if (!scheme %in% schemes) { #assume user has given own colours
    if (length(scheme) > 1) {  ## interpolate
        user.cols  <- colorRampPalette(scheme)
        cols =  user.cols(n)
    } else {

         cols <- rep(scheme, n)
    }
}




cols
}

