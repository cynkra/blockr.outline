# Regenerate inst/templates/widescreen-default.pptx.
#
# The fallback deck every board renders against when no house template is set.
# It must be NEUTRAL: whoever deploys blockr.outline without a template gets a
# usable widescreen deck, not somebody else's branding. An earlier version of
# this file was a copy of a client master with the logo deleted, which left
# that client's font scheme and its footer text in an open package.
#
# So it is BUILT, from officer's stock Office deck -- plain layouts, no media,
# no text -- and only the geometry is carried over:
#
#   * 13.333 x 7.5in (true 16:9). officer's own deck is 4:3, and every exhibit
#     sizes itself to a widescreen slide (gg_attach_pptx_size(), static_table's
#     fit width), so a 4:3 fallback overflows the right edge of every slide.
#   * 0.4in side margins: title and body placeholders 12.53in wide at x = 0.4,
#     which is where the exhibits place themselves (pptx_left = 0.4). This is
#     also what template_content_width() measures tables against.
#   * Arial as the theme font. It exists on every Windows and macOS install and
#     has a metric-compatible substitute on Linux -- which matters more than
#     usual now that the deck's font scheme is what the exhibits are set in
#     (template_body_font()). The previous default named a face that is
#     bundled for the WEB and installed almost nowhere, so every download
#     opened in whatever PowerPoint chose to substitute.
#
# Run from the package root:  Rscript dev/make-default-template.R

stopifnot(requireNamespace("officer"), requireNamespace("zip"))

EMU <- 914400
SLIDE_W <- 13.333333
SLIDE_H <- 7.5
MARGIN <- 0.4
FONT <- "Arial"
# Slide-title point size. Production house decks sit at 20-24pt; the stock
# Office 44 is a cover-slide size printed on every content slide. The cover
# still gets its own (deck_title_size(), 40pt by default).
TITLE_PT <- 20

emu <- function(inches) as.character(round(inches * EMU))

src <- tempfile(fileext = ".pptx")
print(officer::read_pptx(), target = src)

dir <- tempfile("deck-")
dir.create(dir)
utils::unzip(src, exdir = dir)

read_xml <- function(...) paste(readLines(file.path(dir, ...), warn = FALSE),
                                collapse = "\n")
write_xml <- function(x, ...) writeLines(x, file.path(dir, ...))

# ---- slide size --------------------------------------------------------
pres <- read_xml("ppt", "presentation.xml")
pres <- sub(
  "<p:sldSz[^/]*/>",
  paste0("<p:sldSz cx=\"", emu(SLIDE_W), "\" cy=\"", emu(SLIDE_H),
         "\" type=\"screen16x9\"/>"),
  pres
)
write_xml(pres, "ppt", "presentation.xml")

# ---- widen the layouts with the slide ----------------------------------
# The stock deck is laid out for 10in. Scaling the horizontal axis (and only
# it -- the height is unchanged) keeps every layout's proportions on the wider
# slide; the master's own title / body boxes are then pinned to the margin
# below, because those are the two the render actually measures.
scale_x <- SLIDE_W / 10

scale_attr <- function(xml, pattern, group_fmt) {
  m <- gregexpr(pattern, xml, perl = TRUE)
  vals <- regmatches(xml, m)[[1L]]
  if (!length(vals)) return(xml)
  repl <- vapply(vals, function(v) {
    n <- as.numeric(sub("^\\D*(-?[0-9]+).*$", "\\1", v))
    sprintf(group_fmt, round(n * scale_x))
  }, character(1L), USE.NAMES = FALSE)
  regmatches(xml, m) <- list(repl)
  xml
}

parts <- c(
  file.path("ppt", "slideMasters", "slideMaster1.xml"),
  list.files(file.path(dir, "ppt", "slideLayouts"), pattern = "^slideLayout.*\\.xml$",
             full.names = FALSE) |>
    (\(f) file.path("ppt", "slideLayouts", f))()
)

for (p in parts) {
  xml <- paste(readLines(file.path(dir, p), warn = FALSE), collapse = "\n")
  xml <- scale_attr(xml, "<a:off x=\"-?[0-9]+\"", "<a:off x=\"%d\"")
  xml <- scale_attr(xml, "<a:ext cx=\"[0-9]+\"", "<a:ext cx=\"%d\"")
  xml <- scale_attr(xml, "<a:chOff x=\"-?[0-9]+\"", "<a:chOff x=\"%d\"")
  xml <- scale_attr(xml, "<a:chExt cx=\"[0-9]+\"", "<a:chExt cx=\"%d\"")
  writeLines(xml, file.path(dir, p))
}

# ---- pin the master's own boxes to the margin --------------------------
master <- read_xml("ppt", "slideMasters", "slideMaster1.xml")

pin <- function(xml, ph, left, top, width, height) {
  # Rewrite the xfrm of the <p:sp> carrying this placeholder type.
  pat <- paste0("(<p:ph ", ph, ".*?<a:off x=\")-?[0-9]+(\" y=\")[0-9]+",
                "(\"/><a:ext cx=\")[0-9]+(\" cy=\")[0-9]+")
  sub(
    pat,
    paste0("\\1", emu(left), "\\2", emu(top), "\\3", emu(width), "\\4",
           emu(height)),
    xml
  )
}

body_w <- SLIDE_W - 2 * MARGIN

# The title band, deliberately SMALL. The stock Office title is 44pt, centred
# and vertically centred in a 1.25in box: on a deck of exhibits that is a
# banner the reader does not need, and it costs the picture under it an inch
# of slide. A production deck puts its title top-left, near the top edge, and
# spends the rest on the exhibit -- see the titleStyle rewrite below, which
# does the other half (left-aligned, 20pt, anchored to the top of the box).
master <- pin(master, "type=\"title\"", MARGIN, 0.3, body_w, 0.6)
master <- pin(master, "type=\"body\"", MARGIN, 1.05, body_w, 5.65)
master <- pin(master, "type=\"dt\"", MARGIN, 6.951, 2.333, 0.399)
master <- pin(master, "type=\"ftr\"", 5.0, 6.951, 3.333, 0.399)
master <- pin(master, "type=\"sldNum\"", SLIDE_W - MARGIN - 2.333, 6.951,
              2.333, 0.399)

# The title's own typography, which the placeholder geometry alone does not
# fix: a 44pt centred line still fills the box it was given. `pptx_title_bottom()`
# reads this `sz` (in hundredths of a point) to decide where an exhibit starts,
# so shrinking it here is what buys the picture its height back.
master <- sub(
  "(<p:titleStyle><a:lvl1pPr )algn=\"[a-z]+\"",
  paste0("\\1algn=\"l\""),
  master
)
master <- sub(
  "(<p:titleStyle>.*?<a:defRPr )sz=\"[0-9]+\"",
  paste0("\\1sz=\"", TITLE_PT * 100, "\""),
  master
)
# Anchored to the TOP of its box rather than the middle, so a one-line title
# sits where the box starts instead of floating in it.
master <- sub(
  "(<p:ph type=\"title\"/>.*?<a:bodyPr[^>]*?)anchor=\"[a-z]+\"",
  "\\1anchor=\"t\"",
  master
)

write_xml(master, "ppt", "slideMasters", "slideMaster1.xml")

# ---- theme font --------------------------------------------------------
theme <- read_xml("ppt", "theme", "theme1.xml")
theme <- sub("<a:fontScheme name=\"[^\"]*\"",
             paste0("<a:fontScheme name=\"", FONT, "\""), theme)
for (part in c("majorFont", "minorFont")) {
  theme <- sub(
    paste0("(<a:", part, "><a:latin typeface=\")[^\"]*"),
    paste0("\\1", FONT),
    theme
  )
}
write_xml(theme, "ppt", "theme", "theme1.xml")

# ---- pack --------------------------------------------------------------
# Absolute: zip::zip() resolves `zipfile` against `root`, not against the
# working directory.
out <- file.path(normalizePath(file.path("inst", "templates")),
                 "widescreen-default.pptx")
unlink(out)
zip::zip(out, list.files(dir), root = dir, mode = "cherry-pick")

# ---- report ------------------------------------------------------------
doc <- officer::read_pptx(out)
cat("layouts   : ", paste(officer::layout_summary(doc)$layout, collapse = ", "),
    "\n", sep = "")
cat("slide size: ", paste(unlist(officer::slide_size(doc)[c("width", "height")]),
                          collapse = " x "), "\n", sep = "")
cat("media     : ", length(grep("^ppt/media/", utils::unzip(out, list = TRUE)$Name)),
    "\n", sep = "")
cat("written   : ", out, "\n", sep = "")
