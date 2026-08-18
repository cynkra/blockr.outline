#!/usr/bin/env bash
# Render a pptx to PNGs, one per slide, inside the dev container -- so the
# agent (or anyone) can LOOK at a generated deck instead of reasoning about
# its XML. LibreOffice Impress, installed user-space (no root): the arm64
# .debs from the container's own Ubuntu mirrors, extracted under ~/lo-root.
#
# Usage:  tools/pptx-png.sh file.pptx [outdir]
#         -> outdir/<name>-1.png, -2.png, ... (default: alongside the pptx)
#
# First run bootstraps ~/lo-root (~460MB, ~1 min). Fidelity notes:
#   * LibreOffice's pptx rendering is close but not PowerPoint: minor text
#     metrics differ, and fonts the container lacks (Trebuchet MS, the BMS
#     face) are substituted -- wrapping may differ at the margin.
#   * Good enough to see layout, alignment, fills, collisions -- the things
#     design QA is about. Not a pixel oracle.
set -euo pipefail

LO_ROOT="$HOME/lo-root"
PROG="$LO_ROOT/usr/lib/libreoffice/program"

bootstrap() {
  echo "[pptx-png] bootstrapping user-space LibreOffice into $LO_ROOT" >&2
  local lists=/tmp/apt-lists cache=/tmp/apt-cache debs=/tmp/lo-debs
  mkdir -p "$lists/partial" "$cache/archives/partial" "$debs" "$LO_ROOT"
  local APT=(-o "Dir::State::Lists=$lists" -o "Dir::Cache=$cache")
  apt-get "${APT[@]}" update >/dev/null
  apt-cache "${APT[@]}" depends --recurse --no-recommends --no-suggests \
    --no-conflicts --no-breaks --no-replaces --no-enhances \
    libreoffice-impress poppler-utils 2>/dev/null \
    | grep "^[a-z0-9]" | sort -u > /tmp/lo-closure.txt
  dpkg-query -W -f '${Package}\n' 2>/dev/null | sort -u > /tmp/lo-installed.txt
  comm -23 /tmp/lo-closure.txt /tmp/lo-installed.txt > /tmp/lo-missing.txt
  ( cd "$debs"
    while read -r p; do
      apt-get "${APT[@]}" download "$p" >/dev/null 2>&1 || true
    done < /tmp/lo-missing.txt
    for f in *.deb; do dpkg -x "$f" "$LO_ROOT"; done )
  # Relocate: the Debian rc files hardcode /usr and /etc, and the registry
  # postinst normally symlinks /etc/libreoffice/registry -> share/.registry.
  sed -i \
    -e "s|file:///etc/libreoffice/registry|file://$LO_ROOT/usr/lib/libreoffice/share/.registry|g" \
    -e "s|file:///usr/lib/libreoffice|file://$LO_ROOT/usr/lib/libreoffice|g" \
    -e "s|file:///usr/share/java|file://$LO_ROOT/usr/share/java|g" \
    "$PROG/fundamentalrc"
  sed -i "s|file:///etc/libreoffice/sofficerc|file://$LO_ROOT/etc/libreoffice/sofficerc|g" \
    "$PROG/sofficerc"
}

[ -x "$PROG/soffice.bin" ] || bootstrap

in="$(readlink -f "$1")"
outdir="${2:-$(dirname "$in")}"
mkdir -p "$outdir"
base="$(basename "$in" .pptx)"

# program/ FIRST: two copies of the uno libs are extracted, and cppuhelper
# resolves its ini next to whichever copy loaded -- the aarch64-linux-gnu one
# has no unorc beside it.
export LD_LIBRARY_PATH="$PROG:$LO_ROOT/usr/lib/aarch64-linux-gnu:$LO_ROOT/usr/lib"
export SAL_USE_VCLPLUGIN=svp

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Exit 81 = "profile initialised, restart me": expected on every run here,
# because the profile is fresh each time. One retry is the documented cure.
convert() {
  "$PROG/soffice.bin" --headless --norestore \
    -env:UserInstallation="file://$tmp/profile" \
    --convert-to pdf --outdir "$tmp" "$in" >/dev/null
}
convert || { [ $? -eq 81 ] && convert; }

"$LO_ROOT/usr/bin/pdftoppm" -png -r 110 "$tmp/$base.pdf" "$outdir/$base"

ls "$outdir/$base"-*.png
