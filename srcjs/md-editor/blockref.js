// Render markdown images whose src is `blockr://<id>` as block-reference chips
// instead of broken <img> tags. This is the WYSIWYG view of blockr.md's
// document-embed syntax (`![](blockr://id)`), which round-trips for free because
// it is a standard commonmark image node — no custom remark needed.

import { $view } from "@milkdown/kit/utils";
import { imageSchema } from "@milkdown/kit/preset/commonmark";

const PREFIX = "blockr://";

export function blockRefView(getTitle) {
  return $view(imageSchema.node, () => (node) => {
    const src = node.attrs.src || "";

    if (src.startsWith(PREFIX)) {
      const id = src.slice(PREFIX.length);
      const dom = document.createElement("span");
      dom.className = "blockr-md-ref-chip";
      dom.dataset.blockId = id;
      dom.title = src;
      const label = getTitle(id) || node.attrs.alt || id;
      dom.textContent = "▦ " + label; // ▦ block glyph
      return { dom };
    }

    // Fall back to a normal image for non-blockr sources.
    const img = document.createElement("img");
    img.src = src;
    if (node.attrs.alt) img.alt = node.attrs.alt;
    if (node.attrs.title) img.title = node.attrs.title;
    return { dom: img };
  });
}

export const isBlockRef = (src) => typeof src === "string" && src.startsWith(PREFIX);
export const blockRefSrc = (id) => PREFIX + id;
