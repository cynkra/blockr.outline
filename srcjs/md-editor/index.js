// blockr.md WYSIWYG document editor (Milkdown).
//
// Replaces the shinyAce raw-markdown editor with a Milkdown WYSIWYG editor that
// keeps markdown canonical (the document feeds the pandoc/rmarkdown render
// pipeline). One controller drives two views — the WYSIWYG editor and a
// collapsible raw-markdown <textarea> — synced client-side and guarded against
// echo loops. The committed markdown is written to the SAME Shiny input the ace
// editor used (`ace`), so all downstream server logic (validation, download)
// is unchanged. Block embeds (`![](blockr://id)`) render as chips.

import { Editor, rootCtx, defaultValueCtx, editorViewCtx } from "@milkdown/kit/core";
import { commonmark } from "@milkdown/kit/preset/commonmark";
import { gfm } from "@milkdown/kit/preset/gfm";
import { listener, listenerCtx } from "@milkdown/kit/plugin/listener";
import { replaceAll } from "@milkdown/kit/utils";
import { blockRefView, blockRefSrc } from "./blockref.js";

const COMMIT_DELAY = 300;
const instances = new Map();

function setShinyInput(id, value) {
  if (window.Shiny && Shiny.setInputValue) {
    Shiny.setInputValue(id, value, { priority: "event" });
  }
}

class MdEditor {
  constructor(el) {
    this.el = el;
    this.inputId = el.dataset.inputId;
    this.markdown = el.dataset.initial || "";
    this.blocks = []; // [{id, title}]
    this.titles = {}; // id -> title
    this._applyingExternal = false;
    this._commitTimer = null;
    this.editor = null;

    this._buildDom();
    this._initEditor();
  }

  _buildDom() {
    this.el.classList.add("blockr-md-editor");

    this.toolbar = document.createElement("div");
    this.toolbar.className = "blockr-md-toolbar";
    this.refBtn = document.createElement("button");
    this.refBtn.type = "button";
    this.refBtn.className = "btn btn-sm btn-outline-secondary";
    this.refBtn.textContent = "+ Block";
    this.refMenu = document.createElement("div");
    this.refMenu.className = "blockr-md-ref-menu";
    this.refMenu.hidden = true;
    this.refBtn.addEventListener("click", (e) => {
      e.preventDefault();
      this.refMenu.hidden = !this.refMenu.hidden;
    });
    this.toolbar.appendChild(this.refBtn);
    this.toolbar.appendChild(this.refMenu);

    this.editorHost = document.createElement("div");
    this.editorHost.className = "blockr-md-editor-host";

    this.details = document.createElement("details");
    this.details.className = "blockr-md-source";
    const summary = document.createElement("summary");
    summary.textContent = "Markdown source";
    this.textarea = document.createElement("textarea");
    this.textarea.className = "form-control blockr-md-source-ta";
    this.textarea.rows = 8;
    this.textarea.value = this.markdown;
    this.textarea.addEventListener("input", () => this._onRawEdit());
    this.details.appendChild(summary);
    this.details.appendChild(this.textarea);

    this.el.appendChild(this.toolbar);
    this.el.appendChild(this.editorHost);
    this.el.appendChild(this.details);
  }

  async _initEditor() {
    const self = this;
    this.editor = await Editor.make()
      .config((ctx) => {
        ctx.set(rootCtx, this.editorHost);
        ctx.set(defaultValueCtx, this.markdown);
        ctx.get(listenerCtx).markdownUpdated((_ctx, md) => self._onWysiwygUpdate(md));
      })
      .use(commonmark)
      .use(gfm)
      .use(listener)
      .use(blockRefView((id) => self.titles[id]))
      .create();

    // Populate input$ace immediately so server-side validation/download have
    // the document before the first edit.
    setShinyInput(this.inputId, this.markdown);
  }

  _onWysiwygUpdate(md) {
    if (this._applyingExternal) return;
    this.markdown = md;
    if (this.textarea.value !== md) this.textarea.value = md;
    this._commit();
  }

  _onRawEdit() {
    this.markdown = this.textarea.value;
    this._applyToWysiwyg(this.markdown);
    this._commit();
  }

  _applyToWysiwyg(md) {
    if (!this.editor) return;
    this._applyingExternal = true;
    try {
      this.editor.action(replaceAll(md));
    } finally {
      Promise.resolve().then(() => {
        this._applyingExternal = false;
      });
    }
  }

  setMarkdown(md) {
    if (md === this.markdown) return;
    this.markdown = md;
    if (this.textarea.value !== md) this.textarea.value = md;
    this._applyToWysiwyg(md);
  }

  _commit() {
    clearTimeout(this._commitTimer);
    this._commitTimer = setTimeout(() => setShinyInput(this.inputId, this.markdown), COMMIT_DELAY);
  }

  setBlocks(blocks) {
    this.blocks = blocks || [];
    this.titles = {};
    for (const b of this.blocks) this.titles[b.id] = b.title;
    this._renderRefMenu();
  }

  _renderRefMenu() {
    this.refMenu.innerHTML = "";
    if (!this.blocks.length) {
      const empty = document.createElement("div");
      empty.className = "blockr-md-ref-empty";
      empty.textContent = "No blocks on the board";
      this.refMenu.appendChild(empty);
      return;
    }
    for (const b of this.blocks) {
      const item = document.createElement("button");
      item.type = "button";
      item.className = "blockr-md-ref-item";
      item.textContent = b.title || b.id;
      item.addEventListener("click", (e) => {
        e.preventDefault();
        this._insertRef(b);
        this.refMenu.hidden = true;
      });
      this.refMenu.appendChild(item);
    }
  }

  _insertRef(b) {
    if (!this.editor) return;
    this.editor.action((ctx) => {
      const view = ctx.get(editorViewCtx);
      const type = view.state.schema.nodes.image;
      if (!type) return;
      const node = type.create({ src: blockRefSrc(b.id), alt: b.title || b.id, title: "" });
      view.dispatch(view.state.tr.replaceSelectionWith(node).scrollIntoView());
      view.focus();
    });
  }
}

function initEl(el) {
  if (!el || !el.id || instances.has(el.id)) return;
  instances.set(el.id, new MdEditor(el));
}

function scan(root) {
  (root || document).querySelectorAll(".blockr-md-editor[data-input-id]").forEach(initEl);
}

function register() {
  scan(document);

  const mo = new MutationObserver((muts) => {
    for (const m of muts) {
      m.addedNodes.forEach((n) => {
        if (n.nodeType !== 1) return;
        if (n.matches && n.matches(".blockr-md-editor[data-input-id]")) initEl(n);
        if (n.querySelectorAll) scan(n);
      });
    }
  });
  mo.observe(document.body, { childList: true, subtree: true });

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("md-set", (msg) => {
      const inst = instances.get(msg.id);
      if (inst) inst.setMarkdown(msg.markdown || "");
    });
    Shiny.addCustomMessageHandler("md-blocks", (msg) => {
      const inst = instances.get(msg.id);
      if (inst) inst.setBlocks(msg.blocks || []);
    });
  }
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", register);
} else {
  register();
}
