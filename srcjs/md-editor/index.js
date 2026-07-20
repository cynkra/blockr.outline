// Markdown editor for outline section descriptions (Milkdown).
//
// One controller drives two views of the same document -- the WYSIWYG editor
// and a collapsible raw-markdown <textarea> -- synced client-side and guarded
// against echo loops. Markdown stays canonical: the committed text goes to the
// Shiny input named by `data-input-id`, and the server renders it with
// commonmark.
//
// The R side emits a fresh <div class="blockr-md-editor" data-input-id=...
// data-initial=...> on every render, and a MutationObserver picks it up. There
// is deliberately no server-to-client update channel: a new description means a
// new element, not a message to an existing editor.

import { Editor, rootCtx, defaultValueCtx } from "@milkdown/kit/core";
import { commonmark } from "@milkdown/kit/preset/commonmark";
import { gfm } from "@milkdown/kit/preset/gfm";
import { listener, listenerCtx } from "@milkdown/kit/plugin/listener";
import { replaceAll } from "@milkdown/kit/utils";

const COMMIT_DELAY = 300;

// Membership, not identity: an element carries its editor for as long as it is
// in the document, so this needs no element id and leaks nothing once the R
// side replaces the node.
const initialized = new WeakSet();

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
    this._applyingExternal = false;
    this._commitTimer = null;
    this.editor = null;

    this._buildDom();
    this._initEditor();
  }

  _buildDom() {
    this.el.classList.add("blockr-md-editor");

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
      .create();

    // Populate the input immediately, so the server holds the description
    // before the first edit.
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

  _commit() {
    clearTimeout(this._commitTimer);
    this._commitTimer = setTimeout(() => setShinyInput(this.inputId, this.markdown), COMMIT_DELAY);
  }
}

function initEl(el) {
  if (!el || initialized.has(el)) return;
  initialized.add(el);
  new MdEditor(el);
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
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", register);
} else {
  register();
}
