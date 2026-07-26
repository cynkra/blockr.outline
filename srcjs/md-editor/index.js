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

// Membership, not identity: an element carries its editor for as long as it is
// in the document, so this needs no element id and leaks nothing once the R
// side replaces the node.
const initialized = new WeakSet();

// Live editors, so a host page can force every pending value to the server
// before it acts on one (see window.blockrMdEditor.flush below). Weak, so a
// node the R side replaces is collected rather than pinned here.
const editors = new Set();

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
    // Last value actually sent, so a commit with nothing new is free and
    // focusout does not re-send on every tab-out.
    this._sent = null;
    this.editor = null;

    this._buildDom();
    this._initEditor();
    this.el.addEventListener("focusout", (ev) => this._onFocusOut(ev));
    editors.add(this);
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
    this._sent = this.markdown;
    setShinyInput(this.inputId, this.markdown);
  }

  // Typing updates the in-memory value and the mirrored source only. No
  // server round trip until the edit is committed -- see _commit().
  _onWysiwygUpdate(md) {
    if (this._applyingExternal) return;
    this.markdown = md;
    if (this.textarea.value !== md) this.textarea.value = md;
  }

  _onRawEdit() {
    this.markdown = this.textarea.value;
    this._applyToWysiwyg(this.markdown);
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

  // Send the current value, once. Called when focus leaves the editor and
  // from flush() just before the host acts on the value.
  //
  // This used to run on a 300ms debounce while typing, which meant a Shiny
  // input event -- and a full server flush -- every 300ms for the length of a
  // sentence. Nothing on the server reads the value until the edit is
  // committed, so every one of those round trips was discarded work, and the
  // flushes are what made typing feel heavy. Keeping the markdown in memory
  // and sending it once costs nothing and loses nothing.
  _commit() {
    if (this._sent === this.markdown) return;
    this._sent = this.markdown;
    setShinyInput(this.inputId, this.markdown);
  }

  // focusout fires before the click that the host turns into a save, so the
  // value is already on its way when the save action arrives. It also bubbles,
  // unlike blur, so one listener on the root covers the WYSIWYG surface and
  // the raw textarea both.
  _onFocusOut(ev) {
    if (this.el.contains(ev.relatedTarget)) return;
    this._commit();
  }

  destroy() {
    editors.delete(this);
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

// Host hook. focusout already commits before the click that a host turns into
// a save, but a host that acts on the value without moving focus first can
// call this to be certain the server has it.
if (typeof window !== "undefined") {
  window.blockrMdEditor = window.blockrMdEditor || {};
  window.blockrMdEditor.flush = function flush() {
    editors.forEach((ed) => {
      if (ed.el.isConnected) ed._commit();
      else ed.destroy();
    });
  };
}

if (document.readyState === "loading") {
  document.addEventListener("DOMContentLoaded", register);
} else {
  register();
}
