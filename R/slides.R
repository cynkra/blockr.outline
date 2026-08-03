#' Slide builder extension
#'
#' A dock extension that turns a board into a deck: pick the blocks whose
#' output should become slides, order them, download a PowerPoint file or an
#' HTML deck. One block, one slide.
#'
#' The lightweight counterpart to [new_outline_extension()]. The outline
#' builds a whole Quarto document -- prose, chapters, the generated code, an
#' inverted reading order -- and can render it as a deck among other things.
#' This does only the deck, and the difference shows in what it asks of the
#' user: a picker and a list, no writing surface and nothing to read.
#'
#' Two consequences worth knowing about, both of which follow from having no
#' document:
#'
#' * **Slides are freely orderable.** The deck emits every block's code up
#'   front, hidden, and each slide carries only its exhibit -- so slide order
#'   and evaluation order are independent. A deck may open on its conclusion.
#' * **Nothing is evaluated until you download.** The picker and the list
#'   read block names off the board; block expressions are read once, when
#'   the download is clicked. A slide builder sitting in a closed dock panel
#'   costs nothing.
#'
#' Blocks upstream of a picked block are still evaluated -- picking a table
#' means running what feeds it -- but they are not shown and take no slide.
#' Branches nothing picked depends on are never evaluated at all.
#'
#' @section Tables that do not fit:
#' A table too tall for its slide is carried onto the next one, and blockr.viz
#' shrinks the type before it does that: one slide at 10pt beats two at 13pt.
#' How far it may shrink is the board's `exhibit_min_font_size` option
#' (`blockr.viz::new_exhibit_font_option()`, "Smallest table font" in the
#' board settings), so a deck that must not split its tables asks for it
#' there rather than here. It is a board option and not a field in this
#' panel because the same number governs the PowerPoint download on a table
#' or summarize block: a slide and the block it came from have to be the same
#' table.
#'
#' What still does not fit at that size is split, and the download says which
#' tables and at what size they would have stayed whole.
#'
#' @param slides Character vector of block ids, in slide order. Both the
#'   picking and the ordering: a block is a slide iff it is named here.
#' @param title Deck title. Names the file, titles the html deck and appears
#'   as the running footer on every html slide.
#' @param format Download format: `"pptx"` (PowerPoint) or `"html"`
#'   (a self-contained HTML deck). Both are written in this process, so
#'   neither needs the quarto CLI on the machine. `"revealjs"`, the format
#'   string of the quarto render the HTML deck replaced, still restores as
#'   `"html"`.
#' @param template LEGACY, ignored. The reference deck is a property of the
#'   deployment, not of a board: it comes from
#'   `getOption("blockr.outline.template")` (an app sets it once, typically
#'   from `blockr.theme::theme_template()`), falling back to the bundled
#'   widescreen deck. Accepted only so boards saved while the deck panel
#'   still offered a template field restore without error.
#' @param ... Forwarded to [blockr.dock::new_dock_extension()]
#'
#' @return A dock extension object, to be passed in a board's `extensions`
#'   list (see [blockr.dock::new_dock_board()]).
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dock)
#'
#'   board <- new_dock_board(
#'     blocks = c(
#'       data = new_dataset_block("iris"),
#'       audit = new_head_block(n = 3L)
#'     ),
#'     links = links(from = "data", to = "audit"),
#'     extensions = list(
#'       new_slides_extension(slides = "audit", title = "Iris pilot")
#'     )
#'   )
#'
#'   serve(board)
#' }
#'
#' @export
new_slides_extension <- function(slides = character(),
                                 title = "Deck",
                                 format = "pptx",
                                 template = "",
                                 ...) {

  blockr.dock::new_dock_extension(
    slides_ext_srv(slides, title, format),
    slides_ext_ui,
    name = "Slides",
    description = paste(
      "Deck builder: pick the blocks whose output becomes a slide, order",
      "them, download as PowerPoint or HTML."
    ),
    class = "slides_extension",
    ...
  )
}

# The formats a deck offers. Deliberately two, and both written in-process:
# officer for the PowerPoint, this package's own writer for the HTML (see
# R/deck-html.R for why that is not quarto + revealjs). Neither needs a CLI
# on the machine.
deck_formats <- function() {
  c("PowerPoint" = "pptx", "HTML" = "html")
}

# LEGACY: the HTML deck was a quarto revealjs render before it was written
# here, and a board saved in between carries quarto's word for it.
deck_format <- function(fmt) {
  fmt <- coal(fmt, "pptx")
  if (identical(fmt, "revealjs")) "html" else fmt
}

slides_ext_ui <- function(id, board, ...) {

  ns <- NS(id)

  div(
    class = "blockr-sld-panel",
    id = ns("sld_root"),
    slides_dep(),
    div(
      class = "blockr-sld-toolbar",
      textInput(
        ns("sld_title"),
        label = NULL,
        placeholder = "Deck title",
        width = "100%"
      ),
      div(
        class = "blockr-sld-toolbar-right",
        # The outline's split button, same classes: a neutral format picker
        # fused to the green action, labelled for the file you walk away
        # with rather than for the render that produces it.
        div(
          class = "blockr-sld-rendergroup",
          selectInput(
            ns("sld_format"),
            label = NULL,
            choices = deck_formats(),
            selected = "pptx",
            selectize = FALSE,
            width = "116px"
          ),
          # Two-stage, exactly like the outline's: on a deferred board the
          # picked blocks may not be constructed yet, so the click goes to
          # the server first (demand the closure, wait for its code) and
          # the hidden link below is clicked from JS once it is ready.
          actionButton(
            ns("sld_go"),
            "Download",
            icon = icon("download"),
            class = "blockr-sld-renderbtn"
          ),
          downloadLink(ns("sld_dl"), label = NULL, style = "display: none;")
        )
        # No gear. The deck's one settable property used to be the reference
        # template, and that is not the user's to set here: it styles every
        # download of a deployment, so it comes from the app (see
        # effective_template()). With nothing left to configure, a gear would
        # open on an empty band.
      )
    ),
    # The picker: the outline's search-and-add box, which is itself the
    # block browser's. Same classes, hence the same magnifier, focus ring,
    # rows and icon tiles as every other "find a block" control in the app,
    # from the stylesheet blockr.dock already puts on the page.
    #
    # NOT .blockr-block-browser: that class is the block browser's Shiny
    # input binding and its search JS, which would adopt this control as a
    # browser instance and filter it by data attributes these cards do not
    # carry. The card classes are inert styling; the --bb-* tokens they
    # read are mapped in blockr-slides.css.
    div(
      class = "blockr-sld-search",
      tags$input(
        type = "search",
        class = "blockr-block-browser-search blockr-sld-searchinput",
        placeholder = "Search or add a block\u2026",
        `aria-label` = "Search blocks",
        autocomplete = "off",
        spellcheck = "false"
      ),
      span(class = "blockr-sld-searchcount"),
      div(class = "blockr-sld-searchmenu")
    ),
    uiOutput(ns("sld_list")),
    slides_js(ns)
  )
}

slides_dep <- function() {
  htmlDependency(
    "blockr-slides",
    pkg_version(),
    src = pkg_file("assets", "css"),
    stylesheet = "blockr-slides.css"
  )
}

# Delegated client logic, the same shape as outline_js and a small fraction
# of it: the list has no legality to enforce (every order is a valid deck),
# so a drag is just "put this one there".
slides_js <- function(ns) {

  consts <- sprintf(
    "var ACT = '%s', MOVE = '%s', DL = '%s', ADD = '%s', ROOT = '%s';",
    ns("sld_act"),
    ns("sld_move"),
    ns("sld_dl"),
    ns("sld_add"),
    ns("sld_root")
  )

  tags$script(HTML(paste0(
    "$(function() {",
    consts,
    "
      // Delegated from document so the list can be re-rendered freely --
      // rows are markup, never Shiny inputs, so there is nothing to rebind.
      document.addEventListener('click', function(e) {

        var btn = e.target.closest ? e.target.closest('.blockr-sld-act') : null;
        if (!btn) return;
        var row = btn.closest('.blockr-sld-row');
        if (!row) return;
        Shiny.setInputValue(
          ACT,
          {id: row.dataset.blk, act: btn.dataset.act},
          {priority: 'event'}
        );
      });

      var dragged = null;

      document.addEventListener('dragstart', function(e) {
        var row = e.target.closest ? e.target.closest('.blockr-sld-row') : null;
        if (!row) return;
        dragged = row.dataset.blk;
        row.classList.add('is-dragging');
        if (e.dataTransfer) e.dataTransfer.effectAllowed = 'move';
      });

      document.addEventListener('dragend', function() {
        dragged = null;
        document.querySelectorAll('.blockr-sld-row').forEach(function(r) {
          r.classList.remove('is-dragging', 'is-over');
        });
      });

      document.addEventListener('dragover', function(e) {
        if (dragged === null) return;
        var row = e.target.closest ? e.target.closest('.blockr-sld-row') : null;
        if (!row) return;
        e.preventDefault();
        document.querySelectorAll('.blockr-sld-row').forEach(function(r) {
          r.classList.remove('is-over');
        });
        row.classList.add('is-over');
      });

      document.addEventListener('drop', function(e) {
        if (dragged === null) return;
        var row = e.target.closest ? e.target.closest('.blockr-sld-row') : null;
        if (!row) return;
        e.preventDefault();
        // Drop on the lower half of a row means after it, which is the only
        // way to reach the last position.
        var box = row.getBoundingClientRect();
        var after = (e.clientY - box.top) > box.height / 2;
        Shiny.setInputValue(
          MOVE,
          {id: dragged, target: row.dataset.blk, after: after},
          {priority: 'event'}
        );
        dragged = null;
      });

      // ---- search: the picker, as the block browser's box ------------
      // The catalogue (every board block, the picked ones first) is pushed
      // by the server; the menu is rendered HERE so the input never
      // re-renders and never loses focus mid-query. A picked block is a
      // \"go to\" -- its row is in the list below -- and an unpicked one an
      // \"add\", which writes the same input the old select fed. The menu
      // stays open after an add, so building a deck is type, Enter, type,
      // Enter.
      //
      // The rows ARE the block browser's rows: same classes, same
      // stylesheet, so picking a block looks the same everywhere in the
      // app and this file owns no row styling.
      // The catalogue describes the BOARD (names, icons, descriptions) and
      // moves only when the board does; which blocks are in the deck is a
      // separate, tiny message, because that flips on every add and the
      // catalogue costs tens of KB to resend. Icon markup is shipped once
      // per block class and referenced by key for the same reason.
      var catalog = [];
      var icons = {};
      var picked = [];
      var pickedAt = {};
      var hot = 0;

      function isPicked(b) {
        return pickedAt[b.id] !== undefined;
      }

      function panel() {
        return document.getElementById(ROOT);
      }
      function searchRoot() {
        var p = panel();
        return p && p.querySelector('.blockr-sld-search');
      }
      function searchInput() {
        var p = panel();
        return p && p.querySelector('.blockr-sld-searchinput');
      }
      function searchQuery() {
        var inp = searchInput();
        return (inp && inp.value ? inp.value : '').trim().toLowerCase();
      }
      function esc(s) {
        return String(s == null ? '' : s).replace(/[&<>]/g, function(c) {
          return {'&': '&amp;', '<': '&lt;', '>': '&gt;'}[c];
        });
      }
      // Mark the matched substring, on escaped text.
      function mark(text, q) {
        var t = String(text == null ? '' : text);
        if (!q) return esc(t);
        var i = t.toLowerCase().indexOf(q);
        if (i < 0) return esc(t);
        return esc(t.slice(0, i)) + '<mark>' + esc(t.slice(i, i + q.length)) +
          '</mark>' + esc(t.slice(i + q.length));
      }
      // Match on everything the entry shows plus the id, which is what
      // disambiguates two blocks carrying the same name.
      function searchHits(q) {
        return catalog.filter(function(b) {
          if (!q) return true;
          return (b.name + ' ' + b.desc + ' ' + b.id)
            .toLowerCase().indexOf(q) >= 0;
        });
      }
      function cardHtml(b, q, idx) {
        return '<div class=\"blockr-block-browser-card\" data-blk=\"' +
          esc(b.id) + '\" data-idx=\"' + idx + '\">' +
          '<div class=\"blockr-block-browser-card-header\">' +
            '<span class=\"blockr-block-browser-card-icon\">' +
              (icons[b.icon_key] || '') + '</span>' +
            '<div class=\"blockr-block-browser-card-body\">' +
              '<div class=\"blockr-block-browser-card-titles\">' +
                '<span class=\"blockr-block-browser-card-name\">' +
                  mark(b.name, q) + '</span>' +
                (b.kind ?
                  '<span class=\"blockr-block-browser-card-package\">' +
                  esc(b.kind) + '</span>' : '') +
                '<span class=\"blockr-sld-optact\">' +
                  (isPicked(b) ? 'In the deck' : 'Add') + '</span>' +
              '</div>' +
              '<p class=\"blockr-sld-optdesc\">' +
                (b.desc ? mark(b.desc, q) : esc(b.id)) + '</p>' +
            '</div>' +
          '</div>' +
        '</div>';
      }
      // The menu's two sections, and the flat order the keyboard index and
      // the click index both count in. ONE definition: the renderer and the
      // chooser used to sort independently, which was safe only while the
      // server happened to send the catalogue unpicked-first.
      function orderedHits(q) {
        var hits = searchHits(q);
        var out = hits.filter(function(b) { return !isPicked(b); });
        // Deck order, matching the rows below -- the catalogue no longer
        // carries it, since it would change on every add.
        var inn = hits.filter(isPicked).sort(function(a, b) {
          return pickedAt[a.id] - pickedAt[b.id];
        });
        return {out: out, inn: inn, all: out.concat(inn)};
      }
      function sectionHtml(title, items, q, start) {
        if (!items.length) return '';
        var html = '<div class=\"blockr-block-browser-category\"><h3>' +
          title + '</h3><div class=\"blockr-block-browser-cards\">';
        items.forEach(function(b, k) { html += cardHtml(b, q, start + k); });
        return html + '</div></div>';
      }
      // Keyboard selection, the browser's own marker class. Scrolls only
      // when the arrows moved it, so a re-render never jumps the panel.
      function selectHot(scroll) {
        var root = searchRoot();
        if (!root) return;
        root.querySelectorAll('.blockr-block-browser-card').forEach(
          function(c, i) {
            var on = i === hot;
            c.classList.toggle('card-selected', on);
            if (on && scroll) c.scrollIntoView({block: 'nearest'});
          });
      }
      function renderMenu() {
        var root = searchRoot();
        var menu = root && root.querySelector('.blockr-sld-searchmenu');
        if (!menu) return;

        var q = searchQuery();
        var split = orderedHits(q);
        var out = split.out, inn = split.inn, all = split.all;
        if (hot >= all.length) hot = Math.max(0, all.length - 1);

        var count = root.querySelector('.blockr-sld-searchcount');
        if (count) {
          var pool = catalog.filter(function(b) { return !isPicked(b); }).length;
          count.textContent = pool ? pool + ' not in the deck' : '';
        }
        root.classList.toggle('has-value', !!q);

        menu.classList.toggle('is-empty', !all.length);
        menu.innerHTML =
          '<div class=\"blockr-block-browser-categories\">' +
            sectionHtml('Add a slide', out, q, 0) +
            sectionHtml('Already in the deck', inn, q, out.length) +
          '</div>' +
          '<div class=\"blockr-block-browser-empty\">' +
            'No blocks match your search.</div>';
        selectHot(false);
      }
      function searchOpen() {
        var root = searchRoot();
        if (!root) return;
        root.classList.add('open');
        renderMenu();
      }
      function searchClose() {
        var root = searchRoot();
        if (root) root.classList.remove('open');
      }
      // Reveal a picked block's row: scroll it into view and flash it, so
      // a hit on a long deck lands somewhere visible.
      function gotoRow(id) {
        var p = panel();
        var row = p && p.querySelector(
          '.blockr-sld-row[data-blk=\"' + id + '\"]'
        );
        if (!row) return;
        row.scrollIntoView({block: 'center', behavior: 'smooth'});
        row.classList.remove('blockr-sld-flash');
        void row.offsetWidth;
        row.classList.add('blockr-sld-flash');
      }
      function searchChoose(idx) {
        var b = orderedHits(searchQuery()).all[idx];
        if (!b) return;
        if (isPicked(b)) {
          searchClose();
          var inp = searchInput();
          if (inp) inp.blur();
          gotoRow(b.id);
          return;
        }
        // Add: the server appends the slide and pushes the new picked set,
        // which re-renders the menu with the entry moved to the second
        // group.
        Shiny.setInputValue(ADD, b.id, {priority: 'event'});
      }

      Shiny.addCustomMessageHandler('blockr-slides-catalog', function(msg) {
        catalog = msg.items || [];
        icons = msg.icons || {};
        renderMenu();
      });

      // Which blocks are in the deck, and in what order. Its own message:
      // this flips on every add, and re-sending the catalogue to say so
      // meant resending every name, description and icon with it.
      Shiny.addCustomMessageHandler('blockr-slides-picked', function(msg) {
        picked = msg.ids || [];
        if (typeof picked === 'string') picked = [picked];
        pickedAt = {};
        picked.forEach(function(id, i) { pickedAt[id] = i; });
        renderMenu();
      });

      document.addEventListener('input', function(ev) {
        if (ev.target && ev.target.classList &&
            ev.target.classList.contains('blockr-sld-searchinput')) {
          // Top hit selected on every keystroke, so Enter always does
          // something (the block browser's behaviour).
          hot = 0;
          searchOpen();
        }
      });
      document.addEventListener('focusin', function(ev) {
        if (ev.target && ev.target.classList &&
            ev.target.classList.contains('blockr-sld-searchinput')) {
          searchOpen();
        }
      });
      // mousedown, not click: click fires after blur, and blurring the
      // input would have to close the menu first.
      document.addEventListener('mousedown', function(ev) {
        var card = ev.target.closest &&
          ev.target.closest('.blockr-sld-searchmenu .blockr-block-browser-card');
        if (!card) return;
        ev.preventDefault();
        searchChoose(parseInt(card.dataset.idx, 10));
      });
      // CAPTURE phase on purpose: choosing an entry re-renders the menu and
      // detaches the clicked node, so a bubble-phase listener would see a
      // target that is no longer inside the box and close it on every add.
      document.addEventListener('mousedown', function(ev) {
        var root = searchRoot();
        if (root && !root.contains(ev.target)) searchClose();
      }, true);
      document.addEventListener('keydown', function(ev) {
        if (!(ev.target && ev.target.classList &&
              ev.target.classList.contains('blockr-sld-searchinput'))) {
          return;
        }
        var n = searchHits(searchQuery()).length;
        if (ev.key === 'ArrowDown') {
          // Wrapping, like the block browser.
          hot = n ? (hot + 1) % n : 0;
          selectHot(true); ev.preventDefault();
        } else if (ev.key === 'ArrowUp') {
          hot = n ? (hot + n - 1) % n : 0;
          selectHot(true); ev.preventDefault();
        } else if (ev.key === 'Enter') {
          searchChoose(hot); ev.preventDefault();
        } else if (ev.key === 'Escape') {
          // No container to close, so the box clears first and closes
          // second.
          if (ev.target.value) {
            ev.target.value = ''; hot = 0; searchOpen();
          } else {
            searchClose(); ev.target.blur();
          }
        }
      });

      Shiny.addCustomMessageHandler('blockr-slides-download', function(msg) {
        var el = document.getElementById(msg.id);
        if (el) el.click();
      });
    });"
  )))
}

slides_ext_srv <- function(slides, title, format = "pptx") {

  function(id, board, update, session, parent, actions = NULL,
           visibility = NULL, ...) {
    moduleServer(
      id,
      function(input, output, session) {

        rv_slides <- reactiveVal(as.character(unlist(slides)))
        rv_title <- reactiveVal(
          if (is.character(title) && length(title)) title[[1L]] else "Deck"
        )
        rv_format <- reactiveVal(
          if (deck_format(format) %in% deck_formats()) {
            deck_format(format)
          } else {
            "pptx"
          }
        )

        # ---- the board, as names ------------------------------------
        #
        # Everything the panel DRAWS comes from here, and nothing here is an
        # expression. A block's name and its exhibit kind are properties of
        # the block object, readable off the board whether or not the block
        # has ever been constructed -- which is what lets a slide builder on
        # a deferred board cost nothing until the download.

        block_meta <- reactive(
          {
            blks <- blockr.core::board_blocks(board$board)

            lapply(
              setNames(nm = names(blks)),
              function(i) {
                list(
                  name = blockr.core::block_name(blks[[i]]),
                  kind = block_exhibit_kind(blks[[i]]),
                  icon = block_icon_html(blks[[i]]),
                  desc = block_descr_text(blks[[i]])
                )
              }
            )
          }
        )

        # Drop picks for blocks that have left the board: the id-keyed state
        # has to follow the board's block lifecycle, same as the outline's
        # annotations.
        observeEvent(
          board$board,
          {
            ids <- blockr.core::board_block_ids(board$board)
            keep <- intersect(rv_slides(), ids)
            if (!identical(rv_slides(), keep)) {
              rv_slides(keep)
            }
          }
        )

        # ---- picking and ordering -----------------------------------

        # The search menu is filled client-side from this payload: every
        # board block, the ones NOT yet in the deck first (the menu's whole
        # job is what to add next), each carrying what its card shows.
        # Pushed whole rather than diffed -- one small array that only moves
        # when the board or the deck does.
        # Identical-skip: `board$board` is reassigned by EVERY board update,
        # including the state a block commits as it constructs and the views
        # delta a dock tab click sends, and a plain reactive re-emits
        # regardless of whether anything it reads moved. The handler on the
        # other end rebuilds the whole menu, so an unskipped push repaints an
        # open dropdown for nothing. Same store the outline puts in front of
        # its projection (`board_shape`, R/ext.R).
        catalog_sig <- NULL

        # The catalogue describes the BOARD, and nothing in it depends on
        # the deck: which blocks are picked (and in what order) rides in its
        # own message below. It used to be a field on every entry, plus the
        # entry ORDER, so adding one slide resent the whole array --
        # every name, description and icon -- to say one flag had moved.
        # Icon markup is shared by block class for the same reason: inline
        # SVG is up to 1.4KB and a board repeats each of them per block.
        observe(
          {
            meta <- block_meta()
            ids <- names(meta)
            tbl <- icon_key_table(
              chr_ply(ids, function(i) na_blank(meta[[i]]$icon))
            )

            items <- lapply(
              seq_along(ids),
              function(k) {
                i <- ids[[k]]
                list(
                  id = i,
                  name = coal(na_blank(meta[[i]]$name), i),
                  icon_key = tbl$keys[[k]],
                  kind = coal(meta[[i]]$kind, ""),
                  desc = coal(meta[[i]]$desc, "")
                )
              }
            )

            if (!identical(items, catalog_sig)) {
              catalog_sig <<- items
              session$sendCustomMessage(
                "blockr-slides-catalog",
                list(items = items, icons = tbl$icons)
              )
            }
          }
        )

        # The deck: a list of ids, pushed on every change. Tens of bytes.
        observe(
          {
            session$sendCustomMessage(
              "blockr-slides-picked",
              list(ids = as.list(rv_slides()))
            )
          }
        )

        observeEvent(
          input$sld_add,
          {
            id <- input$sld_add
            req(is.character(id), length(id) == 1L, nzchar(id))

            if (!id %in% rv_slides()) {
              rv_slides(c(rv_slides(), id))
            }
          }
        )

        observeEvent(
          input$sld_act,
          {
            blk <- input$sld_act$id
            act <- input$sld_act$act
            req(is.character(blk), is.character(act))

            cur <- rv_slides()
            at <- match(blk, cur)
            req(!is.na(at))

            rv_slides(
              switch(
                act,
                rm = cur[-at],
                up = if (at > 1L) append(cur[-at], blk, after = at - 2L) else cur,
                down = if (at < length(cur)) {
                  append(cur[-at], blk, after = at)
                } else {
                  cur
                },
                cur
              )
            )
          }
        )

        observeEvent(
          input$sld_move,
          {
            blk <- input$sld_move$id
            target <- input$sld_move$target
            req(is.character(blk), is.character(target), !identical(blk, target))

            cur <- rv_slides()
            req(blk %in% cur, target %in% cur)

            rest <- setdiff(cur, blk)
            at <- match(target, rest)
            req(!is.na(at))

            rv_slides(
              append(rest, blk, after = if (isTRUE(input$sld_move$after)) at else at - 1L)
            )
          }
        )

        # ---- the list -----------------------------------------------
        #
        # renderUI replaces the list WHOLESALE, so it must fire only when the
        # list actually changes -- anything else is a visible white flash.
        # Its inputs re-emit far more often than they change: `block_meta()`
        # reads `board$board`, which is reassigned by every board update,
        # and a block committing its state as it constructs is one. So the
        # deck repainted whenever a block it slides loaded, having read back
        # the same names, kinds and icons.
        #
        # The store in front is the same mechanism as the outline's
        # `board_shape` (R/ext.R): the observer pays the read on every flush,
        # and a reactiveVal handed a value identical to the one it holds does
        # not notify -- so the render only re-runs when the drawing changes.
        list_state <- reactiveVal(NULL)

        observe(
          list_state(list(picked = rv_slides(), meta = block_meta()))
        )

        output$sld_list <- renderUI({

          state <- list_state()
          req(!is.null(state))

          picked <- state$picked
          meta <- state$meta

          if (!length(picked)) {
            return(
              div(
                class = "blockr-sld-empty",
                "No slides yet. Search above and add a block to make it one."
              )
            )
          }

          div(
            class = "blockr-sld-list",
            lapply(
              seq_along(picked),
              function(k) slides_row(picked[[k]], k, meta[[picked[[k]]]])
            )
          )
        })

        # ---- settings ------------------------------------------------

        observeEvent(input$sld_title, {
          if (!identical(input$sld_title, rv_title())) {
            rv_title(input$sld_title)
          }
        }, ignoreInit = TRUE)

        updateTextInput(session, "sld_title", value = isolate(rv_title()))

        observeEvent(input$sld_format, {
          if (!identical(input$sld_format, rv_format())) {
            rv_format(input$sld_format)
          }
        }, ignoreInit = TRUE)

        updateSelectInput(session, "sld_format", selected = isolate(rv_format()))

        # ---- the projection, on demand -------------------------------
        #
        # A lazy reactive that nothing reads while the panel is idle. The
        # download observer reads it on click; the wait observer below reads
        # it only once a demand is in flight (req() on `awaiting` first, so
        # no dependency is taken while it is NULL). That is the whole of the
        # outline's visibility gate, obtained by not needing one: the deck
        # never draws anything derived from an expression.

        # Last known expression per block id.
        #
        # A block's expr reactive reports NULL whenever it cannot produce one
        # right now, and "right now" is shorter than it sounds: a block whose
        # dock panel is not the visible tab stops reporting altogether.
        #
        # It is load-bearing for the DOWNLOAD specifically, which is the only
        # thing here that reads expressions at all. The wait observer
        # withdraws its demand the moment the closure reports (see
        # restore_demanded -- the dock overloads `required` as its card-build
        # ledger, so a TRUE left behind blanks a panel later), and only THEN
        # clicks the link. The block goes quiet again in between, so by the
        # time the download handler builds its own projection the expression
        # that was just waited for is gone -- and the deck renders without
        # the slide it was waiting for, silently, because a pending block is
        # skipped rather than raised. The cache is what carries the
        # expression across that gap.
        expr_cache <- new.env(parent = emptyenv())

        board_exprs <- reactive(
          {
            ex <- lapply(
              blockr.core::lst_xtr(board$blocks, "server", "expr"),
              function(e) {
                tryCatch(blockr.core::reval(e), error = function(err) NULL)
              }
            )

            for (id in names(ex)) {
              if (!is.null(ex[[id]])) {
                assign(id, ex[[id]], envir = expr_cache)
              }
            }

            # Keyed on the live board, so a removed block is gone for good
            # rather than resurrected from the cache.
            live <- blockr.core::board_block_ids(board$board)

            out <- lapply(
              setNames(nm = live),
              function(id) {
                if (!is.null(ex[[id]])) {
                  ex[[id]]
                } else if (exists(id, envir = expr_cache, inherits = FALSE)) {
                  get(id, envir = expr_cache)
                }
              }
            )

            rm(list = setdiff(ls(expr_cache), live), envir = expr_cache)

            # A block that has never reported an expression is PENDING, not
            # absent: it holds a placeholder, so the download can see it is
            # missing and demand it rather than quietly rendering a deck
            # without that slide.
            pending <- names(out)[vapply(out, is.null, logical(1L))]

            for (id in pending) {
              # NOT quote(NULL): NULL is self-evaluating, so quote(NULL) IS
              # NULL and assigning it deletes the element instead of filling
              # it.
              out[[id]] <- quote(invisible(NULL))
            }

            structure(out, pending = pending)
          }
        )

        sections <- reactive(slide_sections(board_exprs(), board$board, rv_slides()))

        qmd_txt <- reactive(export_deck_qmd(sections(), rv_title()))

        # ---- the two-stage download ----------------------------------

        awaiting <- reactiveVal(FALSE)
        wait_note <- reactiveVal(NULL)
        demanded <- reactiveVal(list())

        demand_blocks <- function(pending) {

          slots <- if (!is.null(visibility)) visibility$required

          if (is.null(slots)) {
            return(FALSE)
          }

          snap <- demanded()

          for (blk_id in pending) {
            slot <- slots[[blk_id]]
            if (is.function(slot)) {
              if (!blk_id %in% names(snap)) {
                snap[[blk_id]] <- isolate(slot())
              }
              slot(TRUE)
            }
          }

          demanded(snap)

          TRUE
        }

        # The dock overloads `required` as its card-build ledger, so a TRUE
        # left behind on a block whose card was never built would make the
        # first visit to its view skip the build -- a blank panel. Put the
        # prior value back once the demand is served.
        restore_demanded <- function() {

          snap <- demanded()

          for (blk_id in names(snap)) {
            slot <- visibility$required[[blk_id]]
            if (is.function(slot)) {
              slot(snap[[blk_id]])
            }
          }

          demanded(list())
        }

        drop_wait_note <- function() {
          note <- wait_note()
          if (!is.null(note)) {
            removeNotification(note)
            wait_note(NULL)
          }
        }

        pending_exported <- function(sects) {
          sects$ids[sects$exported & sects$pending]
        }

        fire_download <- function() {
          session$sendCustomMessage(
            "blockr-slides-download",
            list(id = session$ns("sld_dl"))
          )
        }

        observeEvent(
          input$sld_go,
          {
            if (!length(rv_slides())) {
              showNotification(
                "Pick at least one block before downloading a deck.",
                type = "warning"
              )
              return()
            }

            sects <- tryCatch(sections(), error = function(e) NULL)

            if (is.null(sects)) {
              showNotification(
                "The board is not ready yet; try again in a moment.",
                type = "warning"
              )
              return()
            }

            pending <- pending_exported(sects)

            if (!length(pending)) {
              fire_download()
              return()
            }

            if (!demand_blocks(pending)) {
              showNotification(
                paste(
                  "Some slide blocks are not initialized yet. Open their",
                  "views to initialize them, then download again."
                ),
                type = "warning",
                duration = 10
              )
              return()
            }

            awaiting(TRUE)
            drop_wait_note()
            wait_note(
              showNotification(
                sprintf(
                  paste(
                    "Evaluating %d block%s\u2026 the download starts when",
                    "the deck is ready."
                  ),
                  length(pending),
                  if (length(pending) == 1L) "" else "s"
                ),
                duration = NULL,
                closeButton = FALSE
              )
            )
          }
        )

        observe(
          {
            req(awaiting())

            if (length(pending_exported(sections()))) {
              return()
            }

            awaiting(FALSE)
            restore_demanded()
            drop_wait_note()
            fire_download()
          }
        )

        output$sld_dl <- downloadHandler(
          filename = function() {
            paste0(
              deck_filename(rv_title()),
              "-",
              format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
              ".",
              report_ext(rv_format())
            )
          },
          content = function(file) {
            # Both formats are written here, in this process. The HTML deck
            # is this package's own writer (R/deck-html.R); the PowerPoint is
            # officer. render_report()'s quarto path is the outline's, for
            # documents, and a deck never takes it.
            if (identical(rv_format(), "html")) {
              return(
                with_render_guard(
                  render_deck_html(sections(), file, rv_title())
                )
              )
            }

            with_render_guard(
              render_report(
                qmd_txt(),
                # The rmarkdown fallback (no quarto on the machine) renders a
                # document rather than a deck, and in document order: the
                # spin script cannot express a free slide order. A degraded
                # deck beats no download, and quarto is present everywhere
                # this actually ships.
                export_spin(sections()),
                rv_format(),
                file,
                rv_title(),
                template = effective_template(),
                sects = sections()
              )
            )
          }
        )

        # A downloadHandler behind a display:none link is a hidden output,
        # and Shiny suspends those -- which here means the href is never
        # populated and the JS click navigates to the bare page URL.
        outputOptions(output, "sld_dl", suspendWhenHidden = FALSE)

        list(
          state = list(
            slides = rv_slides,
            title = rv_title,
            format = rv_format
          )
        )
      }
    )
  }
}

# One row of the deck list. Pure markup, no Shiny inputs: the buttons report
# through one delegated handler (see slides_js), so the list can be
# re-rendered without anything to rebind.
slides_row <- function(id, k, meta) {

  meta <- coal(meta, list())

  act <- function(a, label, path) {
    tags$button(
      type = "button",
      class = "blockr-sld-act",
      `data-act` = a,
      title = label,
      `aria-label` = label,
      HTML(paste0(
        "<svg width='13' height='13' viewBox='0 0 24 24' fill='none' ",
        "stroke='currentColor' stroke-width='2.2' stroke-linecap='round' ",
        "stroke-linejoin='round'>", path, "</svg>"
      ))
    )
  }

  # The block browser's card, compact form: icon tile, name, trailing pill.
  # Same classes as the picker's menu rows and the dock's own sidebar, so a
  # slide reads as the block it is. What the deck adds is the number (and
  # the drag, and the actions).
  div(
    class = "blockr-sld-row blockr-block-browser-card",
    `data-blk` = id,
    draggable = "true",
    div(
      class = "blockr-block-browser-card-header",
      span(class = "blockr-sld-num", k),
      if (nzchar(coal(meta$icon, ""))) {
        span(class = "blockr-block-browser-card-icon", HTML(meta$icon))
      },
      div(
        class = "blockr-block-browser-card-body",
        div(
          class = "blockr-block-browser-card-titles",
          span(
            class = "blockr-block-browser-card-name",
            coal(na_blank(meta$name), id)
          ),
          if (nzchar(coal(meta$kind, ""))) {
            span(class = "blockr-block-browser-card-package", meta$kind)
          }
        )
      ),
      div(
        class = "blockr-sld-acts",
        act("up", "Move up", "<polyline points='18 15 12 9 6 15'/>"),
        act("down", "Move down", "<polyline points='6 9 12 15 18 9'/>"),
        act("rm", "Remove slide", "<path d='M18 6 6 18M6 6l12 12'/>")
      )
    )
  )
}

# The title, as a filename stem. A deck called "Q3 review / EU" must not
# produce a path separator, and a title of nothing at all must still produce
# a name.
deck_filename <- function(title) {

  out <- gsub("^-+|-+$", "", gsub("-+", "-", gsub("[^A-Za-z0-9]+", "-", title)))

  if (!nzchar(out)) "deck" else tolower(out)
}
