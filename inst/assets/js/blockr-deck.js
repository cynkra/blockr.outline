/* The HTML deck's client half. Inlined into the downloaded file.

   Deliberately small. The paging itself is CSS scroll-snap, so the wheel,
   the trackpad, touch and the scrollbar already work and are not
   reimplemented here. What is left is what a reader of a DECK expects on top
   of a scrolling page: arrow keys that land on whole slides, a fullscreen
   key for presenting from it, and a counter that says where you are. */
(function () {
  "use strict";

  var stage = document.querySelector(".bd-stage");
  if (!stage) return;

  var slides = Array.prototype.slice.call(
    document.querySelectorAll(".bd-slide")
  );
  if (!slides.length) return;

  var counter = document.querySelector(".bd-count");
  var at = 0;

  // ---- fit the canvas to the window ---------------------------------
  //
  // Every slide is a fixed 1280x720 box scaled to whatever the window
  // offers, which is what lets the exhibits inside be sized in plain px and
  // still fill a 4K screen. Recomputed on resize and on fullscreen, since
  // both change the box.
  function fit() {
    var pad = 32;
    var w = (window.innerWidth - pad) / 1280;
    var h = (window.innerHeight - pad) / 720;
    var scale = Math.min(w, h);
    if (!isFinite(scale) || scale <= 0) scale = 1;
    document.documentElement.style.setProperty("--bd-scale", scale);
  }

  // ---- where are we -------------------------------------------------
  //
  // Read off the scroll position rather than tracked in a variable, so a
  // wheel scroll, a drag of the scrollbar and an arrow key all agree.
  function current() {
    var top = stage.scrollTop;
    var best = 0;
    var dist = Infinity;
    for (var i = 0; i < slides.length; i++) {
      var d = Math.abs(slides[i].offsetTop - top);
      if (d < dist) { dist = d; best = i; }
    }
    return best;
  }

  function show(i) {
    at = Math.max(0, Math.min(slides.length - 1, i));
    stage.scrollTo({ top: slides[at].offsetTop, behavior: "smooth" });
    mark();
  }

  function mark() {
    if (counter) counter.textContent = (current() + 1) + " / " + slides.length;
  }

  // ---- input ---------------------------------------------------------

  document.addEventListener("keydown", function (ev) {
    if (ev.metaKey || ev.ctrlKey || ev.altKey) return;

    var k = ev.key;

    if (k === "ArrowRight" || k === "ArrowDown" || k === "PageDown" ||
        k === " " || k === "Spacebar") {
      // Space scrolls a long table before it advances the slide: a table
      // that did not fit is still content, and jumping past it would hide
      // the numbers a reader came for.
      var body = slides[current()].querySelector(".bd-body");
      if (k === " " && body && body.scrollHeight - body.clientHeight -
          body.scrollTop > 4) {
        body.scrollBy({ top: body.clientHeight * 0.9, behavior: "smooth" });
        ev.preventDefault();
        return;
      }
      show(current() + 1);
      ev.preventDefault();
    } else if (k === "ArrowLeft" || k === "ArrowUp" || k === "PageUp") {
      show(current() - 1);
      ev.preventDefault();
    } else if (k === "Home") {
      show(0);
      ev.preventDefault();
    } else if (k === "End") {
      show(slides.length - 1);
      ev.preventDefault();
    } else if (k === "f" || k === "F") {
      full();
      ev.preventDefault();
    }
  });

  function full() {
    if (document.fullscreenElement) {
      if (document.exitFullscreen) document.exitFullscreen();
    } else if (document.documentElement.requestFullscreen) {
      document.documentElement.requestFullscreen();
    }
  }

  var prev = document.querySelector(".bd-prev");
  var next = document.querySelector(".bd-next");
  var fs = document.querySelector(".bd-full");
  if (prev) prev.addEventListener("click", function () { show(current() - 1); });
  if (next) next.addEventListener("click", function () { show(current() + 1); });
  if (fs) fs.addEventListener("click", full);

  stage.addEventListener("scroll", function () {
    // Cheap: the counter is the only thing that follows the scroll.
    window.requestAnimationFrame(mark);
  });

  window.addEventListener("resize", fit);
  document.addEventListener("fullscreenchange", fit);

  fit();
  mark();
})();
