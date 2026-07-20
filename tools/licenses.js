// Regenerate the third-party license inventory for the vendored JS bundle.
//
// Reads the esbuild metafile (which lists every module that actually ends up
// in inst/js/md-editor.js), maps each module back to its npm package, and
// writes LICENSE.note plus the verbatim license texts under inst/licenses/.
//
//   npm run build:meta && npm run licenses
//
// Build-only tooling (esbuild and friends) never appears here: the metafile
// records what was bundled, not what was installed.

const fs = require("fs");
const path = require("path");

const META = process.argv[2] || "tools/meta.json";
const OUT_NOTE = "LICENSE.note";
const OUT_DIR = "inst/licenses";

const LICENSE_FILE = /^(LICEN[CS]E|COPYING)(\..*)?$/i;

function pkgDirOf(file) {
  // node_modules/@scope/name/dist/x.js -> node_modules/@scope/name
  const parts = file.split("/");
  let last = -1;
  for (let i = 0; i < parts.length; i++) if (parts[i] === "node_modules") last = i;
  if (last === -1) return null;
  const scoped = parts[last + 1] && parts[last + 1].startsWith("@");
  return parts.slice(0, last + (scoped ? 3 : 2)).join("/");
}

function readJson(p) {
  try {
    return JSON.parse(fs.readFileSync(p, "utf8"));
  } catch (e) {
    return null;
  }
}

function licenseText(dir) {
  let entries;
  try {
    entries = fs.readdirSync(dir);
  } catch (e) {
    return null;
  }
  const hit = entries.find((f) => LICENSE_FILE.test(f));
  if (!hit) return null;
  try {
    return fs.readFileSync(path.join(dir, hit), "utf8").trim();
  } catch (e) {
    return null;
  }
}

const meta = readJson(META);
if (!meta) {
  console.error(`cannot read metafile ${META} -- run \`npm run build:meta\` first`);
  process.exit(1);
}

const pkgs = new Map();
for (const file of Object.keys(meta.inputs)) {
  const dir = pkgDirOf(file);
  if (!dir || pkgs.has(dir)) continue;
  const pj = readJson(path.join(dir, "package.json"));
  if (!pj || !pj.name) continue;
  pkgs.set(dir, {
    name: pj.name,
    version: pj.version || "?",
    license: typeof pj.license === "string" ? pj.license : (pj.license && pj.license.type) || "?",
    homepage: pj.homepage || (pj.repository && (pj.repository.url || pj.repository)) || "",
    text: licenseText(dir),
  });
}

const list = [...pkgs.values()].sort((a, b) => a.name.localeCompare(b.name));
if (!list.length) {
  console.error("no bundled packages found in metafile -- wrong path?");
  process.exit(1);
}

// One copy of each distinct license text, keyed by the package it came from.
fs.rmSync(OUT_DIR, { recursive: true, force: true });
fs.mkdirSync(OUT_DIR, { recursive: true });

const byText = new Map();
for (const p of list) {
  if (!p.text) continue;
  if (!byText.has(p.text)) byText.set(p.text, []);
  byText.get(p.text).push(p);
}

// Name each text after the package it came from, so LICENSE.note reads as a
// map rather than a numbered pile. Packages sharing a byte-identical text
// (the whole prosemirror-* family, say) point at the first one's file.
const written = [];
for (const [text, owners] of byText) {
  const lic = owners[0].license.replace(/[^A-Za-z0-9.-]/g, "-");
  const slug = owners[0].name.replace(/^@/, "").replace(/[^A-Za-z0-9.-]/g, "-");
  const file = `${slug}-${lic}.txt`;
  fs.writeFileSync(path.join(OUT_DIR, file), text + "\n");
  written.push({ file, owners });
}

const byLicense = new Map();
for (const p of list) {
  if (!byLicense.has(p.license)) byLicense.set(p.license, []);
  byLicense.get(p.license).push(p);
}

const textOf = new Map();
for (const w of written) for (const o of w.owners) textOf.set(o.name, w.file);

let out = `Third-party components bundled in inst/js/md-editor.js
=======================================================

blockr.outline itself is licensed GPL (>= 3); see LICENSE.md.

The file inst/js/md-editor.js is a pre-built JavaScript bundle produced by
esbuild from srcjs/md-editor/ (see package.json, \`npm run build\`). It embeds
the npm packages listed below. Their licenses are reproduced verbatim in
inst/licenses/; all are permissive and compatible with GPL-3.

This file is generated -- do not edit by hand. Regenerate with:

    npm ci && npm run build:meta && npm run licenses

Summary
-------

`;

for (const [lic, ps] of [...byLicense.entries()].sort()) {
  out += `  ${lic}: ${ps.length} package${ps.length === 1 ? "" : "s"}\n`;
}

out += `
  Total: ${list.length} bundled packages.

Packages
--------

`;

for (const p of list) {
  const home = p.homepage
    ? String(p.homepage).replace(/^git\+/, "").replace(/\.git$/, "")
    : "";
  out += `${p.name} ${p.version}\n`;
  out += `  License: ${p.license}\n`;
  if (home) out += `  Source:  ${home}\n`;
  const t = textOf.get(p.name);
  out += `  Text:    ${t ? `inst/licenses/${t}` : "(no license file shipped in the npm package)"}\n\n`;
}

fs.writeFileSync(OUT_NOTE, out);
console.log(`${OUT_NOTE}: ${list.length} bundled packages, ${written.length} distinct license texts in ${OUT_DIR}/`);
