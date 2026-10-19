#!/usr/bin/env node
// pele-render.mjs -- render Mermaid snippets with pele for Emacs.
//
// Pele emits SVG that relies on CSS custom properties.  librsvg (Emacs' SVG
// renderer) does not implement custom-property declarations, so `fill="var(--_fg)"'
// would render black.  This wrapper resolves every var() to a literal value first;
// that step is why Emacs can display the output.
//
// Usage:
//   node pele-render.mjs <path/to/pele/dist/pele.js> <theme-json>
//
// stdin:  [{"id": "...", "text": "flowchart LR\\n  A-->B"}]
// stdout: [{"id": "...", "ok": true, "type": "flowchart",
//           "svg": "<svg ...>", "width": 123, "height": 45}]
//         or [{"id": "...", "ok": false, "error": "..."}]
//
// The theme object uses short keys (bg, fg, surface, ...); they are mapped to
// pele's public --pele-* custom properties.

import { pathToFileURL } from 'node:url';

const [distPath, themeArg] = process.argv.slice(2);
if (!distPath) {
  process.stderr.write('usage: node pele-render.mjs <pele-module> [theme-json]\n');
  process.exit(2);
}

let render;
try {
  ({ render } = await import(pathToFileURL(distPath).href));
} catch (error) {
  process.stderr.write(`cannot load pele from ${distPath}: ${error.message}\n`);
  process.exit(3);
}

const rawTheme = themeArg ? JSON.parse(themeArg) : {};
const theme = {};
for (const [key, value] of Object.entries(rawTheme)) {
  theme[key.startsWith('--') ? key : `--pele-${key}`] = String(value);
}

// Resolve `var(--x)' / `var(--x, fallback)' against custom-property declarations
// found in style attributes.  Unknown public properties use `theme' or their fallback.
function flattenTheme(svg, overrides) {
  const declarations = new Map();
  for (const match of svg.matchAll(/style="([^"]*)"/g)) {
    for (const part of match[1].split(';')) {
      const colon = part.indexOf(':');
      if (colon < 0) continue;
      const name = part.slice(0, colon).trim();
      if (name.startsWith('--')) declarations.set(name, part.slice(colon + 1).trim());
    }
  }

  const resolve = (value, depth = 0) => {
    if (depth > 20) return value;
    let out = '';
    let index = 0;
    while (index < value.length) {
      const at = value.indexOf('var(', index);
      if (at < 0) {
        out += value.slice(index);
        break;
      }
      out += value.slice(index, at);
      let close = at + 3;
      let depthParen = 0;
      for (; close < value.length; close++) {
        if (value[close] === '(') depthParen++;
        else if (value[close] === ')' && --depthParen === 0) break;
      }
      const inner = value.slice(at + 4, close);
      let comma = -1;
      depthParen = 0;
      for (let i = 0; i < inner.length; i++) {
        if (inner[i] === '(') depthParen++;
        else if (inner[i] === ')') depthParen--;
        else if (inner[i] === ',' && depthParen === 0) {
          comma = i;
          break;
        }
      }
      const name = (comma < 0 ? inner : inner.slice(0, comma)).trim();
      const fallback = comma < 0 ? undefined : inner.slice(comma + 1).trim();
      let replacement;
      if (Object.hasOwn(overrides, name)) replacement = overrides[name];
      else if (declarations.has(name)) replacement = resolve(declarations.get(name), depth + 1);
      else if (fallback !== undefined) replacement = resolve(fallback, depth + 1);
      else replacement = 'inherit';
      out += replacement;
      index = close + 1;
    }
    return out;
  };

  return resolve(svg);
}

const chunks = [];
for await (const chunk of process.stdin) chunks.push(chunk);
const items = JSON.parse(Buffer.concat(chunks).toString('utf8'));

const output = items.map(({ id, text }) => {
  try {
    const result = render(text, {
      fontFamily: theme['--pele-font'] || 'Arial',
      fontFamilyMono: theme['--pele-font-mono'] || 'monospace',
      links: false,
      images: false,
    });
    return {
      id,
      ok: true,
      type: result.type,
      width: result.width,
      height: result.height,
      svg: flattenTheme(result.svg, theme),
    };
  } catch (error) {
    return {
      id,
      ok: false,
      error: error && error.message ? error.message : String(error),
      code: error && error.code ? error.code : null,
    };
  }
});

process.stdout.write(JSON.stringify(output));
