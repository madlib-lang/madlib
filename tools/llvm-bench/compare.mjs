#!/usr/bin/env node
// Compare results.json (current branch) against baseline.json.
// Prints a markdown table with median, delta, and % change.
//
// Usage:
//   node tools/llvm-bench/compare.mjs

import { readFileSync, existsSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const baselinePath = join(__dirname, "baseline.json");
const resultsPath = join(__dirname, "results.json");

if (!existsSync(baselinePath)) {
  console.error(`baseline.json not found at ${baselinePath}`);
  console.error("run: node tools/llvm-bench/run.mjs --baseline");
  process.exit(1);
}
if (!existsSync(resultsPath)) {
  console.error(`results.json not found at ${resultsPath}`);
  console.error("run: node tools/llvm-bench/run.mjs");
  process.exit(1);
}

const baseline = JSON.parse(readFileSync(baselinePath, "utf8"));
const current = JSON.parse(readFileSync(resultsPath, "utf8"));

const baseByName = new Map(baseline.results.map((r) => [r.name, r]));

function fmt(ms) {
  return `${ms.toFixed(1)} ms`;
}

function delta(curr, base) {
  const d = curr - base;
  const pct = (d / base) * 100;
  const sign = d >= 0 ? "+" : "";
  return { abs: `${sign}${d.toFixed(1)} ms`, pct: `${sign}${pct.toFixed(1)}%`, raw: pct };
}

const w = (s, n) => String(s).padEnd(n);
const cols = [
  ["bench", 26],
  ["baseline", 12],
  ["current", 12],
  ["delta", 14],
  ["%", 10],
  ["cks match", 10],
];
const header = cols.map(([c, n]) => w(c, n)).join(" | ");
const sep = cols.map(([, n]) => "-".repeat(n)).join("-+-");
process.stdout.write(`baseline: ${baseline.capturedAt}\n`);
process.stdout.write(`current:  ${current.capturedAt}\n\n`);
process.stdout.write(`${header}\n${sep}\n`);

let regressions = 0;
let improvements = 0;
for (const r of current.results) {
  const b = baseByName.get(r.name);
  if (!b) {
    process.stdout.write(`${w(r.name, 26)} | ${w(fmt(r.medianMs), 12)} | (no baseline)\n`);
    continue;
  }
  const d = delta(r.medianMs, b.medianMs);
  const cksMatch = b.checksum === r.checksum ? "yes" : "NO";
  process.stdout.write(
    [
      w(r.name, 26),
      w(fmt(b.medianMs), 12),
      w(fmt(r.medianMs), 12),
      w(d.abs, 14),
      w(d.pct, 10),
      w(cksMatch, 10),
    ].join(" | ") + "\n",
  );
  if (d.raw < -3) improvements++;
  if (d.raw > 3) regressions++;
}

process.stdout.write(`\nimprovements (>3% faster): ${improvements}\n`);
process.stdout.write(`regressions  (>3% slower): ${regressions}\n`);
if (regressions > 0) process.exit(2);
