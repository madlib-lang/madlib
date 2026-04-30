#!/usr/bin/env node
// JS backend benchmark harness for the Madlib compiler.
//
// For each benchmark in manifest.json:
//   1. Wipe and recreate a per-bench output dir under .builds/<name>/.
//   2. Compile the .mad source with `madlib compile -t node --O3`.
//   3. Run the resulting .mjs entry under Node N+warmup times, externally
//      timed via process.hrtime.bigint() around child_process.spawnSync.
//   4. Capture stdout for a checksum (catches semantic regressions when JS
//      output changes shape) and report median/min/max wall-clock.
//
// Output: results.json + a markdown table to stdout.
//
// Usage:
//   node tools/js-bench/run.mjs               (writes tools/js-bench/results.json)
//   node tools/js-bench/run.mjs --baseline    (writes tools/js-bench/baseline.json)
//   node tools/js-bench/run.mjs --filter ADT  (only runs benches matching substring)

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, rmSync, mkdirSync, existsSync } from "node:fs";
import { dirname, resolve, join } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const repoRoot = resolve(__dirname, "..", "..");
const buildsRoot = join(__dirname, ".builds");

const args = process.argv.slice(2);
const writeBaseline = args.includes("--baseline");
const filterIdx = args.indexOf("--filter");
const filter = filterIdx >= 0 ? args[filterIdx + 1] : null;

const manifestPath = join(__dirname, "manifest.json");
const manifest = JSON.parse(readFileSync(manifestPath, "utf8"));
const defaultIters = manifest.iters ?? 5;
const defaultWarmup = manifest.warmup ?? 1;

function compile(bench) {
  const outDir = join(buildsRoot, bench.name);
  rmSync(outDir, { recursive: true, force: true });
  mkdirSync(outDir, { recursive: true });
  const sourcePath = join(repoRoot, bench.source);
  const result = spawnSync(
    "madlib",
    ["compile", "-i", sourcePath, "-o", outDir, "-t", "node", "--O3"],
    { encoding: "utf8" },
  );
  if (result.status !== 0) {
    console.error(`compile failed for ${bench.name}:`);
    console.error(result.stdout);
    console.error(result.stderr);
    process.exit(1);
  }
  return outDir;
}

function timedRun(entryPath) {
  const start = process.hrtime.bigint();
  const result = spawnSync("node", ["--expose-gc", entryPath], {
    encoding: "utf8",
  });
  const end = process.hrtime.bigint();
  if (result.status !== 0) {
    console.error(`run failed for ${entryPath}:`);
    console.error(result.stdout);
    console.error(result.stderr);
    process.exit(1);
  }
  const wallMs = Number(end - start) / 1e6;
  return { wallMs, stdout: result.stdout };
}

function median(xs) {
  const sorted = [...xs].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 0
    ? (sorted[mid - 1] + sorted[mid]) / 2
    : sorted[mid];
}

function checksum(s) {
  return createHash("sha256").update(s).digest("hex").slice(0, 16);
}

function fmt(ms) {
  return `${ms.toFixed(1)} ms`;
}

const results = [];

for (const bench of manifest.benchmarks) {
  if (filter && !bench.name.includes(filter)) continue;
  const iters = bench.iters ?? defaultIters;
  const warmup = bench.warmup ?? defaultWarmup;

  process.stdout.write(`[${bench.name}] compiling…\n`);
  const outDir = compile(bench);
  const entryPath = join(outDir, bench.entry);
  if (!existsSync(entryPath)) {
    console.error(`entry not found after compile: ${entryPath}`);
    process.exit(1);
  }

  process.stdout.write(`[${bench.name}] warmup x${warmup}…\n`);
  let firstStdout = null;
  for (let i = 0; i < warmup; i++) {
    const r = timedRun(entryPath);
    if (firstStdout === null) firstStdout = r.stdout;
  }

  const samples = [];
  for (let i = 0; i < iters; i++) {
    const r = timedRun(entryPath);
    samples.push(r.wallMs);
    process.stdout.write(`[${bench.name}] iter ${i + 1}/${iters}: ${fmt(r.wallMs)}\n`);
    if (firstStdout !== null && r.stdout !== firstStdout) {
      console.error(`[${bench.name}] stdout changed across runs — non-deterministic bench!`);
    }
  }

  const med = median(samples);
  const min = Math.min(...samples);
  const max = Math.max(...samples);
  const cks = checksum(firstStdout ?? "");

  results.push({
    name: bench.name,
    iters,
    warmup,
    samples,
    medianMs: med,
    minMs: min,
    maxMs: max,
    checksum: cks,
  });

  process.stdout.write(
    `[${bench.name}] median=${fmt(med)}  min=${fmt(min)}  max=${fmt(max)}  cks=${cks}\n\n`,
  );
}

const outFile = writeBaseline ? "baseline.json" : "results.json";
const outPath = join(__dirname, outFile);
writeFileSync(
  outPath,
  JSON.stringify(
    {
      capturedAt: new Date().toISOString(),
      node: process.version,
      results,
    },
    null,
    2,
  ),
);
process.stdout.write(`wrote ${outPath}\n\n`);

// Markdown table
const w = (s, n) => String(s).padEnd(n);
const cols = [["bench", 26], ["median", 12], ["min", 12], ["max", 12], ["cks", 18]];
const header = cols.map(([c, n]) => w(c, n)).join(" | ");
const sep = cols.map(([, n]) => "-".repeat(n)).join("-+-");
process.stdout.write(`${header}\n${sep}\n`);
for (const r of results) {
  process.stdout.write(
    [
      w(r.name, 26),
      w(fmt(r.medianMs), 12),
      w(fmt(r.minMs), 12),
      w(fmt(r.maxMs), 12),
      w(r.checksum, 18),
    ].join(" | ") + "\n",
  );
}
