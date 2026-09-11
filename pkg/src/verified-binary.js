const { createHash } = require("crypto");
const { createWriteStream, existsSync, mkdirSync, rmSync } = require("fs");
const { join } = require("path");
const { tmpdir } = require("os");
const { pipeline } = require("stream/promises");
const { spawnSync } = require("child_process");
const https = require("https");
const tar = require("tar");

function error(message) { throw new Error(message); }

function checksumFor(text, filename) {
  const escaped = filename.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
  const match = text.match(new RegExp(`^([a-fA-F0-9]{64})\\s+[* ]?${escaped}$`, "m"));
  return match && match[1].toLowerCase();
}

function get(url, redirects = 0) {
  if (redirects > 5) return Promise.reject(new Error("Too many redirects while downloading Madlib."));
  return new Promise((resolve, reject) => {
    const request = https.get(url, { headers: { "User-Agent": "madlib-installer" } }, (response) => {
      if (response.statusCode >= 300 && response.statusCode < 400 && response.headers.location) {
        response.resume();
        resolve(get(new URL(response.headers.location, url), redirects + 1));
      } else if (response.statusCode === 200) {
        resolve(response);
      } else {
        response.resume();
        reject(new Error(`Download failed with HTTP ${response.statusCode}.`));
      }
    });
    request.on("error", reject);
    request.setTimeout(30_000, () => request.destroy(new Error("Download timed out.")));
  });
}

async function getText(url) {
  const response = await get(url);
  let text = "";
  for await (const chunk of response) text += chunk;
  return text;
}

class VerifiedBinary {
  constructor(name, url, checksumsUrl, archiveName, localBinaryPath = null) {
    this.name = name;
    this.url = url;
    this.checksumsUrl = checksumsUrl;
    this.archiveName = archiveName;
    this.installDirectory = join(__dirname, "..", "node_modules", ".bin");
    this.binaryPath = join(this.installDirectory, name);
    this.localBinaryPath = localBinaryPath;
  }

  executablePath() {
    return this.localBinaryPath && existsSync(this.localBinaryPath)
      ? this.localBinaryPath
      : this.binaryPath;
  }

  exists() { return existsSync(this.executablePath()); }

  async install() {
    if (this.exists()) return;
    const expected = checksumFor(await getText(this.checksumsUrl), this.archiveName);
    if (!expected) error(`Release checksum for ${this.archiveName} is missing from ${this.checksumsUrl}.`);

    const temporaryArchive = join(tmpdir(), `${this.archiveName}.${process.pid}.${Date.now()}`);
    const hash = createHash("sha256");
    try {
      const response = await get(this.url);
      await pipeline(response.data, async function* (source) {
        for await (const chunk of source) {
          hash.update(chunk);
          yield chunk;
        }
      }, createWriteStream(temporaryArchive, { mode: 0o600 }));
      if (hash.digest("hex") !== expected) error(`Checksum mismatch for ${this.archiveName}; refusing to install it.`);

      rmSync(this.installDirectory, { recursive: true, force: true });
      mkdirSync(this.installDirectory, { recursive: true });
      await tar.x({ file: temporaryArchive, strip: 1, C: this.installDirectory });
      if (!this.exists()) error(`Release ${this.archiveName} did not contain the ${this.name} executable.`);
    } finally {
      rmSync(temporaryArchive, { force: true });
    }
  }

  async run() {
    await this.install();
    const result = spawnSync(this.executablePath(), process.argv.slice(2), { cwd: process.cwd(), stdio: "inherit" });
    if (result.error) error(result.error.message);
    process.exit(result.status === null ? 1 : result.status);
  }
}

module.exports = { VerifiedBinary, checksumFor };
