const fs = require("fs");
const os = require("os");
const path = require("path");
const { spawnSync } = require("child_process");

function findBrowser() {
  const candidates = [
    process.env.CHROME_BIN,
    process.env.BROWSER,
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge",
    "google-chrome",
    "google-chrome-stable",
    "chromium",
    "chromium-browser",
  ].filter(Boolean);

  for (const candidate of candidates) {
    if (candidate.includes(path.sep) && fs.existsSync(candidate)) return candidate;
    const found = spawnSync("sh", ["-c", `command -v ${JSON.stringify(candidate)}`], {
      encoding: "utf8",
    });
    if (found.status === 0) return found.stdout.trim();
  }
  throw new Error("Chrome/Chromium not found; set CHROME_BIN to run browser memory tests");
}

const jsPath = process.argv[2];
if (!jsPath) throw new Error("usage: run_browser_memory_test.js <js_of_ocaml-output>");

const browser = findBrowser();
const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "pss-browser-memory-"));
const htmlPath = path.join(tmpDir, "index.html");
const html = `<!doctype html>
<html>
<head><meta charset="utf-8"><title>PSS browser memory test</title></head>
<body data-pss-memory-result="pending">pending</body>
<script src="${path.resolve(jsPath).replaceAll("&", "&amp;").replaceAll('"', "&quot;")}"></script>
</html>`;

fs.writeFileSync(htmlPath, html);

const result = spawnSync(
  browser,
  [
    "--headless=new",
    "--disable-gpu",
    "--no-sandbox",
    "--disable-dev-shm-usage",
    "--js-flags=--expose-gc",
    "--virtual-time-budget=5000",
    "--dump-dom",
    `file://${htmlPath}`,
  ],
  { encoding: "utf8" }
);

fs.rmSync(tmpDir, { recursive: true, force: true });

if (result.error) throw result.error;
if (result.status !== 0) {
  process.stderr.write(result.stderr);
  process.exit(result.status);
}

if (!result.stdout.includes('data-pss-memory-result="pass"')) {
  process.stderr.write(result.stderr);
  process.stderr.write(result.stdout);
  throw new Error("browser memory test did not report pass");
}
