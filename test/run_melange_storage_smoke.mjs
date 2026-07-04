import { pathToFileURL } from "node:url";

import "./melange_weak_polyfill.mjs";

await import(pathToFileURL(process.argv[2]).href);
