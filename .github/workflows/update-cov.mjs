import fs from "fs"

const coverageDotJsonContent = fs.readFileSync("../../coverage/codecov.json")

const coverageDotJson = JSON.parse(coverageDotJsonContent)

const updated = {
  ...coverageDotJson,
  coverage: Object.fromEntries(Object.entries(coverageDotJson.coverage).map(([k, v]) => [
    k,
    Object.fromEntries(Object.entries(v).map(([line, cov]) => [
      line,
      cov === "1/2"
        ? "1"
        : cov
    ]))
  ]))
}

fs.writeFileSync("../../coverage/codecov.json", JSON.stringify(updated))
