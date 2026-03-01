import { describe, expect, it } from "vitest";
import { join } from "node:path";
import { runWriteModeAndSnapshot, runAndSnapshot } from "../utils";

const fixturesDir = join(import.meta.dirname, "fixtures");

describe("astro", () => {
  it("should format .astro files with --check", async () => {
    const testCases = [
      ["--check", "basic.astro"],
      ["--check", "no-frontmatter.astro"],
      ["--check", "with-comment.astro"],
    ];

    const snapshot = await runAndSnapshot(fixturesDir, testCases);
    expect(snapshot).toMatchSnapshot();
  });

  it("should format .astro files in write mode", async () => {
    const snapshot = await runWriteModeAndSnapshot(fixturesDir, [
      "basic.astro",
      "no-frontmatter.astro",
      "with-comment.astro",
    ]);
    expect(snapshot).toMatchSnapshot();
  });
});
