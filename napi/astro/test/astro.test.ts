import { describe, expect, it } from "vitest";

import { compileAstroSync, compileAstro } from "../index";

describe("compileAstroSync", () => {
  it("compiles a simple Astro component", () => {
    const result = compileAstroSync("<h1>Hello</h1>");
    expect(result.errors).toEqual([]);
    const code = result.code;
    expect(code).toContain("$$render");
    expect(code).toContain("<h1>Hello</h1>");
    expect(code).toContain("$$createComponent");
  });

  it("compiles frontmatter", () => {
    const result = compileAstroSync(`---
const name = "World";
---
<h1>Hello {name}!</h1>`);
    expect(result.errors).toEqual([]);
    const code = result.code;
    expect(code).toContain('const name = "World"');
    expect(code).toContain("${name}");
  });

  it("accepts filename option", () => {
    const result = compileAstroSync("<h1>Hello</h1>", {
      filename: "Test.astro",
    });
    expect(result.errors).toEqual([]);
    expect(result.code).toContain("$$createComponent");
  });

  it("includes metadata when requested", () => {
    const result = compileAstroSync("<h1>Hello</h1>", {
      includeMetadata: true,
    });
    expect(result.errors).toEqual([]);
    const code = result.code;
    expect(code).toContain("$$metadata");
  });

  it("handles components", () => {
    const result = compileAstroSync("<Component />");
    expect(result.errors).toEqual([]);
    expect(result.code).toContain("$$renderComponent");
  });

  it("returns errors for invalid syntax", () => {
    const result = compileAstroSync("{ invalid js {{{");
    expect(result.errors.length).toBeGreaterThan(0);
    expect(result.code).toEqual("");
  });

  it("handles MathML content", () => {
    const result = compileAstroSync('<math><annotation>R^{2x}</annotation></math>');
    expect(result.errors).toEqual([]);
    expect(result.code).toContain("R^{2x}");
  });

  it("returns TransformResult fields", () => {
    const result = compileAstroSync("<h1>Hello</h1>");
    expect(result.errors).toEqual([]);
    // New fields from TransformResult
    expect(typeof result.map).toBe("string");
    expect(typeof result.scope).toBe("string");
    expect(result.scope.length).toBeGreaterThan(0);
    expect(result.css).toEqual([]);
    expect(result.scripts).toEqual([]);
    expect(result.hydratedComponents).toEqual([]);
    expect(result.clientOnlyComponents).toEqual([]);
    expect(result.serverComponents).toEqual([]);
    expect(typeof result.containsHead).toBe("boolean");
    expect(typeof result.propagation).toBe("boolean");
    expect(result.styleError).toEqual([]);
    expect(result.diagnostics).toEqual([]);
  });

  it("detects explicit <head> element", () => {
    const result = compileAstroSync("<html><head><title>Test</title></head><body><h1>Hi</h1></body></html>");
    expect(result.errors).toEqual([]);
    expect(result.containsHead).toBe(true);
  });

  it("reports containsHead false when no head", () => {
    const result = compileAstroSync("<h1>Hello</h1>");
    expect(result.errors).toEqual([]);
    expect(result.containsHead).toBe(false);
  });
});

describe("compileAstro (async)", () => {
  it("compiles a simple Astro component", async () => {
    const result = await compileAstro("<h1>Hello</h1>");
    expect(result.errors).toEqual([]);
    expect(result.code).toContain("$$render");
  });

  it("returns TransformResult fields", async () => {
    const result = await compileAstro("<h1>Hello</h1>");
    expect(result.errors).toEqual([]);
    expect(typeof result.map).toBe("string");
    expect(typeof result.scope).toBe("string");
    expect(result.css).toEqual([]);
    expect(result.scripts).toEqual([]);
  });
});
