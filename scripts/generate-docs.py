#!/usr/bin/env python3
"""
Generate Markdown documentation from Coal standard library modules.

Extracts docblocks (/** ... */) from .coal files and converts them into 
Markdown documentation. Docblocks can include a **Type**: field for the
type signature of the associated definition. Definitions without a docblock
are still included with a "No documentation available." placeholder.

Usage:
    python3 scripts/generate-docs.py                    # process all lang/*.coal files
    python3 scripts/generate-docs.py lang/List.coal     # process specific files
    python3 scripts/generate-docs.py --output-dir docs/generated  # specify output dir
"""

import os
import re
import sys
import argparse
from pathlib import Path
from typing import Optional


def find_project_root() -> Path:
    """Find the project root (where the lang/ directory is located)."""
    cwd = Path.cwd()
    if (cwd / "lang").is_dir():
        return cwd
    if (cwd.parent / "lang").is_dir():
        return cwd.parent
    return cwd


def make_output_path(source: Path, lang_dir: Path, output_dir: Path) -> Path:
    """Compute the output Markdown path for a given .coal source file.
    
    Mirrors the lang/ directory structure under output_dir.
    E.g., lang/Coal/Combinators.coal → output_dir/Coal/Combinators.md
    """
    try:
        rel = source.relative_to(lang_dir)
    except ValueError:
        rel = Path(source.name)
    return (output_dir / str(rel)).with_suffix(".md")


def count_braces(text: str) -> int:
    """
    Count { as +1 and } as -1 on a line, ignoring comments and string literals.
    Returns the net brace depth for the line.
    """
    depth = 0
    in_block = False
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if in_block:
            if ch == '*' and i + 1 < n and text[i + 1] == '/':
                in_block = False
                i += 2
            else:
                i += 1
        elif ch == '/' and i + 1 < n:
            nxt = text[i + 1]
            if nxt == '/':
                break  # rest of line is a comment
            elif nxt == '*':
                in_block = True
                i += 2
            else:
                i += 1
        elif ch == '"':
            i += 1
            while i < n:
                if text[i] == '\\' and i + 1 < n:
                    i += 2
                elif text[i] == '"':
                    i += 1
                    break
                else:
                    i += 1
        elif ch == '{':
            depth += 1
            i += 1
        elif ch == '}':
            depth -= 1
            i += 1
        else:
            i += 1
    return depth


def get_top_level_indent(text: str) -> Optional[int]:
    """
    Determine the indentation (in spaces) used for top-level definitions
    inside the module { … } block.  Returns None if we can't determine it.
    """
    lines = text.split("\n")
    in_module_body = False
    depth = 0
    for raw in lines:
        stripped = raw.strip()

        # Track brace depth, ignoring comments/strings for simplicity here.
        # The module declaration is the only thing we care about.
        depth += count_braces(raw)

        # If we just entered the module body (depth == 1 after seeing the opening brace),
        # find the next non-empty, non-comment line to sample its indent.
        if not in_module_body and depth >= 1:
            # The module opening brace was on this (or a previous) line.
            # Look at the *next* lines for the first definition.
            in_module_body = True
            continue

        if in_module_body and depth == 1 and stripped:
            # Only skip blank, comment, import, module lines
            if (stripped.startswith("//")
                    or stripped.startswith("/*")
                    or stripped.startswith("import")
                    or stripped.startswith("*")
                    or stripped.startswith("module ")):
                continue
            # Compute leading whitespace
            indent = len(raw) - len(raw.lstrip())
            return indent

        # If we've left the module body, stop
        if depth < 1 and in_module_body:
            break

    return None


def parse_coal_file(filepath: Path) -> list[dict]:
    """
    Parse a .coal file and extract docblock-definition pairs.

    Returns a list of dicts with keys:
        - doc: the docblock text (without /** */ delimiters), or
               "No documentation available."
        - kind: the definition kind (fun, let, type, type alias, trait, instance, fold)
        - name: the definition name
        - signature: the full definition line(s)
    """
    with open(filepath, "r", encoding="utf-8") as f:
        text = f.read()

    entries = []
    lines = text.split("\n")
    n = len(lines)

    # Determine the top-level indentation so Pass 2 can skip nested defs
    top_level_indent = get_top_level_indent(text)

    # ── Pass 1: extract docblock-definition pairs ──────────────────────
    # Track which line numbers are consumed by docblock definitions so that
    # Pass 2 can skip over them.
    consumed_lines: set[int] = set()

    i = 0
    while i < n:
        line = lines[i]

        # Look for opening of a docblock /**
        if re.match(r'^\s*/\*\*', line):
            doc_lines = []
            # Check if the docblock ends on the same line
            end_match = re.search(r'\*/\s*$', line)
            if end_match:
                # Single-line docblock: extract content between /** and */
                content = re.sub(r'^\s*/\*\*\s*', '', line)
                content = re.sub(r'\s*\*/\s*$', '', content)
                doc_lines = [content]
                i += 1
            else:
                # Multi-line docblock
                first = re.sub(r'^\s*/\*\*\s*', '', line)
                doc_lines.append(first)
                i += 1
                while i < n:
                    close_match = re.search(r'\*/\s*$', lines[i])
                    if close_match:
                        rest = re.sub(r'\s*\*/\s*$', '', lines[i])
                        rest = re.sub(r'^\s*\*\s?', '', rest)
                        doc_lines.append(rest)
                        i += 1
                        break
                    else:
                        content = re.sub(r'^\s*\*\s?', '', lines[i])
                        doc_lines.append(content)
                        i += 1

            # Clean up doc lines - strip leading whitespace but preserve blank lines
            # (needed for Markdown tables and paragraph separation)
            doc_lines = [re.sub(r'^\s+', '', l) for l in doc_lines]
            while doc_lines and not doc_lines[0].strip():
                doc_lines.pop(0)
            while doc_lines and not doc_lines[-1].strip():
                doc_lines.pop()
            doc_text = "\n".join(doc_lines).strip()

            # Skip whitespace/comments/imports to find the associated definition
            while i < n:
                line = lines[i].strip()
                if line == "" or line.startswith("//") or line.startswith("/*") or line.startswith("import") or line.startswith("*"):
                    i += 1
                else:
                    break

            if i < n:
                def_line = lines[i].strip()
                parsed = parse_definition(def_line)
                if parsed:
                    kind, name = parsed
                    if is_branched_definition(def_line):
                        start = i
                        sig_lines = [def_line]
                        i += 1
                        while i < n:
                            l = lines[i].strip()
                            if l == "" or l.startswith("//") or l.startswith("import") or l.startswith("/*"):
                                break
                            if l.startswith("fun ") or l.startswith("let ") or l.startswith("type ") or l.startswith("trait ") or l.startswith("instance ") or l.startswith("fold "):
                                break
                            sig_lines.append(l)
                            i += 1
                        signature = "\n".join(sig_lines)
                        for j in range(start, i):
                            consumed_lines.add(j)
                        entries.append({
                            "doc": doc_text,
                            "kind": kind,
                            "name": name,
                            "signature": signature,
                        })
                    else:
                        consumed_lines.add(i)
                        entries.append({
                            "doc": doc_text,
                            "kind": kind,
                            "name": name,
                            "signature": def_line,
                        })
                        i += 1
                else:
                    i += 1
            else:
                # Docblock with no following definition – still advance
                pass
        else:
            i += 1

    # ── Pass 2: collect undocumented top-level definitions ─────────────
    # Walk through the file again and pick up any top-level definitions
    # that were not already paired with a docblock.
    # We track brace depth and indentation to avoid capturing nested
    # definitions (e.g. inside instance/trait blocks, or nested lets
    # inside function bodies).
    i = 0
    brace_depth = 0
    while i < n:
        line = lines[i]
        stripped = line.strip()

        # Track brace depth changes on this line
        brace_depth += count_braces(line)

        # Skip blank lines, comments, imports, module headers
        if (stripped == ""
                or stripped.startswith("//")
                or stripped.startswith("/*")
                or stripped.startswith("import")
                or stripped.startswith("*")
                or stripped.startswith("module ")):
            i += 1
            continue

        # Skip lines already consumed by a docblock in Pass 1
        if i in consumed_lines:
            i += 1
            continue

        # Only consider definitions at brace depth 1 (inside module { … })
        if brace_depth != 1:
            i += 1
            continue

        # Only consider definitions at the top-level indentation (skip
        # nested definitions like let-bindings inside function bodies).
        if top_level_indent is not None:
            actual_indent = len(line) - len(line.lstrip())
            if actual_indent > top_level_indent:
                i += 1
                continue

        parsed = parse_definition(stripped)
        if parsed:
            kind, name = parsed
            if is_branched_definition(stripped):
                start = i
                sig_lines = [stripped]
                i += 1
                while i < n:
                    l = lines[i].strip()
                    if l == "" or l.startswith("//") or l.startswith("import") or l.startswith("/*"):
                        break
                    if l.startswith("fun ") or l.startswith("let ") or l.startswith("type ") or l.startswith("trait ") or l.startswith("instance ") or l.startswith("fold "):
                        break
                    sig_lines.append(l)
                    i += 1
                signature = "\n".join(sig_lines)
                for j in range(start, i):
                    consumed_lines.add(j)
                entries.append({
                    "doc": "No documentation available.",
                    "kind": kind,
                    "name": name,
                    "signature": signature,
                })
            else:
                consumed_lines.add(i)
                entries.append({
                    "doc": "No documentation available.",
                    "kind": kind,
                    "name": name,
                    "signature": stripped,
                })
                i += 1
        else:
            i += 1

    return entries


def is_branched_definition(line: str) -> bool:
    """Check if a definition line starts a branched definition (fun with | branches)."""
    stripped = line.strip()
    if stripped.startswith("fun "):
        return "|" in stripped and not stripped.rstrip().endswith("=")
    return False


def parse_definition(line: str) -> Optional[tuple[str, str]]:
    """
    Parse a definition line to extract kind and name.

    Returns (kind, name) or None if not a definition.
    """
    line = line.strip()
    
    # fun name(...)
    m = re.match(r'^fun\s+([a-zA-Z_][a-zA-Z0-9_]*)\b', line)
    if m:
        return ("fun", m.group(1))
    
    # fun `backtick-name`(...)
    m = re.match(r'^fun\s+`([^`]+)`', line)
    if m:
        return ("fun", m.group(1))

    # let name = ...
    m = re.match(r'^let\s+([a-zA-Z_][a-zA-Z0-9_]*)\b', line)
    if m:
        return ("let", m.group(1))

    # type alias Name<...>
    m = re.match(r'^type\s+alias\s+([A-Z][a-zA-Z0-9_]*)', line)
    if m:
        return ("type alias", m.group(1))

    # type Name<...>
    m = re.match(r'^type\s+([A-Z][a-zA-Z0-9_]*)', line)
    if m:
        return ("type", m.group(1))

    # trait Name<...>
    m = re.match(r'^trait\s+([A-Z][a-zA-Z0-9_]*)', line)
    if m:
        return ("trait", m.group(1))

    # instance Name<...>
    m = re.match(r'^instance\s+([A-Z][a-zA-Z0-9_]*)', line)
    if m:
        return ("instance", m.group(1))

    # fold name(...)
    m = re.match(r'^fold\s+([a-zA-Z_][a-zA-Z0-9_]*)', line)
    if m:
        return ("fold", m.group(1))

    return None


def extract_module_name(filepath: Path) -> str:
    """Extract the module name from a .coal file."""
    with open(filepath, "r", encoding="utf-8") as f:
        text = f.read()
    m = re.search(r'^module\s+([a-zA-Z.]+)', text, re.MULTILINE)
    if m:
        return m.group(1)
    return filepath.stem


def extract_module_docblock(filepath: Path) -> Optional[str]:
    """
    Extract a module-level docblock from a .coal file.

    A module-level docblock is a /** ... */ comment that appears before the
    module declaration (the ``module Name {`` line).  Returns the docblock
    text (without the /** */ delimiters and leading ``*`` markers), or None if
    no such docblock exists.
    """
    with open(filepath, "r", encoding="utf-8") as f:
        text = f.read()

    m = re.search(r'^\s*/\*\*(.*?)\*/\s*^module ', text, re.MULTILINE | re.DOTALL)
    if not m:
        return None

    raw = m.group(1)
    # Strip the leading * from each line, clean up whitespace
    lines = []
    for line in raw.split("\n"):
        cleaned = re.sub(r'^\s*\*\s?', '', line).strip()
        if cleaned:
            lines.append(cleaned)
    return " ".join(lines) if lines else None


def docblock_to_markdown(entry: dict) -> str:
    """Convert a docblock entry to Markdown."""
    kind = entry["kind"]
    name = entry["name"]
    doc = entry["doc"]

    lines = []
    
    # Section header with function name
    lines.append(f"### `{name}`")
    lines.append("")
    
    # Add a badge for type and trait definitions
    if kind in ("type", "type alias", "trait"):
        badge_label = "trait" if kind == "trait" else "type"
        lines.append(f'<span class="badge badge-primary">{badge_label}</span>')
        lines.append("")
    
    # Check for **Type**: in the docblock - extract it separately
    type_match = re.search(r'\*\*Type\*\*\s*:\s*(.+)', doc)
    if type_match:
        type_sig = type_match.group(1).strip()
        doc_without_type = re.sub(r'\n?\*\*Type\*\*\s*:\s*.+', '', doc).strip()
        description = doc_without_type.strip()
    else:
        description = doc.strip()
        type_sig = None

    # Description paragraph
    if description:
        lines.append(description)
        lines.append("")

    # Code block with type signature (strip any backticks from the type line)
    if type_sig:
        clean_sig = type_sig.strip("`")
        lines.append("```coal")
        lines.append(clean_sig)
        lines.append("```")
        lines.append("")

    return "\n".join(lines)


def generate_module_doc(filepath: Path) -> str:
    """Generate full Markdown documentation for a .coal file."""
    module_name = extract_module_name(filepath)
    entries = parse_coal_file(filepath)

    lines = []
    lines.append(f"# `{module_name}`")
    lines.append("")

    # Include module-level docblock, if present, as an introductory paragraph
    module_doc = extract_module_docblock(filepath)
    if module_doc:
        lines.append(module_doc)
        lines.append("")

    if not entries:
        lines.append("*No documented definitions.*")
        lines.append("")
        return "\n".join(lines)

    # Add a divider after the intro section
    lines.append("---")
    lines.append("")

    # Join entries with a divider between them (but not after the last one)
    entry_mds = [docblock_to_markdown(entry) for entry in entries]
    lines.append("\n---\n\n".join(entry_mds))

    return "\n".join(lines)


def generate_index(files: list[Path], lang_dir: Path) -> str:
    """Generate an index Markdown file listing all documented modules."""
    lines = []
    lines.append("# Standard library")
    lines.append("")
    lines.append("This documentation is automatically generated from the source code.")
    lines.append("")
    
    for f in sorted(files, key=lambda p: str(p)):
        module_name = extract_module_name(f)
        try:
            rel = f.relative_to(lang_dir)
        except ValueError:
            rel = Path(f.name)
        md_path = str(rel.with_suffix(".md"))
        lines.append(f"- [`{module_name}`]({md_path})")

    lines.append("")
    return "\n".join(lines)


def heading_to_anchor(text: str) -> str:
    """
    Convert a heading title (e.g. `Coal.Applicative`) into a GitHub-style
    anchor fragment suitable for intra-page links.
    """
    # Remove backticks, lowercase, strip non-alphanumeric characters
    # (matching GitHub's anchor generation: dots and other punctuation
    #  are removed, not replaced with hyphens).
    anchor = text.lower()
    anchor = anchor.replace("`", "")
    # Remove any character that isn't alphanumeric, space, or hyphen
    anchor = re.sub(r'[^a-z0-9 -]', '', anchor)
    # Replace spaces with hyphens
    anchor = anchor.replace(' ', '-')
    # Collapse multiple hyphens
    anchor = re.sub(r'-+', '-', anchor)
    anchor = anchor.strip('-')
    return f"#{anchor}"


def generate_combined_doc(files: list[Path], lang_dir: Path) -> str:
    """
    Generate a single Markdown page that combines all module documentation.
    Module titles are demoted from # to ## to avoid multiple H1 headers.
    Modules are sorted lexicographically by module path.
    Includes a table of contents with links to each module section.
    """
    modules = []
    for f in sorted(files, key=lambda p: str(p)):
        try:
            md = generate_module_doc(f)
            module_name = extract_module_name(f)
            # Demote the module title from # to ##
            md = re.sub(r'^# ', '## ', md, count=1)
            modules.append((module_name, md))
        except Exception as e:
            print(f"  ✗ (skipping combined) {f}: {e}", file=sys.stderr)

    lines = []
    lines.append("# Standard library")
    lines.append("")
    lines.append("This documentation is automatically generated from the source code.")
    lines.append("")

    # ── Table of contents ──────────────────────────────────────────────
    for module_name, _ in modules:
        anchor = heading_to_anchor(module_name)
        lines.append(f"- [`{module_name}`]({anchor})")
    lines.append("")
    lines.append("---")
    lines.append("")

    # ── Module sections ────────────────────────────────────────────────
    for module_name, md_content in modules:
        lines.append(md_content)

    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser(
        description="Generate Markdown documentation from Coal standard library docblocks."
    )
    parser.add_argument(
        "files",
        nargs="*",
        help="Coal source files to process (default: all files under lang/)",
    )
    parser.add_argument(
        "--output-dir",
        default="docs/generated",
        help="Output directory for generated Markdown files (default: docs/generated)",
    )
    parser.add_argument(
        "--index",
        action="store_true",
        help="Generate an index file listing all documented modules",
    )
    parser.add_argument(
        "--combine",
        action="store_true",
        help="Generate a combined index.md with all module documentation in a single page",
    )
    args = parser.parse_args()

    project_root = find_project_root()
    lang_dir = project_root / "lang"

    if not lang_dir.is_dir():
        print(f"Error: lang/ directory not found at {project_root}", file=sys.stderr)
        sys.exit(1)

    if args.files:
        coal_files = [Path(f) for f in args.files if Path(f).suffix == ".coal"]
    else:
        coal_files = list(lang_dir.rglob("*.coal"))

    if not coal_files:
        print("No .coal files found.", file=sys.stderr)
        sys.exit(0)

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    generated = 0
    for f in sorted(coal_files, key=lambda p: str(p)):
        try:
            md_content = generate_module_doc(f)
            md_path = make_output_path(f, lang_dir, output_dir)
            md_path.parent.mkdir(parents=True, exist_ok=True)
            with open(md_path, "w", encoding="utf-8") as out:
                out.write(md_content)
            print(f"  ✓ {f} → {md_path}")
            generated += 1
        except Exception as e:
            print(f"  ✗ {f}: {e}", file=sys.stderr)

    if args.index:
        index_path = output_dir / "README.md"
        index_content = generate_index(coal_files, lang_dir)
        with open(index_path, "w", encoding="utf-8") as out:
            out.write(index_content)
        print(f"  ✓ Index → {index_path}")

    if args.combine:
        combine_path = output_dir / "index.md"
        combine_content = generate_combined_doc(coal_files, lang_dir)
        with open(combine_path, "w", encoding="utf-8") as out:
            out.write(combine_content)
        print(f"  ✓ Combined → {combine_path}")

    print(f"\nGenerated {generated} documentation files in {output_dir}/")


if __name__ == "__main__":
    main()