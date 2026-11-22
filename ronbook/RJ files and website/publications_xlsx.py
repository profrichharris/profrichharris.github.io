#!/usr/bin/env python3
"""Round-trip the publications list between the website and Excel.

    python3 publications_xlsx.py export [file.xlsx]
        Writes every record in ../publications_data.js to an editable workbook.

    python3 publications_xlsx.py import file.xlsx            (preview only)
    python3 publications_xlsx.py import file.xlsx --apply    (writes the data file)
        Reads the edited workbook back and updates ../publications_data.js.

The workbook's "Key" column (the site's internal record number, `o`) is how
rows are matched back to records, so it must not be changed on existing rows.
Rows with a blank Key are treated as new records. Only cells that were
actually edited are written back, so untouched records stay byte-identical.

New records left without a Theme or Keywords are auto-tagged on import (see
autotag.py; skip with --no-autotag). `autotag` tags previously added records
that still have none.

The data file is under git: commit before --apply (or just `git diff` after) to
review or undo an import.
"""
import argparse
import json
import re
import sys
from datetime import date
from pathlib import Path

from openpyxl import Workbook, load_workbook
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.datavalidation import DataValidation

HERE = Path(__file__).resolve().parent
DATA_JS = HERE.parent / "publications_data.js"
DEFAULT_XLSX = HERE / "Ron Johnston Publications - editable.xlsx"
PREFIX = "const PUBLICATIONS = "

THEMES = [
    "Electoral and political geography",
    "History of human geography",
    "Urban social geography",
    "Methods and methodology",
    "Regional geography, population and migration",
    "Higher education policy and research assessment",
    "Reflections and commentaries",
]
TYPES = [
    "Journal article", "Book", "Chapter in book", "Obituaries and Memoirs",
    "Review Articles", "Reviews", "Other publication",
    "Dictionary of Human Geography",
]

# (sheet header, record field, column width)
COLUMNS = [
    ("Key", "o", 7),
    ("#", "id", 9),
    ("Source", "source", 9),
    ("Year", "year", 7),
    ("Type", "type", 22),
    ("Citation", "citation", 90),
    ("Link", "link", 38),
    ("Co-authors", "coauthors", 28),
    ("Theme", "theme", 34),
    ("Keywords", "keywords", 28),
    ("Abstract", "abstract", 40),
    ("Synopsis", "synopsis", 40),
    ("Entries list", "list", 30),
    ("Partial list", "listPartial", 9),
    ("Delete?", "delete", 9),
]
HEADERS = [h for h, _, _ in COLUMNS]
FIELD_OF = {h: f for h, f, _ in COLUMNS}
FONT = "Arial"


# ---------------------------------------------------------------- data file
def load_records(path):
    text = Path(path).read_text(encoding="utf-8")
    return json.loads(text[len(PREFIX):].rstrip(";\n"))


def save_records(path, recs):
    js = PREFIX + json.dumps(recs, ensure_ascii=False, separators=(",", ":")) + ";"
    Path(path).write_text(js, encoding="utf-8")


# ------------------------------------------------------------ cell <-> field
def to_cell(rec, field):
    """The text/number a record's field shows as in the spreadsheet."""
    if field == "o":
        return rec["o"]
    if field == "id":
        return "" if rec.get("id") is None else str(rec["id"])
    if field == "year":
        return "" if rec.get("year") is None else rec["year"]
    if field in ("coauthors", "keywords"):
        return "; ".join(rec.get(field) or [])
    if field == "theme":
        return rec["themes"][0][0] if rec.get("themes") else ""
    if field == "listPartial":
        return "Y" if rec.get("listPartial") else ""
    if field == "delete":
        return ""
    return rec.get(field) or ""


def cell_text(v):
    """A cell value as comparable, whitespace-trimmed text."""
    if v is None:
        return ""
    if isinstance(v, float) and v.is_integer():
        v = int(v)
    return str(v).strip()


def split_list(text):
    return [p.strip() for p in text.split(";") if p.strip()]


def letter_code(n):
    """0->A ... 25->Z, 26->AA ... (the site's A, B, C... AA, AB lettering)."""
    s = ""
    n += 1
    while n:
        n, r = divmod(n - 1, 26)
        s = chr(65 + r) + s
    return s


def letter_index(code):
    n = 0
    for ch in code:
        n = n * 26 + (ord(ch) - 64)
    return n - 1


def next_letter_code(recs):
    used = [letter_index(r["id"]) for r in recs
            if isinstance(r.get("id"), str) and re.fullmatch(r"[A-Z]+", r["id"])]
    return letter_code(max(used) + 1 if used else 0)


# -------------------------------------------------------------------- export
def style_header(ws):
    fill = PatternFill("solid", fgColor="DDDDDD")
    for c in ws[1]:
        c.font = Font(name=FONT, bold=True, size=10)
        c.fill = fill
        c.alignment = Alignment(vertical="center", wrap_text=True)


def build_instructions(ws, n_total, n_archive, n_cv):
    ws.column_dimensions["A"].width = 24
    ws.column_dimensions["B"].width = 110
    rows = [
        ("Ron Johnston publications - editable workbook", None),
        (f"Exported {date.today():%d %B %Y}: {n_total:,} records "
         f"({n_archive:,} from Ron's archive, {n_cv:,} from the 2013 CV).", None),
        ("", None),
        ("HOW IT WORKS", None),
        ("1. Edit", "Correct any cell on the 'Publications' sheet, or add new records in new rows at the bottom. "
                    "Don't re-order the columns or rename the sheets."),
        ("2. Keep the Key", "The grey Key column is how each row is matched back to the website. Never change or "
                            "re-use it on an existing row. Leave it BLANK on a new row - one is assigned on import."),
        ("3. Deleting", "Type DELETE in the 'Delete?' column to remove a record. (A row that is simply "
                        "removed from the sheet is kept on the site, unless the import is run with --drop-missing.)"),
        ("4. Save", "Save as .xlsx (not .csv) so accents and line breaks survive."),
        ("5. Bring it back", "Run  python3 publications_xlsx.py import <file.xlsx>  for a preview of every change, "
                             "then add --apply to update publications_data.js. (Or ask Claude to do it.) "
                             "Afterwards the page's headline counts and 'Last updated' date need refreshing."),
        ("", None),
        ("COLUMN GUIDE", None),
        ("#", "The number shown on the site: Ron's own number for archive records, a letter code (A, B ... AA, AB ...) "
              "for CV records. Stored as text. Blank on a new row = the next free letter code."),
        ("Source", "archive or CV. Blank on a new row = 'added'."),
        ("Year", "Four-digit year; blank = undated (shown 'n.d.', sorted last)."),
        ("Type", "Pick from the drop-down. A type not in the list will be accepted, but appears as a new filter option."),
        ("Citation", "The full reference as shown on the site, including any '(with ...)' co-author prefix."),
        ("Link", "DOI or other persistent link (full https:// address)."),
        ("Co-authors", "Separate with semicolons, e.g.  C Pattie; D Rossiter . Used for the collaborator filter. "
                       "If left blank on a new row that starts '(with ...)', it is filled in from the citation."),
        ("Theme", "One theme, from the drop-down, or blank for none. A theme you change is stored as high-confidence. "
                  "Leave Theme/Keywords blank on a new row and the import will auto-tag it (check the preview)."),
        ("Keywords", "Separate with semicolons. The site's existing vocabulary is short (Britain; segregation; vote ...) "
                     "so reuse existing terms where you can."),
        ("Abstract / Synopsis /\nEntries list", "Optional text behind the [abstract], [synopsis] and [list] pop-ups."),
        ("Partial list", "Y if the entries list is only partial."),
        ("", None),
        ("EXAMPLE NEW ROW", "(for illustration only - not part of the data)"),
        ("Key", "(blank)"),
        ("#", "(blank)"),
        ("Year", "2021"),
        ("Type", "Journal article"),
        ("Citation", "(with C Pattie) An example article title. Example Journal, 12(3), 45-67."),
        ("Link", "https://doi.org/10.0000/example"),
        ("Co-authors", "C Pattie"),
        ("Theme", "Electoral and political geography"),
        ("Keywords", "elections; voting"),
    ]
    for r, (a, b) in enumerate(rows, 1):
        ca = ws.cell(r, 1, a)
        ca.font = Font(name=FONT, size=10, bold=bool(b is None and a.isupper() or r == 1))
        ca.alignment = Alignment(vertical="top", wrap_text=True)
        if b is not None:
            cb = ws.cell(r, 2, b)
            cb.font = Font(name=FONT, size=10)
            cb.alignment = Alignment(vertical="top", wrap_text=True)
    ws["A1"].font = Font(name=FONT, size=14, bold=True)
    for r in range(len(rows) - 9, len(rows) + 1):  # example block
        ws.cell(r, 1).fill = PatternFill("solid", fgColor="FFF2CC")
        ws.cell(r, 2).fill = PatternFill("solid", fgColor="FFF2CC")


def cmd_export(args):
    recs = load_records(args.data)
    out = Path(args.xlsx or DEFAULT_XLSX)
    wb = Workbook()
    ws = wb.active
    ws.title = "Publications"
    ws.append(HEADERS)
    style_header(ws)

    key_fill = PatternFill("solid", fgColor="EEEEEE")
    for rec in recs:
        row = []
        for _, field, _ in COLUMNS:
            row.append(to_cell(rec, field))
        ws.append(row)
    n = len(recs)
    for r in range(2, n + 2):
        for ci, (h, field, _) in enumerate(COLUMNS, 1):
            c = ws.cell(r, ci)
            c.font = Font(name=FONT, size=10)
            c.alignment = Alignment(vertical="top", wrap_text=(field == "citation"))
            if field in ("id", "year", "source"):
                c.number_format = "@"
            if isinstance(c.value, str) and c.value.startswith("="):
                c.data_type = "s"  # never let a citation be read as a formula
            if field == "o":
                c.fill = key_fill
            if field == "year" and c.value != "":
                c.number_format = "0"
    # '#' and Source are text; year numeric
    for r in range(2, n + 2):
        ws.cell(r, 4).number_format = "0"

    for ci, (_, _, w) in enumerate(COLUMNS, 1):
        ws.column_dimensions[get_column_letter(ci)].width = w
    ws.freeze_panes = "C2"
    ws.auto_filter.ref = f"A1:{get_column_letter(len(COLUMNS))}{n + 1}"

    # Drop-downs (warning-only so a new type/theme can still be typed).
    lists = wb.create_sheet("Lists")
    lists["A1"], lists["B1"] = "Types", "Themes"
    for i, t in enumerate(TYPES, 2):
        lists.cell(i, 1, t)
    for i, t in enumerate(THEMES, 2):
        lists.cell(i, 2, t)
    for c in (lists["A1"], lists["B1"]):
        c.font = Font(name=FONT, bold=True)
    lists.column_dimensions["A"].width = 32
    lists.column_dimensions["B"].width = 48
    last = n + 1 + 500  # room for new rows
    for col, ref, msg in (
        ("E", f"=Lists!$A$2:$A${len(TYPES) + 1}", "That type isn't in the list - it will be added as a new type."),
        ("I", f"=Lists!$B$2:$B${len(THEMES) + 1}", "That theme isn't in the list - it will be added as a new theme."),
    ):
        dv = DataValidation(type="list", formula1=ref, allow_blank=True,
                            errorStyle="warning", error=msg, errorTitle="Not in list")
        dv.add(f"{col}2:{col}{last}")
        ws.add_data_validation(dv)
    dv = DataValidation(type="list", formula1='"DELETE"', allow_blank=True,
                        errorStyle="stop", error="Type DELETE or leave blank.")
    dv.add(f"O2:O{last}")
    ws.add_data_validation(dv)
    dv = DataValidation(type="list", formula1='"Y"', allow_blank=True,
                        errorStyle="stop", error="Type Y or leave blank.")
    dv.add(f"N2:N{last}")
    ws.add_data_validation(dv)

    ins = wb.create_sheet("Instructions", 0)
    n_arch = sum(1 for r in recs if r.get("source") == "archive")
    build_instructions(ins, n, n_arch, sum(1 for r in recs if r.get("source") == "CV"))
    wb.active = 0  # open on the Instructions sheet
    wb.save(out)
    print(f"Wrote {n:,} records to {out}")


# -------------------------------------------------------------------- import
def parse_year(text):
    if text == "":
        return None
    m = re.fullmatch(r"(\d{4})(?:\.0)?", text)
    if not m:
        raise ValueError(f"year '{text}' is not a four-digit year")
    return int(m.group(1))


def parse_id(text):
    if text == "":
        return None
    return int(text) if re.fullmatch(r"\d+", text) else text


def apply_field(rec, field, text, errors, where):
    """Set one record field from edited cell text."""
    if field == "id":
        rec["id"] = parse_id(text)
    elif field == "year":
        try:
            rec["year"] = parse_year(text)
        except ValueError as e:
            errors.append(f"{where}: {e}")
    elif field in ("coauthors", "keywords"):
        rec[field] = split_list(text)
    elif field == "theme":
        old = rec["themes"][0] if rec.get("themes") else None
        if text == "":
            rec["themes"] = []
        elif old and old[0] == text:
            pass
        else:
            rec["themes"] = [[text, 1.0]]
    elif field == "listPartial":
        if text.lower() in ("y", "yes", "true", "1"):
            rec["listPartial"] = True
        else:
            rec.pop("listPartial", None)
    elif field == "link":
        rec["link"] = text or None
    elif field in ("abstract", "synopsis", "list"):
        if text:
            rec[field] = text
        else:
            rec.pop(field, None)
    else:  # source, type, citation
        rec[field] = text


def get_tagger(recs, warnings):
    """Build the auto-tagger (needs scikit-learn); None if unavailable."""
    try:
        sys.path.insert(0, str(HERE))
        import autotag
        return autotag.default_tagger(recs)
    except ImportError as e:
        warnings.append(f"Auto-tagging skipped ({e}); new records are untagged")
        return None


def describe_tags(rec):
    th = f"{rec['themes'][0][0]} ({rec['themes'][0][1]:.2f})" if rec["themes"] else "no theme"
    kw = ", ".join(rec["keywords"]) if rec["keywords"] else "no keywords"
    return f"{th}; {kw}"


def short(v, n=70):
    s = str(v).replace("\n", " ⏎ ")
    return s if len(s) <= n else s[: n - 1] + "…"


def cmd_import(args):
    recs = load_records(args.data)
    by_o = {r["o"]: r for r in recs}
    wb = load_workbook(args.xlsx, data_only=True)
    if "Publications" not in wb.sheetnames:
        sys.exit("No 'Publications' sheet in that workbook.")
    ws = wb["Publications"]
    header = [cell_text(c.value) for c in ws[1]][: len(HEADERS)]
    if header != HEADERS:
        sys.exit(f"Header row has changed.\n  expected: {HEADERS}\n  found:    {header}")

    errors, warnings = [], []
    changed, new_rows, deletes = [], [], []
    seen = set()
    next_o = max(by_o) + 1
    auto_letter = next_letter_code(recs)
    auto_letter_n = letter_index(auto_letter)

    for r in range(2, ws.max_row + 1):
        vals = {h: ws.cell(r, i).value for i, h in enumerate(HEADERS, 1)}
        if all(cell_text(v) == "" for v in vals.values()):
            continue
        where = f"row {r}"
        key_text = cell_text(vals["Key"])
        action = cell_text(vals["Delete?"]).upper()

        if key_text:
            if not re.fullmatch(r"\d+", key_text) or int(key_text) not in by_o:
                errors.append(f"{where}: Key '{key_text}' doesn't match any existing record")
                continue
            o = int(key_text)
            if o in seen:
                errors.append(f"{where}: Key {o} appears more than once")
                continue
            seen.add(o)
            rec = by_o[o]
            if action == "DELETE":
                deletes.append(o)
                continue
            if action:
                warnings.append(f"{where}: unrecognised Delete? value '{action}' ignored")
            diffs = []
            for h, field, _ in COLUMNS:
                if field in ("o", "delete"):
                    continue
                new_t = cell_text(vals[h])
                old_t = cell_text(to_cell(rec, field))
                if new_t != old_t:
                    diffs.append((h, old_t, new_t))
            if diffs:
                new_rec = json.loads(json.dumps(rec))
                for h, _, new_t in diffs:
                    apply_field(new_rec, FIELD_OF[h], new_t, errors, f"{where} (Key {o})")
                if not new_rec.get("citation"):
                    errors.append(f"{where} (Key {o}): Citation is empty")
                changed.append((o, rec, new_rec, diffs))
        else:
            if action == "DELETE":
                continue  # a new row marked for deletion: nothing to do
            citation = cell_text(vals["Citation"])
            typ = cell_text(vals["Type"])
            if not citation:
                errors.append(f"{where}: new row has no Citation")
                continue
            if not typ:
                errors.append(f"{where}: new row has no Type")
                continue
            rec = {"o": next_o, "id": None, "year": None, "citation": citation, "link": None,
                   "coauthors": [], "type": typ, "themes": [], "keywords": [], "source": "added"}
            for h, field, _ in COLUMNS:
                if field in ("o", "delete", "citation", "type"):
                    continue
                t = cell_text(vals[h])
                if t:
                    apply_field(rec, field, t, errors, where)
            if rec["id"] is None:
                rec["id"] = letter_code(auto_letter_n)
                auto_letter_n += 1
            if not rec["coauthors"]:
                m = re.match(r"\(with ([^)]*)\)", citation, re.I)
                if m:
                    rec["coauthors"] = [n.strip() for n in re.split(r",| and ", m.group(1)) if n.strip()]
                    warnings.append(f"{where}: Co-authors derived from the citation ({'; '.join(rec['coauthors'])}) - please check")
            if rec["id"] in {r_["id"] for r_ in recs} | {n_["id"] for n_ in new_rows}:
                warnings.append(f"{where}: # '{rec['id']}' is already used by another record")
            next_o += 1
            new_rows.append(rec)

    # auto-tag new rows whose Theme / Keywords were left blank
    auto_tagged = {}
    if new_rows and not args.no_autotag:
        tagger = get_tagger(recs, warnings)
        if tagger:
            for rec in new_rows:
                auto_theme, auto_kw = not rec["themes"], not rec["keywords"]
                if auto_theme:
                    rec["themes"] = tagger.theme(rec)
                if auto_kw:
                    rec["keywords"] = tagger.keywords(rec)
                auto_tagged[rec["o"]] = (auto_theme, auto_kw)

    # type / theme sanity
    known_types = {r["type"] for r in recs}
    for o, _, new_rec, _ in changed:
        if new_rec["type"] not in known_types:
            warnings.append(f"Key {o}: new type '{new_rec['type']}' (will appear as a new filter option)")
        for t in new_rec.get("themes", []):
            if t[0] not in THEMES:
                warnings.append(f"Key {o}: theme '{t[0]}' is not one of the seven site themes")
    for rec in new_rows:
        if rec["type"] not in known_types:
            warnings.append(f"New record #{rec['id']}: new type '{rec['type']}'")
        for t in rec.get("themes", []):
            if t[0] not in THEMES:
                warnings.append(f"New record #{rec['id']}: theme '{t[0]}' is not one of the seven site themes")

    missing = [o for o in by_o if o not in seen and o not in deletes]
    if missing and not args.drop_missing:
        warnings.append(f"{len(missing)} existing record(s) are absent from the sheet and will be KEPT "
                        f"(use --drop-missing to remove them)")
    drops = missing if args.drop_missing else []

    # ---- report
    print(f"Workbook: {args.xlsx}")
    print(f"Records on site now: {len(recs):,}\n")
    limit = None if args.verbose else 40
    print(f"EDITED   {len(changed)} record(s)")
    for o, rec, _, diffs in changed[:limit]:
        print(f"  Key {o} (#{rec['id']}):")
        for h, old, new in diffs:
            print(f"     {h}: {short(old)!r}  ->  {short(new)!r}")
    if limit and len(changed) > limit:
        print(f"  ... and {len(changed) - limit} more (use --verbose to list all)")
    print(f"\nNEW      {len(new_rows)} record(s)")
    for rec in new_rows[:limit]:
        print(f"  Key {rec['o']} (#{rec['id']}, {rec['year'] or 'n.d.'}): {short(rec['citation'])}")
        at = auto_tagged.get(rec["o"])
        label = "auto-tagged" if at and any(at) else "tags"
        print(f"      {label}: {describe_tags(rec)}")
    print(f"\nDELETE   {len(deletes) + len(drops)} record(s)")
    for o in (deletes + drops)[:limit]:
        print(f"  Key {o} (#{by_o[o]['id']}): {short(by_o[o]['citation'])}")
    if warnings:
        print("\nWARNINGS")
        for w in warnings:
            print("  -", w)
    if errors:
        print("\nERRORS (nothing can be applied until these are fixed)")
        for e in errors:
            print("  -", e)

    # ---- apply
    final_n = len(recs) - len(deletes) - len(drops) + len(new_rows)
    print(f"\nRecords after import: {final_n:,}")
    if not (changed or new_rows or deletes or drops):
        print("No changes.")
        return
    if errors:
        sys.exit(1)
    if not args.apply:
        print("\nPreview only - re-run with --apply to write the changes.")
        return

    gone = set(deletes) | set(drops)
    edited = {o: new for o, _, new, _ in changed}
    out = []
    for rec in recs:
        if rec["o"] in gone:
            continue
        out.append(edited.get(rec["o"], rec))
    out.extend(new_rows)
    out.sort(key=lambda r: r["o"])
    dest = args.out or args.data
    save_records(dest, out)
    n_arch = sum(1 for r in out if r.get("source") == "archive")
    n_cv = sum(1 for r in out if r.get("source") == "CV")
    print(f"\nWrote {len(out):,} records to {dest}")
    print(f"  archive {n_arch:,} | CV {n_cv:,} | other {len(out) - n_arch - n_cv:,}")
    print("Remember to update the headline counts and 'Last updated' date in publications.Rmd and re-render.")


def cmd_autotag(args):
    """Auto-tag records added earlier without tags (source 'added', no theme and no keywords)."""
    recs = load_records(args.data)
    warnings = []
    todo = [r for r in recs if r.get("source") == "added" and not r["themes"] and not r["keywords"]]
    if not todo:
        print("No untagged 'added' records.")
        return
    tagger = get_tagger(recs, warnings)
    for w in warnings:
        print("WARNING:", w)
    if not tagger:
        sys.exit(1)
    for rec in todo:
        rec["themes"], rec["keywords"] = tagger.tag(rec)
        print(f"  Key {rec['o']} (#{rec['id']}): {short(rec['citation'])}\n      {describe_tags(rec)}")
    if not args.apply:
        print("\nPreview only - re-run with --apply to write the tags.")
        return
    save_records(args.out or args.data, recs)
    print(f"\nTagged {len(todo)} record(s).")


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)
    e = sub.add_parser("export", help="write the editable workbook")
    e.add_argument("xlsx", nargs="?", help=f"output file (default: {DEFAULT_XLSX.name})")
    e.add_argument("--data", default=DATA_JS)
    e.set_defaults(fn=cmd_export)
    i = sub.add_parser("import", help="read an edited workbook back")
    i.add_argument("xlsx")
    i.add_argument("--apply", action="store_true", help="write the changes (default is preview only)")
    i.add_argument("--drop-missing", action="store_true",
                   help="treat records absent from the sheet as deleted (default: keep them)")
    i.add_argument("--no-autotag", action="store_true",
                   help="leave new records' blank Theme/Keywords empty instead of auto-tagging them")
    i.add_argument("--verbose", action="store_true", help="list every change")
    i.add_argument("--data", default=DATA_JS)
    i.add_argument("--out", help="write here instead of over the data file (for testing)")
    i.set_defaults(fn=cmd_import)
    a = sub.add_parser("autotag", help="tag previously added records that have no tags")
    a.add_argument("--apply", action="store_true")
    a.add_argument("--data", default=DATA_JS)
    a.add_argument("--out")
    a.set_defaults(fn=cmd_autotag)
    args = p.parse_args()
    args.fn(args)


if __name__ == "__main__":
    main()
