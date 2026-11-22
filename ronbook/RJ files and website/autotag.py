"""Auto-tag publications with a theme and keywords, consistently with the site.

Used by publications_xlsx.py to tag NEW records that arrive without tags. It
re-creates the site's tagging method, and is self-contained (no saved model):

  * Theme: cosine similarity between the record's text (abstract if given, else
    title, plus the journal name only where the journal itself signals a theme)
    and a short seed vocabulary for each of the seven themes. The three
    "starred" themes are tried first (floor 0.08), then the other four
    (floor 0.12); below the floors the record is left unclassified. The TF-IDF
    space is fitted on the existing abstracts.
  * Keywords: the site's fixed vocabulary of recurring words only (never new
    terms); up to the three highest TF-IDF-weighted that occur in the
    title/abstract/synopsis.

Obituaries and Memoirs are always left unclassified, as on the site.
Needs numpy and scikit-learn.
"""
import json
import re
from pathlib import Path

import numpy as np
from sklearn.feature_extraction import text as sk_text
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

HERE = Path(__file__).resolve().parent
DATA_JS = HERE.parent / "publications_data.js"
OPENALEX_JSON = HERE / "openalex_abstracts.json"
PREFIX = "const PUBLICATIONS = "

PRIMARY_FLOOR = 0.08
FALLBACK_FLOOR = 0.12

# ------------------------------------------------------------------ theme seeds
PRIMARY_THEMES = {
    "Electoral and political geography": [
        "electoral geography", "great britain", "regional voting", "british politics",
        "government support", "party support", "geography of voting", "british electoral",
        "electoral bias", "seats votes", "party bias", "vote share", "swing",
        "labour conservative", "cube law", "votes seats",
        "turnout", "abstention", "participation", "general election turnout",
        "british general elections", "non-voting", "electoral participation",
        "boundary commission", "constituency boundaries", "parliamentary boundaries",
        "redistricting", "boundary commissions", "constituencies", "malapportionment",
        "boundary review", "electoral districts",
        "campaign spending", "constituency campaigning", "election expenditure",
        "campaign expenditure", "local campaigns", "candidate spending", "campaigning",
        "proportional representation", "electoral systems", "power indices",
        "apportionment", "voting power", "european elections", "electoral reform",
        "united states", "federal", "gerrymandering", "american politics",
        "supreme court", "redistricting united states", "united states congress",
        "neighbourhood effect", "contextual effects", "voting patterns", "local context",
        "geography of voting behaviour", "neighbourhood effects",
        "voting behaviour", "spatial model voting", "ecological inference", "attitudes voting",
        "spatial variations voting", "statistical model of voting", "political parties",
        "election", "elections", "voters", "voting", "constituency", "parliamentary",
        "referendum", "political geography",
    ],
    "Urban social geography": [
        "residential segregation", "ethnic segregation", "ethnic groups", "segregation",
        "residential", "cities", "degree of segregation", "dissimilarity index",
        "urban segregation", "school segregation", "pupils", "secondary schools",
        "ethnic composition schools", "education segregation", "schools england",
        "school choice", "labour market", "ethnic minorities", "housing", "immigrants",
        "immigration", "assimilation", "labour market penalties", "migrants",
        "ethnic minority", "melbourne", "auckland", "new zealand", "australian geography",
        "central place", "urban development australia", "suburbs", "australian cities",
        "urban structure", "urban social area", "residential patterns", "neighbourhood",
        "city structure", "urban geography", "social area analysis", "mental maps",
        "residential preferences", "urban system",
    ],
    "History of human geography": [
        "history of human geography", "history of geography", "disciplinary history",
        "history of the discipline", "discipline of geography", "historiography of geography",
        "history of geographical thought", "history of geographical ideas",
        "geographical tradition", "geography's development", "quantitative revolution",
        "philosophy of geography", "geographical thought", "paradigm shift geography",
        "paradigms and revolutions geography", "classics in human geography",
        "classic paper in geography", "geography's past", "pioneers of geography",
        "geographer's career", "geographer's biography", "biography of geographers",
        "autobiographical essays geographers", "obituary", "in memoriam", "tribute",
        "academic history", "geography department archives", "geography's identity",
        "nature of geography", "geographical canon",
    ],
}

FALLBACK_THEMES = {
    "Methods and methodology": [
        "quantitative methods", "statistical model", "multilevel modelling",
        "factor analysis", "entropy maximising", "spatial autocorrelation",
        "discriminant analysis", "regression analysis", "methodology",
        "statistical technique", "modelling approach", "spatial statistics",
        "principal components", "cluster analysis", "classification methods",
        "spatial analysis method", "statistical methods geography",
        "qualitative methods", "research design", "case study method",
        "fieldwork", "survey method", "questionnaire design", "interview method",
        "mixed methods", "methodological debate", "measurement error",
        "data collection method", "sampling method", "research technique",
    ],
    "Regional geography, population and migration": [
        "population change", "population movement", "population growth",
        "regional development", "regional planning", "rural population",
        "metropolitan expansion", "regional unemployment", "friction of distance",
        "spatial interaction", "gravity model", "mover stayer model", "kinship",
        "population stability", "migrant", "migrants", "immigrant", "immigrants",
        "immigration", "assimilation", "ancestry", "regional studies",
        "small towns", "income variations", "standard of living", "drift north",
        "regional economic geography", "spatial mobility", "population distribution",
        "regional income", "regional unemployment trends", "new zealand",
        "migration chains", "skilled immigrant", "consumer behaviour",
        "shopping habits", "central business", "grocery prices", "urban hierarchy",
    ],
    "Higher education policy and research assessment": [
        "higher education", "universities", "university", "research assessment exercise",
        "rae", "ref2021", "research funding", "university funding", "teaching quality",
        "student staff ratios", "performance indicators", "department size",
        "postgraduate training", "graduate careers", "geography degree",
        "academic career", "widening participation", "admissions policy",
        "tuition fees", "student fees", "quality assessment", "research council",
        "funding council", "academic labour market", "university administration",
        "career patterns graduates", "research quality", "academics", "professors",
        "dearing report", "graduate dissatisfaction",
    ],
    "Reflections and commentaries": [
        "reply", "response", "comment", "commentary", "reflections",
        "some reactions", "some responses", "editorial",
        "professional responsibilities", "discipline's literature", "citations",
        "misconceptions", "technical comments", "critique", "rejoinder",
        "remarks on", "a reply to", "responses to", "reactions to",
        "correspondence", "rebuttal", "counter argument",
    ],
}

# Journals whose own name signals a theme; every other journal name is ignored
# so that words like "geography" or "journal" in venue names can't skew results.
MEANINGFUL_JOURNALS = {
    "Electoral Studies", "British Journal of Political Science", "Political Geography",
    "Political Studies", "Political Geography Quarterly", "Journal of Elections, Public Opinion and Parties",
    "Parliamentary Affairs", "The Political Quarterly", "Political Quarterly", "Representation",
    "Party Politics", "European Journal of Political Research", "Journal of Legislative Studies",
    "British Journal of Politics and International Relations", "The British Journal of Politics and International Relations",
    "Politics", "Geopolitics", "British Politics", "Canadian Journal of Political Science",
    "Political Behavior", "Political Analysis", "Australian Journal of Political Science",
    "Space and Polity", "American Political Science Review", "British Elections and Parties Yearbook",
    "Votes & Opinions", "Journal of Elections", "Electoral Geography",

    "Journal of Economic Geography", "International Journal of Population Geography",
    "Journal of Ethnic and Migration Studies", "Regional Studies", "Regional & Federal Studies",
    "Journal of Rural Studies", "Population, Space and Place", "International Journal of Urban and Regional Research",

    "Housing Studies", "Urban Geography", "Urban Studies", "Ethnicities",

    "Journal of Geography in Higher Education", "Studies in Higher Education",
    "Higher Education Quarterly", "Quality in Higher Education",

    "Qual Quant", "The Statistician", "Journal of the Operational Research Society", "Radical Statistics",

    "Dialogues in Human Geography",
}

# Words stripped from the TF-IDF space.
EXTRA_STOPWORDS = [
    "paper", "study", "studies", "article", "research", "analysis", "based",
    "using", "results", "findings", "data", "approach", "author", "authors",
    "abstract", "case", "examine", "examines", "examined", "explore", "explores",
    "discuss", "discusses", "discussed", "argue", "argues", "argued", "suggest",
    "suggests", "suggested", "provide", "provides", "provided", "shows", "show",
    "shown", "significant", "significantly", "importance", "important", "does",
    "used", "use", "new", "recent", "recently", "different", "particular",
    "wiley", "sons", "ltd", "john", "rights", "reserved", "copyright", "elsevier",
    "pp", "vol", "reg", "johnston", "pattie", "rossiter", "ron", "taylor",
    "francis", "springer", "sage", "publishing", "press", "informa", "springerlink",
    "der", "die", "das", "und", "mit", "von", "ist", "dem", "den", "des",
    "une", "la", "le", "les", "et", "en", "dans", "pour", "est", "sur",
    "qui", "que", "dont", "cette", "ces", "aux", "au",
]


# ------------------------------------------------------------- text helpers
def strip_coauthor_prefix(citation):
    return re.sub(r"^\(with[^)]*\)\s*", "", citation, flags=re.IGNORECASE)


def split_title_rest(citation):
    """'Title. Journal, Vol(Issue), pages.' -> (title, rest)."""
    c = strip_coauthor_prefix(citation)
    parts = re.split(r"(?<=[a-z0-9'\"’])\.\s+(?=[A-Z])", c, maxsplit=1)
    if len(parts) == 2:
        return parts[0], parts[1]
    # Titles ending '?' or '!' - only trust the split if what follows looks
    # bibliographic (a digit soon after), since '?' is often mid-title.
    for m in re.finditer(r"[?!]\s+(?=[A-Z])", c):
        rest = c[m.end():]
        if re.search(r"\d", rest[:100]):
            return c[:m.start() + 1], rest
    return c, ""


def clean_journal(rest):
    if not rest:
        return ""
    r = re.sub(r"\(ISSN[^)]*\)", "", rest)
    m = re.search(r"\d", r)
    if m:
        r = r[:m.start()]
    return r.strip(" ,.;:()")


def journal_of(citation):
    _, rest = split_title_rest(citation)
    j = clean_journal(rest)
    return j if j in MEANINGFUL_JOURNALS else ""


# Capitalised "University"/"Universities" in a publisher name ("Temple University
# Press") would otherwise drag book reviews into the higher-education theme.
PUBLISHER_NOISE_RE = re.compile(r"\bUniversit(?:y|ies)\b(?:\s+of\s+[A-Z][a-zA-Z]*)?(?:\s+Press)?")


def strip_publisher_noise(text):
    return PUBLISHER_NOISE_RE.sub("", text)


def classification_text(rec):
    """The text a record is classified on (theme)."""
    typ = rec.get("type")
    abstract = (rec.get("abstract") or "").strip()
    synopsis = (rec.get("synopsis") or "").split("\n\n(AI generated)")[0].strip()
    journal = journal_of(rec["citation"])
    if abstract:
        return abstract + (" " + journal if journal else "")
    if typ == "Book" and synopsis:
        return synopsis + (" " + journal if journal else "")
    if typ == "Reviews":
        body = re.sub(r"^Review of:\s*", "", rec["citation"])
        title, _ = split_title_rest(body)
        return strip_publisher_noise(title)
    if typ == "Dictionary of Human Geography":
        return rec["citation"]
    title, _ = split_title_rest(rec["citation"])
    return strip_publisher_noise(title) + (" " + journal if journal else "")


def keyword_text(rec):
    """Title/abstract only - never journal name or year."""
    typ = rec.get("type")
    abstract = (rec.get("abstract") or "").strip()
    synopsis = (rec.get("synopsis") or "").split("\n\n(AI generated)")[0].strip()
    if abstract:
        return abstract
    if typ == "Book" and synopsis:
        return synopsis
    if typ == "Reviews":
        body = re.sub(r"^Review of:\s*", "", rec["citation"])
        title, _ = split_title_rest(body)
        return strip_publisher_noise(title)
    if typ == "Dictionary of Human Geography":
        return rec["citation"]
    title, _ = split_title_rest(rec["citation"])
    return strip_publisher_noise(title)


_JOINS = {r"\bnew\s+zealand\b": "newzealandplaceholder", r"\bunited\s+kingdom\b": "unitedkingdomplaceholder"}


def join_proper_nouns(text):
    for pat, tok in _JOINS.items():
        text = re.sub(pat, tok, text, flags=re.IGNORECASE)
    return text


def vocab_form(term):
    return {"New Zealand": "newzealandplaceholder",
            "United Kingdom": "unitedkingdomplaceholder"}.get(term, term.lower())


# --------------------------------------------------------- abstract cleaning
_JUNK = ["click to increase image size", "click to decrease image size"]
_CITE_ECHO = re.compile(r"[A-Z][A-Za-z.,\s]{0,60}\(\d{4}[a-z]?\)")
_KW_SPLIT = re.compile(r"key\s*words?\s*:", re.IGNORECASE)


def clean_abstract(raw):
    if not raw:
        return None
    a = raw
    if any(m in a.lower() for m in _JUNK):
        return None
    if "Keywords:" in a:
        a = a.split("Keywords:")[0]
    a = _KW_SPLIT.split(a, maxsplit=1)[0]
    matches = list(_CITE_ECHO.finditer(a))
    if len(matches) >= 2:
        a = a[:matches[1].start()]
    a = a.strip()
    return a if len(a) > 40 else None


# ------------------------------------------------------------------ the model
class AutoTagger:
    def __init__(self, recs, extra_abstracts=None):
        self.themes = {**PRIMARY_THEMES, **FALLBACK_THEMES}
        self.theme_names = list(self.themes)
        self.n_primary = len(PRIMARY_THEMES)
        self.n_fallback = len(FALLBACK_THEMES)

        # TF-IDF space fitted on the existing abstracts (archive records).
        abstracts = {}
        for o, raw in (extra_abstracts or {}).items():
            c = clean_abstract(raw)
            if c:
                abstracts[o] = c
        for r in recs:
            if r.get("abstract") and r["o"] not in abstracts:
                abstracts[r["o"]] = r["abstract"]
        stop = list(sk_text.ENGLISH_STOP_WORDS.union(EXTRA_STOPWORDS))
        self.vectorizer = TfidfVectorizer(
            ngram_range=(1, 3), stop_words=stop, min_df=4, max_df=0.5,
            sublinear_tf=True, token_pattern=r"(?u)\b[a-zA-Z][a-zA-Z\-]+\b",
        )
        self.vectorizer.fit(list(abstracts.values()))
        self.theme_vecs = self.vectorizer.transform([" ".join(v) for v in self.themes.values()])

        # Keywords: fixed vocabulary = every keyword already used on the site.
        vocab = sorted({k for r in recs for k in (r.get("keywords") or [])})
        self.kw_display = {vocab_form(k): k for k in vocab}
        self.kw_vec = TfidfVectorizer(
            vocabulary=[vocab_form(k) for k in vocab], ngram_range=(1, 1), sublinear_tf=True,
            token_pattern=r"(?u)\b[a-zA-Z][a-zA-Z\-]{2,}\b",
        )
        self.kw_vec.fit([join_proper_nouns(keyword_text(r)) for r in recs])  # IDF weights only
        self.kw_names = self.kw_vec.get_feature_names_out()

    def theme(self, rec):
        """-> [[theme, score]] or []"""
        if rec.get("type") == "Obituaries and Memoirs":
            return []
        v = self.vectorizer.transform([classification_text(rec)])
        sims = cosine_similarity(v, self.theme_vecs)[0]
        prim = sims[: self.n_primary]
        bp = int(np.argmax(prim))
        fall = sims[self.n_primary: self.n_primary + self.n_fallback]
        bf = int(np.argmax(fall))
        if prim[bp] >= PRIMARY_FLOOR:
            return [[self.theme_names[bp], round(float(prim[bp]), 4)]]
        if fall[bf] >= FALLBACK_FLOOR:
            return [[self.theme_names[self.n_primary + bf], round(float(fall[bf]), 4)]]
        return []

    def keywords(self, rec):
        row = self.kw_vec.transform([join_proper_nouns(keyword_text(rec))])
        top = sorted(zip(row.indices, row.data), key=lambda x: -x[1])[:3]
        return [self.kw_display[self.kw_names[i]] for i, s in top if s > 0]

    def tag(self, rec):
        return self.theme(rec), self.keywords(rec)


def load_records(path=DATA_JS):
    text = Path(path).read_text(encoding="utf-8")
    return json.loads(text[len(PREFIX):].rstrip(";\n"))


def default_tagger(recs=None):
    recs = recs if recs is not None else load_records()
    extra = {}
    if OPENALEX_JSON.exists():
        for e in json.loads(OPENALEX_JSON.read_text(encoding="utf-8")).values():
            if e.get("abstract") and "o" in e:
                extra[e["o"]] = e["abstract"]
    return AutoTagger(recs, extra)
