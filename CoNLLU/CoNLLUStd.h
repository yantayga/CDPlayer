#include <string>

const char defServiceTag[] = "<>";

const std::vector<std::string> POS_TAGS = {
    // Service
    defServiceTag,
    // https://universaldependencies.org/u/pos/all.html
    "x",      // other/url/foreign/unknown
    "adj",    // adjective
    "adp",    // adposition (prepositions and postpositions)
    "adv",    // adverb
    "aux",    // auxiliary
    "cconj",  // coordinating conjunction
    "det",    // determiner
    "intj",   // interjection
    "noun",   // noun
    "num",    // numeral
    "part",   // particle
    "pron",   // pronoun
    "propn",  // proper noun
    "punct",  // punctuation
    "sconj",  // subordinating conjunction
    "sym",    // symbol
    "verb",   // verb
};

const std::vector<std::string> FEATURE_NAMES = {
    "prontype", "gender", "verbform",
    "numtype", "animacy", "mood",
    "poss", /*"nounclass",*/ "tense",
    "reflex", "number", "aspect",
    "other", "case", "voice",
    "abbr", "definite",/* "evident",*/
    "typo", "deixis", "polarity",
    "foreign", "deixisref", "person",
    "extpos", "degree", "polite",
    /*"clusivity", */"numform", "hyph",
    "subcat", "nametype", "style",
    // added for short verbs/adjs
    "short",
};

const std::vector<std::string> FEATURE_VALUES = {
    /* prontype  */ "art", "dem", "emp", "exc", "ind", "int", "neg", "prs", "rcp", "rel", "tot",
    /* numtype   */ "card", "dist", "frac", "mult", "ord", "range", "sets",
    /* numform   */ "combi", "digit", "roman", "word",
    /* poss      */ "yes",
    /* reflex    */ "yes",
    /* abbr      */ "yes",
    /* typo      */ "yes",
    /* foreign   */ "yes",
    /* extpos    */ "adj", "adp", "adv", "aux", "cconj", "det", "intj", "pron", "propn", "sconj",
    /* gender    */ "com", "fem", "masc", "neut",
    /* animacy   */ "anim", "hum", "inan", "nhum",
    /* nounclass */ // skipped intentionally
    /* number    */ "coll", "count", "dual", "grpa", "grpl", "inv", "pauc", "plur", "ptan", "sing", "tri",
    /* case      */ "nom", "gen", "dat", "acc", "abl", "ins", "voc", "par", "loc",
//                  "abs", "ben", "cmp", "cns", "equ", "erg", "ess", "com", "lat", "ter", "tra", "cau", // skipped intentionally
//                  "ine", "ill", "ela", "add", "ade", "all", "sup", "spl", "del", "sub", "sbe", "per", // skipped intentionally
//                  "tem", "abe", "dis", "ine", "sbl", // skipped intentionally
    /* definite  */ "com", "cons", "def", "ind", "spec",
    /* deixis    */ "abv", "bel", "even", "med", /* "nvis", */ "prox", "remt",
    /* deixisref */ "1", "2",
    /* degree    */ "abs", "aug", "cmp", "dim", "equ", "pos", "sup",
    /* verbform  */ "conv", "fin", "gdv", "ger", "inf", "part", "sup", "vnoun",
    /* mood      */ "cnd", "imp", "ind", "int", "pot", "sub",
//                  "adm", "irr", "jus", "nec", "prp", "qot", "des", "opt", // skipped intentionally
    /* tense     */ "fut", "imp", "past", "pqp", "pres",
    /* aspect    */ "hab", "imp", "iter", "perf", "prog", "prosp",
    /* voice     */ "act", "mid", "pass", "rcp",
//                  "antip", "bfoc", "cau", "dir", "inv", "lfoc", // skipped intentionally
//    /* evident   */ "fh", "nfh", // skipped intentionally
    /* polarity  */ "neg", "pos",
    /* person    */ "0", "1", "2", "3", "4",
    /* polite    */ "elev", "form", "humb", "infm",
//    /* clusivity */ "ex", "in", // skipped intentionally
    /* subcat    */ "ditr", "indir", "intr", "tran",
    /* nametype  */ "com", "geo", "giv", "nat", "oth", "pat", "pro", "prs", "sur", "zoon",
    /* style     */ "arch", "coll", "expr", "form", "rare", "slng", "vrnc", "vulg",
};

const std::vector<std::string> DEP_RELS = {
    /* Core arguments      */ "nsubj", "obj", "iobj", "csubj", "ccomp", "xcomp",
    /* Non-core dependents */ "obl", "vocative", "expl", "dislocated", "advcl",
                              "advmod", "discourse", "aux", "cop", "mark",
    /* Nominal dependents  */ "nmod", "appos", "nummod", "acl", "amod", "det", "clf", "case",
    /* Coordination        */ "conj", "cc",
    /* Headless            */ "fixed", "flat",
    /* Loose               */ "list", "parataxis",
    /* Special             */ "compound", "orphan", "goeswith", "reparandum",
    /* Other               */ "punct", "root", "dep",
};

const std::vector<std::string> DEP_RELS_MODIFIERS = {
    "", "outer", "pass", "agent", "arg", "lmod", "tmod", "outer", "pass", "emph", "lmod", "impers", "pass", "relcl", "poss",
    "pass", "tmod", "numgov", "nummod", "gov", "foreign", "name", "lvc", "prt", "redup", "svc", "pv", "relcl", "poss", "preconj",
};