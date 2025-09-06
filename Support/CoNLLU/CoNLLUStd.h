#include <string>

const char defServiceTag[] = "<>";

const std::vector<std::string> EMPTY_FEATURES = {};
const std::vector<std::string> X_FEATURES =     {"typo", "hyph", "abbr", "foreign", };
const std::vector<std::string> ADJ_FEATURES =   {"typo", "hyph", "style", "abbr", "anom", "degree", "gender",
                                                 "number", "case", "numtype", "numform", "variant", "decl", };
const std::vector<std::string> ADP_FEATURES =   {"typo", "hyph", "style", "abbr", "anom", };
const std::vector<std::string> ADV_FEATURES =   {"typo", "hyph", "style", "abbr", "anom", "polarity", "degree", "numtype", "numform", "variant", };
const std::vector<std::string> AUX_FEATURES =   {"typo", "hyph", "gender", "verbform", "mood", "tense", "number", "case", "aspect", "voice", "person", };
const std::vector<std::string> CCONJ_FEATURES = {"typo", "hyph", "polarity", };
const std::vector<std::string> DET_FEATURES =   {"typo", "hyph", "abbr", "anom", "gender", "number", "case", "variant", };
const std::vector<std::string> INTJ_FEATURES =  {"typo", "hyph", "anom", };
const std::vector<std::string> NOUN_FEATURES =  {"typo", "hyph", "style", "abbr", "anom", "gender", "animacy",
                                                 "number", "case", "nametype", "numtype", "numform", "decl", };
const std::vector<std::string> NUM_FEATURES =   {"typo", "hyph", "numtype", "gender", "number", "case", "numform", };
const std::vector<std::string> PART_FEATURES =  {"typo", "hyph", "style", "anom", "polarity", "mood", };
const std::vector<std::string> PRON_FEATURES =  {"typo", "hyph", "style", "abbr", "prontype", "polarity", "reflex", "gender", "number", "case", "person", };
const std::vector<std::string> PROPN_FEATURES = {"typo", "hyph", "style", "abbr", "anom", "foreign", "nametype", "gender", "animacy", "number", "case", "numtype", };
const std::vector<std::string> PUNCT_FEATURES = EMPTY_FEATURES;
const std::vector<std::string> SCONJ_FEATURES = {"typo", "hyph", "polarity", "mood", };
const std::vector<std::string> SYM_FEATURES =   {"numtype", };
const std::vector<std::string> VERB_FEATURES =  {"typo", "hyph", "style", "abbr", "anom", "gender", "verbform", "mood", "tense", "gender", 
                                                 "number", "case", "aspect", "voice", "person", "subcat", "variant", };

const std::vector<TagDescription> TAG_DESCRIPTIONS = {
    TagDescription {defServiceTag, EMPTY_FEATURES}, // Service
    // https://universaldependencies.org/u/pos/all.html
    TagDescription {"x",     X_FEATURES},     // other/url/foreign/unknown
    TagDescription {"adj",   ADJ_FEATURES},   // adjective
    TagDescription {"adp",   ADP_FEATURES},   // adposition (prepositions and postpositions)
    TagDescription {"adv",   ADV_FEATURES},   // adverb
    TagDescription {"aux",   AUX_FEATURES},   // auxiliary
    TagDescription {"cconj", CCONJ_FEATURES}, // coordinating conjunction
    TagDescription {"det",   DET_FEATURES},   // determiner
    TagDescription {"intj",  INTJ_FEATURES},  // interjection
    TagDescription {"noun",  NOUN_FEATURES},  // noun
    TagDescription {"num",   NUM_FEATURES},   // numeral
    TagDescription {"part",  PART_FEATURES},  // particle
    TagDescription {"pron",  PRON_FEATURES},  // pronoun
    TagDescription {"propn", PROPN_FEATURES}, // proper noun
    TagDescription {"punct", PUNCT_FEATURES}, // punctuation
    TagDescription {"sconj", SCONJ_FEATURES}, // subordinating conjunction
    TagDescription {"sym",   SYM_FEATURES},   // symbol
    TagDescription {"verb",  VERB_FEATURES},  // verb
};

const std::vector<std::string> FEATURE_NAMES = {
    "prontype", "gender", "verbform",
    "numtype", "animacy", "mood",
    "poss", "nounclass", "tense",
    "reflex", "number", "aspect",
    "other", "case", "voice",
    "abbr", "definite", "evident",
    "typo", "deixis", "polarity",
    "foreign", "deixisref", "person",
    "extpos", "degree", "polite",
    "clusivity", "numform", "hyph",
    "subcat", "nametype", "style",
    // added for short verbs/adjs
    "variant",
    // added for noun w/o declension
    "decl",
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
                    "abs", "ben", "cmp", "cns", "equ", "erg", "ess", "com", "lat", "ter", "tra", "cau",
                    "ine", "ill", "ela", "add", "ade", "all", "sup", "spl", "del", "sub", "sbe", "per",
                    "tem", "abe", "dis", "ine", "sbl",
    /* definite  */ "com", "cons", "def", "ind", "spec",
    /* deixis    */ "abv", "bel", "even", "med", "nvis", "prox", "remt",
    /* deixisref */ "1", "2",
    /* degree    */ "abs", "aug", "cmp", "dim", "equ", "pos", "sup",
    /* verbform  */ "conv", "fin", "gdv", "ger", "inf", "part", "sup", "vnoun",
    /* mood      */ "cnd", "imp", "ind", "int", "pot", "sub",
                    "adm", "irr", "jus", "nec", "prp", "qot", "des", "opt",
    /* tense     */ "fut", "imp", "past", "pqp", "pres",
    /* aspect    */ "hab", "imp", "iter", "perf", "prog", "prosp",
    /* voice     */ "act", "mid", "pass", "rcp",
                    "antip", "bfoc", "cau", "dir", "inv", "lfoc",
    /* evident   */ "fh", "nfh",
    /* polarity  */ "neg", "pos",
    /* person    */ "0", "1", "2", "3", "4",
    /* polite    */ "elev", "form", "humb", "infm",
    /* clusivity */ "ex", "in",
    /* subcat    */ "ditr", "indir", "intr", "tran",
    /* nametype  */ "com", "geo", "giv", "nat", "oth", "pat", "pro", "prs", "sur", "zoon",
    /* style     */ "arch", "coll", "expr", "form", "rare", "slng", "vrnc", "vulg",
    /* variant   */ "short",
    /* decl      */ "zero"
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