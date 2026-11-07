#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Utils.h>
#include <R_ext/Visibility.h>
#include <stdint.h>
#include <string.h>
#include <limits.h>

typedef struct {
    int child[26];
    int count;
} Node;

typedef struct {
    int m, n, blocks, used[5], shared, stopped;
    int *letters, *paths;
    Node *nodes;
    uint32_t *position_bits, *scratch;
    double counts[5], checked, limit;
    SEXP hits[5];
    PROTECT_INDEX protection[5];
    int stored[5], capacity[5], retain[5];
} Search;

/* A subtree is usable if it contains a word other than the selected rows. */
static int available(Search *s, int node, int depth, int used_count) {
    if (!node) return 0;
    if (s->shared) return 1;
    int remaining = s->nodes[node].count;
    for (int i = 0; i < used_count; ++i) {
        if (s->paths[s->used[i] * s->n + depth] == node) --remaining;
    }
    return remaining > 0;
}

static void remember(Search *s, int depth) {
    int slot = depth - 1;
    if (!s->retain[slot]) return;
    if (s->stored[slot] == s->capacity[slot]) {
        if (s->capacity[slot] > INT_MAX / 2) error("too many stored squares");
        int capacity = s->capacity[slot] ? s->capacity[slot] * 2 : 64;
        SEXP expanded = PROTECT(allocVector(VECSXP, capacity));
        for (int i = 0; i < s->stored[slot]; ++i)
            SET_VECTOR_ELT(expanded, i, VECTOR_ELT(s->hits[slot], i));
        REPROTECT(s->hits[slot] = expanded, s->protection[slot]);
        UNPROTECT(1);
        s->capacity[slot] = capacity;
    }
    SEXP row = PROTECT(allocVector(INTSXP, depth));
    for (int i = 0; i < depth; ++i) INTEGER(row)[i] = s->used[i] + 1;
    SET_VECTOR_ELT(s->hits[slot], s->stored[slot]++, row);
    UNPROTECT(1);
}

static int trailing_zeroes(uint32_t bits) {
#if defined(__GNUC__) || defined(__clang__)
    return __builtin_ctz(bits);
#else
    int i = 0;
    while (!(bits & 1U)) { bits >>= 1; ++i; }
    return i;
#endif
}

/* depth is the next row position (zero based); columns are trie node IDs. */
static void visit(Search *s, int depth, const int *columns) {
    R_CheckUserInterrupt();
    uint32_t *candidates = s->scratch + (size_t) depth * 2 * s->blocks;
    uint32_t *allowed = candidates + s->blocks;
    for (int b = 0; b < s->blocks; ++b) candidates[b] = UINT32_MAX;
    for (int col = 0; col < s->n; ++col) {
        memset(allowed, 0, (size_t) s->blocks * sizeof(uint32_t));
        for (int letter = 0; letter < 26; ++letter) {
            int child = s->nodes[columns[col]].child[letter];
            if (!available(s, child, depth, depth)) continue;
            uint32_t *bits = s->position_bits +
                ((size_t) col * 26 + letter) * s->blocks;
            for (int b = 0; b < s->blocks; ++b) allowed[b] |= bits[b];
        }
        int any = 0;
        for (int b = 0; b < s->blocks; ++b) {
            candidates[b] &= allowed[b];
            any |= candidates[b] != 0;
        }
        if (!any) return;
    }
    for (int i = 0; i < depth; ++i)
        candidates[s->used[i] / 32] &= ~(1U << (s->used[i] % 32));
    for (int b = 0; b < s->blocks; ++b) {
        uint32_t bits = candidates[b];
        while (bits) {
            int word = b * 32 + trailing_zeroes(bits);
            bits &= bits - 1U;
            s->checked += 1;
            if (((unsigned long) s->checked & 16383UL) == 0) R_CheckUserInterrupt();
            s->used[depth] = word;
            int next[5], possible = 1;
            for (int col = 0; col < s->n; ++col) {
                next[col] = s->nodes[columns[col]].child[s->letters[word * s->n + col]];
                if (!available(s, next[col], depth, depth + 1)) {
                    possible = 0;
                    break;
                }
            }
            if (!possible) continue;
            if (depth >= 1) {
                s->counts[depth] += 1;
                remember(s, depth + 1);
            }
            if (depth + 1 == s->n) {
                if (s->counts[depth] >= s->limit) {
                    s->stopped = 1;
                    return;
                }
            } else {
                visit(s, depth + 1, next);
                if (s->stopped) return;
            }
        }
    }
}

SEXP C_search_squares(SEXP words, SEXP shared, SEXP retain, SEXP limit) {
    Search s;
    memset(&s, 0, sizeof(s));
    if (TYPEOF(words) != STRSXP || XLENGTH(words) < 1 || XLENGTH(words) > INT_MAX / 5 - 2)
        error("invalid dictionary size");
    if (TYPEOF(shared) != LGLSXP || XLENGTH(shared) != 1 || LOGICAL(shared)[0] == NA_LOGICAL ||
        TYPEOF(retain) != LGLSXP || XLENGTH(retain) != 5)
        error("invalid search flags");
    if (STRING_ELT(words, 0) == NA_STRING) error("missing dictionary word");
    s.m = (int) XLENGTH(words);
    s.n = (int) strlen(CHAR(STRING_ELT(words, 0)));
    if (s.n < 2 || s.n > 5) error("invalid word length");
    s.blocks = (s.m + 31) / 32;
    s.shared = asLogical(shared);
    s.limit = asReal(limit);
    /* Root is node 1; node 0 represents a missing prefix. */
    size_t max_nodes = (size_t) s.m * s.n + 2;
    if (max_nodes > INT_MAX) error("dictionary too large for prefix index");
    s.nodes = (Node *) R_alloc(max_nodes, sizeof(Node));
    memset(s.nodes, 0, max_nodes * sizeof(Node));
    s.letters = (int *) R_alloc((size_t) s.m * s.n, sizeof(int));
    s.paths = (int *) R_alloc((size_t) s.m * s.n, sizeof(int));
    size_t bit_count = (size_t) s.n * 26 * s.blocks;
    s.position_bits = (uint32_t *) R_alloc(bit_count, sizeof(uint32_t));
    memset(s.position_bits, 0, bit_count * sizeof(uint32_t));
    s.scratch = (uint32_t *) R_alloc((size_t) s.n * 2 * s.blocks, sizeof(uint32_t));
    int node_count = 2;
    for (int word = 0; word < s.m; ++word) {
        if ((word & 16383) == 0) R_CheckUserInterrupt();
        if (STRING_ELT(words, word) == NA_STRING) error("missing dictionary word");
        const char *text = CHAR(STRING_ELT(words, word));
        if (strlen(text) != (size_t) s.n) error("inconsistent word lengths");
        int node = 1;
        for (int pos = 0; pos < s.n; ++pos) {
            int letter = text[pos] - 'A';
            if (letter < 0 || letter >= 26) error("dictionary must contain A-Z only");
            s.letters[word * s.n + pos] = letter;
            if (!s.nodes[node].child[letter]) s.nodes[node].child[letter] = node_count++;
            node = s.nodes[node].child[letter];
            ++s.nodes[node].count;
            s.paths[word * s.n + pos] = node;
            s.position_bits[((size_t) pos * 26 + letter) * s.blocks + word / 32]
                |= 1U << (word % 32);
        }
    }
    for (int i = 0; i < 5; ++i) {
        s.retain[i] = LOGICAL(retain)[i];
        PROTECT_WITH_INDEX(s.hits[i] = allocVector(VECSXP, 0), &s.protection[i]);
    }
    int columns[5] = {1, 1, 1, 1, 1};
    visit(&s, 0, columns);
    SEXP result = PROTECT(allocVector(VECSXP, 4));
    SEXP counts = PROTECT(allocVector(REALSXP, 5));
    SEXP hits = PROTECT(allocVector(VECSXP, 5));
    for (int i = 0; i < 5; ++i) {
        REAL(counts)[i] = i < s.n ? s.counts[i] : NA_REAL;
        if (s.stored[i]) {
            SEXP matrix = PROTECT(allocMatrix(INTSXP, i + 1, s.stored[i]));
            for (int j = 0; j < s.stored[i]; ++j)
                memcpy(INTEGER(matrix) + (size_t) j * (i + 1),
                       INTEGER(VECTOR_ELT(s.hits[i], j)), (size_t) (i + 1) * sizeof(int));
            SET_VECTOR_ELT(hits, i, matrix);
            UNPROTECT(1);
        }
    }
    SET_VECTOR_ELT(result, 0, counts);
    SET_VECTOR_ELT(result, 1, hits);
    SET_VECTOR_ELT(result, 2, ScalarLogical(!s.stopped));
    SET_VECTOR_ELT(result, 3, ScalarReal(s.checked));
    UNPROTECT(8);
    return result;
}

static const R_CallMethodDef call_methods[] = {
    {"C_search_squares", (DL_FUNC) &C_search_squares, 4},
    {NULL, NULL, 0}
};

void attribute_visible R_init_magicsquare(DllInfo *dll) {
    R_registerRoutines(dll, NULL, call_methods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
