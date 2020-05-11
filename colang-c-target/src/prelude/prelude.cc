#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <algorithm>
#include <vector>

#define static_str static const char *
#define vec std::vector
typedef int32_t i32;
typedef std::vector<char> *str;

#define add(a, b) ((a) + (b))
#define sub(a, b) ((a) - (b))
#define mul(a, b) ((a) * (b))
#define div(a, b) ((a) / (b))
#define mod(a, b) ((a) % (b))
#define lt(a, b) ((a) < (b))
#define gt(a, b) ((a) > (b))
#define lte(a, b) ((a) <= (b))
#define gte(a, b) ((a) >= (b))
#define eq(a, b) ((a) == (b))
#define neq(a, b) ((a) != (b))

#define init_0(v) memset(v, 0, sizeof(*v))
template <typename T>
void init_vec(vec<T> **v) { *v = new vec<T>; }

void co_assert(char cond) {
    if (!cond) {
        fprintf(stderr, "Runtime error: Assertion failed\n");
        exit(1);
    }
}

i32 ascii_code(char c) {
    return (i32) c;
}

char ascii_char(i32 code) {
    if (code < 0 || 0xFF < code) {
        fprintf(stderr, "Runtime error: %d is not a valid ASCII code.\n", code);
        exit(1);
    }

    return (char) code;
}

str co_itos(i32 x) {
    if (x == INT_MIN) {
        return new vec<char> {'-','2','1','4','7','4','8','3','6','4','8'};
    }
    if (x == 0) {
        return new vec<char> {'0'};
    }

    str s = new vec<char>;
    char neg = 0;
    if (x < 0) {
        neg = 1;
        x = -x;
    }
    while (x) {
        s->push_back('0' + (x % 10));
        x /= 10;
    }
    if (neg) {
        s->push_back('-');
    }

    for (size_t l = 0, r = s->size() - 1; l < r; ++l, --r) {
        char t = (*s)[l];
        (*s)[l] = (*s)[r];
        (*s)[r] = t;
    }
    return s;
}

str str_add(str a, str b) {
    str result = new vec<char>(a->size() + b->size());
    std::copy(a->begin(), a->end(), result->begin());
    std::copy(b->begin(), b->end(), result->begin() + a->size());
    return result;
}

char *str_index(str s, i32 i) {
    return &(*s)[i];
}

char str_eq(str a, str b) {
    return (a->size() == b->size()) && std::equal(a->begin(), a->end(), b->begin());
}

char str_neq(str a, str b) {
    return !str_eq(a, b);
}

template <typename T>
void vec_push(vec<T> **v, T elem) {
    (**v).push_back(elem);
}

template <typename T>
T vec_pop(vec<T> **v) {
    T elem = (**v).back();
    (**v).pop_back();
    return elem;
}

template <typename T>
i32 vec_len(vec<T> *v) {
    return v->size();
}

template <typename T>
T *vec_index(vec<T> *v, i32 i) {
    return &(*v)[i];
}

// Reads the next line from stdin, storing it in newly allocated *buf.
//
// Line length is returned from function. Newline character is not included.
size_t raw_readln(char **buf) {
    static const char MARK = 0xFF;

    size_t buf_size = 16;
    *buf = (char *) malloc(buf_size);
    char *next = *buf;

    for (;;) {
        (*buf)[buf_size - 1] = MARK;

        if (fgets(next, buf_size - (next - *buf), stdin) == NULL) {
            perror("Input error when reading line from stdin: ");
            exit(1);
        }

        if (feof(stdin) || (*buf)[buf_size - 1] == MARK) {
            break;
        }

        *buf = (char *) realloc(*buf, buf_size * 2);
        if (*buf == NULL) {
            perror("Could not allocate buffer for line read from stdin:");
            exit(1);
        }

        next = *buf + (buf_size - 1);
        buf_size *= 2;
    }

    size_t len = (next - *buf) + strlen(next);
    if ((*buf)[len - 1] == '\n') {
        (*buf)[len - 1] = '\0';
        --len;
    }

    return len;
}

// Buffered words from the line currently being read by word.
typedef struct {
    char *line;
    size_t line_len;
    size_t next;
} read_word_ctx_t;

static read_word_ctx_t rwctx { NULL, 0, 0 };

void raw_readword(char **out_buf, size_t *out_len) {
    for (;;) {
        if (rwctx.line != NULL && rwctx.next < rwctx.line_len) {
            while (!isalnum(rwctx.line[rwctx.next])) ++rwctx.next;
        }

        if (rwctx.line == NULL || rwctx.next == rwctx.line_len) {
            free(rwctx.line);
            rwctx.line_len = raw_readln(&rwctx.line);
            rwctx.next = 0;
            continue;
        }

        break;
    }

    size_t end = rwctx.next;
    while (end < rwctx.line_len && isalnum(rwctx.line[end])) ++end;

    *out_buf = rwctx.line + rwctx.next;
    *out_len = end - rwctx.next;

    rwctx.next = end;
}

void readln(str *target) {
    free(rwctx.line);
    rwctx.line = NULL;

    char *buf = NULL;
    size_t len = 0;
    while (!len) {
        free(buf);
        len = raw_readln(&buf);
    }
    *target = new vec<char>(buf, buf + len);
    free(buf);
}

void readword_str(str *target) {
    char *buf;
    size_t len;
    raw_readword(&buf, &len);
    *target = new vec<char>(buf, buf + len);
}

void readword_int(i32 *target) {
    char *buf;
    size_t len;
    raw_readword(&buf, &len);

    char neg = 0;
    if (*buf == '-') {
        ++buf;
        --len;
        neg = 1;
    }

    *target = 0;
    while (len) {
        *target *= 10;
        *target -= *buf - '0';
        ++buf;
        --len;
    }

    if (!neg) {
        *target = -*target;
    }
}

void write_str(str s) {
    char *buf = s->data();
    size_t len = s->size();
    if (!len) {
        return;
    }

    char last_char = buf[len - 1];
    buf[len - 1] = '\0';

    if (fputs(buf, stdout) < 0) {
        perror("Runtime error when writing to stdout: ");
        exit(1);
    }

    buf[len - 1] = last_char;
    if (fputc(last_char, stdout) < 0) {
        perror("Runtime error when writing to stdout: ");
        exit(1);
    }
}