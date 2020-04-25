#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include <algorithm>
#include <vector>

#if _POSIX_C_SOURCE >= 200809L || _XOPEN_SOURCE >= 7000
#else
// TODO get rid of getline() calls and support non-POSIX.
#error "C/C++ target for CO only supports POSIX platforms, it seems you are not using one"
#endif

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

#define init0(v) memset(&(v), 0, sizeof(v))
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
    T elem = (**v).last();
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

size_t raw_readln(char **buf) {
    *buf = NULL;
    size_t size = 0;
    ssize_t ret = getline(buf, &size, stdin);
    if (ret < 0) {
        perror("Runtime error when reading line from stdin: ");
        exit(1);
    }
    if ((*buf)[ret - 1] == '\n') {
        (*buf)[ret - 1] = '\0';
        --ret;
    }
    return (size_t) ret;
}

void raw_readword(char **out_buf, size_t *out_len) {
    static char *line = NULL;
    static size_t line_len;
    static size_t next;

    for (;;) {
        if (line != NULL && next < line_len) {
            while (!isalnum(line[next])) ++next;
        }

        if (line == NULL || next == line_len) {
            if (line != NULL) free(line);

            line_len = raw_readln(&line);
            next = 0;
            continue;
        }

        break;
    }

    size_t end = next;
    while (end < line_len && isalnum(line[end])) ++end;

    *out_buf = line + next;
    *out_len = end - next;

    next = end;
}

void readln(str *target) {
    char *buf;
    size_t len = raw_readln(&buf);
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