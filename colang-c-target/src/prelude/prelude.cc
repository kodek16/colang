#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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

#define init0(v) memset(&(v), 0, sizeof(v))
template <typename T>
void init_vec(vec<T> **v) { *v = new vec<T>; }

#if _POSIX_C_SOURCE >= 200809L || _XOPEN_SOURCE >= 7000
#else
// TODO get rid of getline() calls and support non-POSIX.
#error "C/C++ target for CO only supports POSIX platforms, it seems you are not using one"
#endif

size_t raw_readln(char **buf) {
    *buf = NULL;
    size_t size = 0;
    ssize_t ret = getline(buf, &size, stdin);
    if (ret < 0) {
        perror("Runtime error when reading line from stdin: ");
        exit(1);
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