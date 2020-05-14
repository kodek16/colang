#include <ctype.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef int32_t i32;

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

#define define_array(T, array_T)                             \
  typedef struct s_ ## array_T {                             \
    T *data;                                                 \
    size_t len;                                              \
    size_t capacity;                                         \
  } array_T;                                                 \
                                                             \
  void init_with_cap_  ## array_T(array_T *v, size_t cap) {  \
    v->data = (T *) malloc(cap * sizeof(T));                 \
    v->capacity = cap;                                       \
    v->len = 0;                                              \
  }                                                          \
                                                             \
  inline void init_ ## array_T(array_T *v) {                 \
    init_with_cap_ ## array_T(v, 8);                         \
  }                                                          \
                                                             \
  void push_ ## array_T(array_T *v, T x) {                   \
    if (v->len == v->capacity) {                             \
      v->data = (T *) realloc(v->data, v->capacity * 2);     \
      if (v->data == NULL) {                                 \
        perror("Out of memory error:");                      \
        exit(1);                                             \
      }                                                      \
                                                             \
      v->capacity *= 2;                                      \
    }                                                        \
                                                             \
    v->data[v->len] = x;                                     \
    ++(v->len);                                              \
  }                                                          \
                                                             \
  T pop_ ## array_T(array_T *v) {                            \
    if (v->len == 0) {                                       \
      fprintf(                                               \
          stderr,                                            \
          "Error: cannot pop() from empty array\n");         \
      exit(1);                                               \
    }                                                        \
                                                             \
    --(v->len);                                              \
    return v->data[v->len];                                  \
  }                                                          \
                                                             \
  inline int32_t len_ ## array_T(array_T v) {                \
    return v.len;                                            \
  }                                                          \
                                                             \
  T *index_ ## array_T(array_T v, int32_t index) {           \
    if (index < 0 || v.len <= index) {                       \
      fprintf(                                               \
          stderr,                                            \
          "Error: array index %d out of bounds: size is %d", \
          index,                                             \
          (int32_t) v.len);                                  \
      exit(1);                                               \
    }                                                        \
                                                             \
    return &v.data[index];                                   \
  }

define_array(char, str)

str str_from_static(const char *s) {
    size_t len = strlen(s);
    str result;
    result.data = (char *) malloc(len);
    result.len = result.capacity = len;
    memcpy(result.data, s, len);
    return result;
}

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

str int_to_string(i32 x) {
    if (x == INT_MIN) {
        return str_from_static("-2147483648");
    }
    if (x == 0) {
        return str_from_static("0");
    }

    str s;
    init_with_cap_str(&s, 11);

    char neg = 0;
    if (x < 0) {
        neg = 1;
        x = -x;
    }
    while (x) {
        push_str(&s, '0' + (x % 10));
        x /= 10;
    }
    if (neg) {
        push_str(&s, '-');
    }

    for (size_t l = 0, r = s.len - 1; l < r; ++l, --r) {
        char t = s.data[l];
        s.data[l] = s.data[r];
        s.data[r] = t;
    }
    return s;
}

str str_add(str a, str b) {
    str result;
    init_with_cap_str(&result, a.len + b.len);

    memcpy(result.data, a.data, a.len);
    memcpy(result.data + a.len, b.data, b.len);
    result.len = a.len + b.len;

    return result;
}

char *str_index(str s, i32 i) {
    return &s.data[i];
}

char str_eq(str a, str b) {
    return a.len == b.len && memcmp(a.data, b.data, a.len) == 0;
}

char str_neq(str a, str b) {
    return !str_eq(a, b);
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
    rwctx.line = NULL;

    char *buf = NULL;
    size_t len = 0;
    while (!len) {
        free(buf);
        len = raw_readln(&buf);
    }

    target->data = buf;
    target->len = target->capacity = len;
}

void readword_str(str *target) {
    char *buf;
    size_t len;
    raw_readword(&buf, &len);

    target->data = buf;
    target->len = target->capacity = len;
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
    if (!s.len) {
        return;
    }

    char last_char = s.data[s.len - 1];
    s.data[s.len - 1] = '\0';

    if (fputs(s.data, stdout) < 0) {
        perror("Runtime error when writing to stdout: ");
        exit(1);
    }

    s.data[s.len - 1] = last_char;
    if (fputc(last_char, stdout) < 0) {
        perror("Runtime error when writing to stdout: ");
        exit(1);
    }
}
