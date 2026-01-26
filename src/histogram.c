/* Performance histogram for timing distributions with percentile statistics */

#include "histogram.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct Histogram {
    char *name;
    uint64_t *samples;     /* circular buffer */
    size_t capacity;       /* max samples to retain */
    size_t count;          /* current number of samples */
    size_t head;           /* next write position */
    size_t print_interval; /* auto-print every N records (0 = disabled) */
    size_t record_count;   /* total records since last auto-print */
};

Histogram *histogram_create(const char *name, size_t capacity, size_t print_interval) {
    if (capacity == 0) {
        capacity = HISTOGRAM_DEFAULT_CAPACITY;
    }

    Histogram *h = malloc(sizeof(Histogram));
    if (!h)
        return NULL;

    h->name = strdup(name ? name : "unnamed");
    if (!h->name) {
        free(h);
        return NULL;
    }

    h->samples = malloc(capacity * sizeof(uint64_t));
    if (!h->samples) {
        free(h->name);
        free(h);
        return NULL;
    }

    h->capacity = capacity;
    h->count = 0;
    h->head = 0;
    h->print_interval = print_interval;
    h->record_count = 0;

    return h;
}

void histogram_destroy(Histogram *h) {
    if (!h)
        return;
    free(h->samples);
    free(h->name);
    free(h);
}

/* Comparison function for qsort */
static int compare_uint64(const void *a, const void *b) {
    uint64_t va = *(const uint64_t *)a;
    uint64_t vb = *(const uint64_t *)b;
    if (va < vb)
        return -1;
    if (va > vb)
        return 1;
    return 0;
}

void histogram_get_stats(const Histogram *h, HistogramStats *stats) {
    if (!h || !stats)
        return;

    memset(stats, 0, sizeof(HistogramStats));
    stats->count = h->count;

    if (h->count == 0)
        return;

    /* Copy samples and sort for percentile calculation */
    uint64_t *sorted = malloc(h->count * sizeof(uint64_t));
    if (!sorted)
        return;

    /* Copy from circular buffer */
    if (h->count < h->capacity) {
        /* Buffer not full yet, samples are at indices 0..count-1 */
        memcpy(sorted, h->samples, h->count * sizeof(uint64_t));
    } else {
        /* Buffer is full, head points to oldest sample */
        size_t first_part = h->capacity - h->head;
        memcpy(sorted, h->samples + h->head, first_part * sizeof(uint64_t));
        memcpy(sorted + first_part, h->samples, h->head * sizeof(uint64_t));
    }

    qsort(sorted, h->count, sizeof(uint64_t), compare_uint64);

    stats->min = sorted[0];
    stats->max = sorted[h->count - 1];

    /* Percentile indices (0-based) */
    size_t p50_idx = (h->count * 50) / 100;
    size_t p90_idx = (h->count * 90) / 100;
    size_t p99_idx = (h->count * 99) / 100;

    /* Clamp to valid range */
    if (p50_idx >= h->count)
        p50_idx = h->count - 1;
    if (p90_idx >= h->count)
        p90_idx = h->count - 1;
    if (p99_idx >= h->count)
        p99_idx = h->count - 1;

    stats->p50 = sorted[p50_idx];
    stats->p90 = sorted[p90_idx];
    stats->p99 = sorted[p99_idx];

    free(sorted);
}

void histogram_print(const Histogram *h) {
    if (!h)
        return;

    HistogramStats stats;
    histogram_get_stats(h, &stats);

    if (stats.count == 0) {
        printf("[%s] n=0 (no samples)\n", h->name);
        return;
    }

    /* Convert nanoseconds to milliseconds for display */
    printf("[%s] n=%zu min=%.3fms p50=%.3fms p90=%.3fms p99=%.3fms max=%.3fms\n", h->name, stats.count,
           (double)stats.min / 1e6, (double)stats.p50 / 1e6, (double)stats.p90 / 1e6, (double)stats.p99 / 1e6,
           (double)stats.max / 1e6);
}

int histogram_record(Histogram *h, uint64_t sample_ns) {
    if (!h)
        return 0;

    /* Insert sample into circular buffer */
    h->samples[h->head] = sample_ns;
    h->head = (h->head + 1) % h->capacity;
    if (h->count < h->capacity) {
        h->count++;
    }

    h->record_count++;

    /* Auto-print if interval is set and reached */
    if (h->print_interval > 0 && h->record_count >= h->print_interval) {
        histogram_print(h);
        h->record_count = 0;
        return 1;
    }

    return 0;
}

void histogram_reset(Histogram *h) {
    if (!h)
        return;
    h->count = 0;
    h->head = 0;
    h->record_count = 0;
}
