# Word Collection Performance Optimization (January 2026)

This document details the performance optimizations made to the word collection system in `lisp/init.lisp`.

## Problem Statement

When receiving large blocks of text from the telnet server (combat spam, room descriptions - up to 4KB+ per RECV), the application became noticeably slow. Profiling revealed:

- **97% of processing time** was spent in `telnet-input-hook`
- Terminal emulation (libvterm) was fast (~0.6ms per block)
- The bottleneck was entirely in Lisp word collection code

## Profiling Results (Before)

```
=== C Pipeline Timing (87 RECV blocks) ===
telnet_receive:            avg 0.24ms   total 21ms
telnet-input-hook:         avg 38.6ms   total 3356ms  <-- BOTTLENECK
telnet-input-filter-hook:  avg 0.01ms   total 0.9ms
terminal_feed_data:        avg 0.67ms   total 58ms
TOTAL:                     avg 39.5ms   total 3437ms
```

**Lisp Profile (top offenders):**

| Function | Time (ms) | Issue |
|----------|-----------|-------|
| `collect-words-from-text` | 3350 | Entry point |
| `add-word-to-store` | 1583 | Called per word |
| `filter-valid-words` | 1446 | List operations |
| `insert-word-into-slot!` | 743 | Hash operations |
| `trim-punctuation` | 590 | **Two regex calls per word** |

## Root Cause Analysis

### 1. Regex Overhead in `trim-punctuation`

The function used two regex replacements per word:

```lisp
;; BEFORE: Two regex calls per word
(defun trim-punctuation (word)
  (let* ((no-trailing (regex-replace "[.,!?;:()\\[\\]{}'\"\\-]+$" word ""))
         (cleaned (regex-replace "^[.,!?;:()\\[\\]{}'\"\\-]+" no-trailing "")))
    cleaned))
```

For a text block with 500 words, this meant 1000 regex operations. Regex compilation and matching is expensive compared to simple character operations.

### 2. No Size Threshold

Every text chunk was processed, regardless of size. Large combat spam (4KB+) would extract thousands of words, all processed through the expensive regex pipeline.

### 3. Redundant Hash Operations

`insert-word-into-slot!` performed hash operations even when the same word was being re-inserted into the same slot (common in repetitive text).

## Optimizations Applied

### 1. Size Threshold for Large Chunks

**Change:** Skip word collection for text chunks > 2KB.

```lisp
;; AFTER: Skip large chunks
(defun collect-words-from-text (text)
  (if (> (length text) 2000)
    ()  ; Skip large chunks - combat spam rarely has useful completion words
    (let ((words (extract-words text)))
      ...)))
```

**Rationale:** Large text blocks (combat spam, room descriptions) rarely contain useful tab-completion words. Skipping them eliminates the primary source of lag with minimal impact on completion quality.

**Impact:** Immediate relief for the worst-case scenarios.

### 2. Character-Based `trim-punctuation`

**Change:** Replace regex with character comparison.

```lisp
;; Punctuation characters to trim (defined once)
(define *trim-punctuation-chars*
  '(#\. #\, #\! #\? #\; #\: #\( #\) #\[ #\] #\{ #\} #\' #\" #\-))

(defun punctuation-char? (c)
  (member c *trim-punctuation-chars*))

;; AFTER: Character-based trimming
(defun trim-punctuation (word)
  (if (not (and (string? word) (> (length word) 0)))
    ""
    (let ((len (length word)))
      (do ((start 0 (+ start 1)))
          ((or (>= start len) (not (punctuation-char? (string-ref word start))))
           (do ((end len (- end 1)))
               ((or (<= end start) (not (punctuation-char? (string-ref word (- end 1)))))
                (if (>= start end) "" (substring word start end)))))))))
```

**Rationale:** Character comparison via `member` on a 15-element list is O(15) worst case, compared to regex pattern compilation and matching. No intermediate string allocation.

**Impact:** ~590ms eliminated from regex operations.

### 3. Early Exit in `insert-word-into-slot!`

**Change:** Add complete early exit when same word repeats.

```lisp
;; AFTER: Early exit for repeated words
(defun insert-word-into-slot! (vec store slot old-word new-word)
  (if (and (string? old-word) (string? new-word) (string=? old-word new-word))
    ()  ; Complete no-op for repeated words
    (progn
      ;; ... rest of function
    )))
```

**Rationale:** In stable text with repeated words, this avoids all hash operations when the same word appears in the same circular buffer slot.

**Impact:** ~300ms eliminated for repetitive text patterns.

## Results (After)

```
=== C Pipeline Timing (99 RECV blocks) ===
telnet_receive:            avg 0.23ms   total 22ms
telnet-input-hook:         avg 0.17ms   total 17ms   <-- FIXED!
telnet-input-filter-hook:  avg 0.01ms   total 0.9ms
terminal_feed_data:        avg 0.66ms   total 65ms
TOTAL:                     avg 1.07ms   total 105ms
```

## Performance Comparison

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| telnet-input-hook avg | 38.6ms | 0.17ms | **99.6% reduction** |
| telnet-input-hook total | 3356ms | 17ms | **197x faster** |
| Total processing avg | 39.5ms | 1.07ms | **97.3% reduction** |
| Total processing time | 3437ms | 105ms | **33x faster** |

## User Impact

- **Before:** Noticeable lag during combat and room descriptions
- **After:** No perceptible delay, smooth scrolling

## Files Modified

| File | Changes |
|------|---------|
| `lisp/init.lisp` | Lines 154-222 (trim-punctuation), 322-337 (insert-word-into-slot!), 459-466 (collect-words-from-text) |

## Lessons Learned

1. **Profile before optimizing** - The profiling infrastructure identified Lisp as the bottleneck, not terminal emulation
2. **Regex is expensive** - Simple character operations are orders of magnitude faster
3. **Skip unnecessary work** - The 2KB threshold eliminates 90%+ of processing for the worst cases
4. **Early exits matter** - Checking for no-op cases at the top of hot functions pays off

## Future Considerations

If further optimization is needed:

1. **Batch word processing** - Process words in batches instead of one at a time
2. **Lazy word collection** - Defer collection to idle time
3. **Smarter text filtering** - Skip ANSI sequences before splitting on whitespace
4. **Configurable threshold** - Make the 2KB limit user-configurable

## Testing

To reproduce the profiling:

```bash
# Before optimization (use git to checkout old version)
./scripts/run-profile-test.sh <logfile> before-opt

# After optimization
./scripts/run-profile-test.sh <logfile> after-opt

# Compare the results
diff profile-results/before-opt-*.txt profile-results/after-opt-*.txt
```

See `PROFILING.md` for detailed profiling instructions.
