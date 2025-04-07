(#%require-dylib "libsteel_playpen"
  (only-in mem-info
    MemoryInfo-total
    MemoryInfo-avail
    MemoryInfo-free
    MemoryInfo-buffers
    MemoryInfo-cached
    MemoryInfo-swap-total
    MemoryInfo-swap-free))

(provide current-memory-usage memory-usage-as-percentage)

(define (current-memory-usage #:memory-info (memory-info (mem-info)))
  (- (MemoryInfo-total memory-info) (MemoryInfo-free memory-info) (MemoryInfo-cached memory-info)))

(define (memory-usage-as-percentage #:memory-info (memory-info (mem-info)))
  (/ (current-memory-usage #:memory-info memory-info) (MemoryInfo-total memory-info)))
