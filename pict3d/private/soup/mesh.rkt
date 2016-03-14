#lang typed/racket/base

(require racket/match
         racket/list
         typed/safe/ops
         "../utils.rkt"
         "types.rkt")

(provide (all-defined-out))

(: mesh->faces (All (A B) (-> (Vectorof vtx) (Vectorof Index) A B (Listof (face A B)))))
(define (mesh->faces vtxs idxs data edge-data)
  (let loop : (Listof (face A B))
    ([als : (Listof (face A B)) null]
     [i : Natural 0])
    (cond
      [(< i (- (vector-length idxs) 2))
       (define vtx1 (vector-ref vtxs (safe-vector-ref idxs i)))
       (define vtx2 (vector-ref vtxs (safe-vector-ref idxs (+ i 1))))
       (define vtx3 (vector-ref vtxs (safe-vector-ref idxs (+ i 2))))
       (loop (cons (face vtx1 vtx2 vtx3 data edge-data edge-data edge-data) als)
             (+ i 3))]
      [else (reverse als)])))

(: faces->mesh (All (A B) (-> (Listof (face A B)) (Values (Vectorof vtx) (Vectorof Index)))))
(define (faces->mesh faces)
  (define vtxss
    (for/list : (Listof (Vectorof vtx)) ([f  (in-list faces)])
      (define-values (vtx1 vtx2 vtx3) (face-vtxs f))
      (vector vtx1 vtx2 vtx3)))
  
  (define idxss (make-list (length vtxss) ((inst vector Index) 0 1 2)))
  
  (indexed-vector-append vtxss idxss))
