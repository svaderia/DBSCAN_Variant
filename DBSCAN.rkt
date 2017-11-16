#lang racket
; read file and make a list of strings
(define f (vector-ref (current-command-line-arguments) 0))
(define data (file->lines f))

; take the list of string and convert it into list of integer 
(define (make_ints lst acc)
  (if (null? lst) (reverse acc)
      (make_ints (cdr lst) (cons (string->number (car lst)) acc))
))

; use make_ints to convert list of strings to list of list of integers 
(define (read_points lst acc)
  (if (null? lst) (reverse acc)
      (read_points (cdr lst) (cons (make_ints (regexp-split #px" " (car lst)) '()) acc))
))

; defining values of the variables
(define temp (read_points data '()))
(define values (car temp))
(define points (cdr temp))
(define N (car values))
(define D (car (cdr values)))
(define K (car (cdr (cdr values))))
(define e (car (cdr (cdr (cdr values)))))
(define MinPts (car (cdr (cdr (cdr (cdr values))))))

; corelate each point with it's dimension as given in first part of the assignment
(define (pts_dim ls count acc)
  (if (null? ls)
      (reverse acc)
      (pts_dim (cdr ls) (+ count 1) (cons (list count (car ls)) acc)))
)
(define step1 (pts_dim points '1 '()))
(provide step1)

; calculated Euclidean distance between two points 
(define (eucl_distance2 ls1 ls2 acc)
  (cond
    ((null? ls1)  (exact->inexact (sqrt acc)))
    ((eq? ls1 ls2) +inf.0)
    (else (eucl_distance2 (cdr ls1) (cdr ls2) (+ acc (expt (- (car ls1) (car ls2)) 2))))
))

; take a point and returns a list having euclean distance of that point with all other points, (a point is list of it's dimension)
(define (eucl_distance pt ls acc)
  (if (null? ls) (reverse acc)
      (eucl_distance pt (cdr ls) (cons (list (car (car ls)) (eucl_distance2 (list-ref pt 1) (list-ref (car ls) 1) 0)) acc))
  )
)

; list of list of euclidean distance
(define (eucl_list ls1 ls2 acc)
  (if (null? ls1) (reverse acc)
      (eucl_list (cdr ls1) ls2 (cons (eucl_distance (car ls1) ls2 '()) acc)))
)
(define mstep2 (eucl_list step1 step1 '()))

; setting precision to 6 digits 
(define precision '6)

(define (mysetprecision n p)
  (if (= n +inf.0) +inf.0
      (string->number (~r n #:precision p))
  )
) 

(define (precision_util lst)
  (if (null? lst) '()
      (cons (list (car(car lst)) (mysetprecision (car(cdr(car lst))) precision))  (precision_util (cdr lst))))
)

(define (modify_precision lst)
  (if (null? lst) '()
  (cons (precision_util (car lst)) (modify_precision (cdr lst))))
)

(define step2 (modify_precision mstep2))
(provide step2)

; sort in assending order
(define (my_sort_asc ls)
  (sort ls
        (lambda (x y) (< (list-ref x 1) (list-ref y 1))))
)
; sort in desending order
(define (my_sort_des ls)
  (sort ls
        (lambda (x y) (> (list-ref x 1) (list-ref y 1))))
)

; sort the euclidean distance list in assending order of distance for each point
(define (sort_eucl_lst ls acc)
  (if (null? ls)  (reverse acc)
      (sort_eucl_lst (cdr ls) (cons  (my_sort_asc (car ls)) acc))
      )
)
(define sorted_eucl_lst (sort_eucl_lst step2 '()))


; chose first K points in the sorted list
(define (chooseK ls acc K)
  (if (= K 0) (sort acc <)
      (chooseK (cdr ls) (cons (list-ref (car ls) 0) acc) (- K 1)))
)
(define (choose_k ls acc K)
  (if (null? ls)  (reverse acc)
      (choose_k (cdr ls) (cons (chooseK (car ls) '() K) acc) K))
)
(define KNN (choose_k sorted_eucl_lst '() K))
(define step3 KNN)
(provide step3)

; count number of members in intersection of two lists
(define (weight ls1 ls2 count)
  (cond
    ((null? ls1) count)
    ((member (car ls1) ls2) (weight (cdr ls1) ls2 (+ count 1)))
    (else (weight (cdr ls1) ls2 count))
))

; given point, KNN[point] and KNN construct edges for that point
(define (edges point Kpoint KNN acc)
  (cond
    ((null? Kpoint) (my_sort_des (reverse acc)))
    ((member point (list-ref KNN (- (car Kpoint) 1))) (edges point (cdr Kpoint) KNN (cons (list (car Kpoint) (weight (list-ref KNN (- point 1)) (list-ref KNN (- (car Kpoint) 1)) 0)) acc)))
    (else (edges point (cdr Kpoint) KNN acc))
    )
)

(define (construct_graph point KNN1 KNN2 acc)
  (if (null? KNN1) (reverse acc)
      (construct_graph (+ point 1) (cdr KNN1) KNN2 (cons (edges point (car KNN1) KNN2 '()) acc))
      )
)

(define graph (construct_graph 1 KNN KNN '()))
(define step4 graph)
(provide step4)

; finding density of a points
(define (count_density ls e count)
  (cond
    ((null? ls) count)
    ((>= (list-ref (car ls) 1) e) (count_density (cdr ls) e (+ count 1)))
    (else (count_density (cdr ls) e count))
))

; calculating density of all the points
(define (density_graph graph e acc)
  (if (null? graph) (reverse acc)
      (density_graph (cdr graph) e (cons (count_density (car graph) e 0) acc))
     )
)
(define density (density_graph graph e '()))
(define step5 density)
(provide step5)

; finding core points
(define (core density MinPts count acc)
  (cond
    ((null? density) (sort acc <))
    ((>= (car density) MinPts) (core (cdr density) MinPts (+ 1 count) (cons count acc)))
    (else (core (cdr density) MinPts (+ 1 count) acc))
))
(define core_points (core density MinPts '1 '() ))
(define step6 core_points)
(provide step6)

; removing edges from the graph that have weight < e
(define (remove_edges ls e acc)
  (cond
    ((null? ls) (reverse acc))
    ((>= (list-ref (car ls) 1) e) (remove_edges (cdr ls) e (cons (list-ref (car ls) 0) acc)))
    (else (remove_edges (cdr ls) e acc))
  )
)

; remove points that are not core points from the list
(define (remove_noncore core_points ls acc)
  (cond
    ((null? ls) (sort acc <))
    ((member (car ls) core_points) (remove_noncore core_points (cdr ls) (cons (car ls) acc)))
    (else (remove_noncore core_points (cdr ls) acc))
   )
)

(define (cluster_graph graph e acc core_points)
  (if (null? graph) (reverse acc)
      (cluster_graph (cdr graph) e (cons (remove_noncore core_points (remove_edges (car graph) e '()) '()) acc) core_points)
      )
)
(define cgraph (cluster_graph graph e '() core_points))

; making cluster containing given point
(define (cluster cgraph visited stack)
  (cond
    ((null? stack) visited)
    ((not (member (car stack) visited)) (cluster cgraph (cons (car stack) visited) (append (cdr stack) (list-ref cgraph (- (car stack) 1)))))
    (else (cluster cgraph visited (cdr stack)))
    )
)
(define (Cluster cgraph point core_points)
  (remove_noncore core_points (cluster cgraph '() (list point)) '())
)

; remove ls2 from ls1
(define (remove_list ls1 ls2 acc)
  (cond
    ((null? ls1) (reverse acc))
    ((member (car ls1) ls2) (remove_list (cdr ls1) ls2 acc))
    (else (remove_list (cdr ls1) ls2 (cons (car ls1) acc)))
))

(define (cluster_id cgraph core_points count acc)
  (cond
    ((null? core_points) (reverse acc))
    (else (cluster_id cgraph (remove_list core_points (Cluster cgraph (car core_points) core_points) '()) (+ count 1) (cons (list count (Cluster cgraph (car core_points) core_points)) acc)))
))

(define core_cluster (cluster_id cgraph core_points '1 '()))
(define step7 core_cluster)
(provide step7)

(define (noise_graph graph e acc )
  (if (null? graph) (reverse acc)
      (noise_graph (cdr graph) e (cons  (remove_edges (car graph) e '()) acc) )
      )
)
(define ngraph (noise_graph graph e '()))

; finding noise_points
(define (noise_points cgraph count N core_points acc)
  (cond
    ((> count N) (sort acc < ))
    ((and (not (member count core_points)) (null? (list-ref cgraph (- count 1)))) (noise_points cgraph (+ count 1) N core_points (cons count acc)))
    (else (noise_points cgraph (+ count 1) N core_points acc))
   )
)
(define noise (noise_points ngraph '1 N core_points '()))
(define step8 noise)
(provide step8)

; finding border_points 
(define (border_points noise count N core_points acc)
  (cond
    ((> count N) (sort acc <))
    ((and (not (member count noise)) (not (member count core_points))) (border_points noise (+ count 1) N core_points (cons count acc)))
    (else (border_points noise (+ count 1) N core_points acc))
))

(define border (border_points noise '1 N core_points '()))
(define step9 border)
(provide step9)

; merging border points with the cluster
(define (max_core core_points KNN point)
  (car (sort core_points
        (lambda (x y) (> (weight (list-ref KNN (- x 1)) (list-ref KNN (- point 1)) 0) (weight (list-ref KNN (- y 1)) (list-ref KNN (- point 1)) 0))))
))


(define (assign_cluster core_cluster point max_point acc)
  (cond
    ((null? core_cluster) (reverse acc))
    ((member max_point (list-ref (car core_cluster) 1)) (assign_cluster (cdr core_cluster) point max_point  (cons (list (list-ref (car core_cluster) 0) (cons point (list-ref (car core_cluster) 1))) acc)))
    (else (assign_cluster (cdr core_cluster) point max_point (cons (car core_cluster)acc)))
   )
)
(define (sort_cluster cluster acc)
  (if (null? cluster) (reverse acc)
      (sort_cluster (cdr cluster) (cons (list (list-ref (car cluster) 0) (sort (list-ref (car cluster) 1) <)) acc))
  )
)

(define (Final_Cluster core_cluster border)
  (if (null? (cdr border))
    (sort_cluster (assign_cluster core_cluster (car border) (max_core core_points KNN (car border)) '()) '())
    (Final_Cluster (assign_cluster core_cluster (car border) (max_core core_points KNN (car border)) '()) (cdr border)))
)

(define step10 (Final_Cluster core_cluster border))
(provide step10)