
;; ===== MINIMAL VOTING =====
;; voting-minimal.clar
(define-constant contract-owner tx-sender)
(define-constant err-already-voted (err u201))

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, amount: uint }
)

(define-map proposal-votes
  uint
  { total-yes: uint, total-no: uint }
)

(define-read-only (get-votes (proposal-id uint))
  (default-to 
    { total-yes: u0, total-no: u0 }
    (map-get? proposal-votes proposal-id)
  )
)

(define-public (cast-vote (proposal-id uint) (vote-for bool))
  (let
    ((current-votes (get-votes proposal-id))
     (vote-weight u1000000)) ;; 1 STX weight
    
    ;; Check if already voted
    (asserts! (is-none (map-get? votes { proposal-id: proposal-id, voter: tx-sender })) 
              err-already-voted)
    
    ;; Record vote
    (map-set votes 
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-for, amount: vote-weight }
    )
    
    ;; Update totals
    (map-set proposal-votes proposal-id
      (if vote-for
        (merge current-votes { total-yes: (+ (get total-yes current-votes) vote-weight) })
        (merge current-votes { total-no: (+ (get total-no current-votes) vote-weight) })
      )
    )
    
    (ok true)
  )
)

