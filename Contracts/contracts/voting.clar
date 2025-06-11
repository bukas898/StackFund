;; voting.clar - Stage 1: Basic Voting Implementation
;; Simple voting mechanism for StackFund proposals

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-already-voted (err u201))
(define-constant err-invalid-proposal (err u204))

;; Data Variables
(define-data-var proposal-counter uint u0)

;; Data Maps
;; Track votes per proposal
(define-map proposal-votes
  uint  ;; proposal-id
  {
    total-yes: uint,
    total-no: uint,
    total-voters: uint
  }
)

;; Track individual votes (simple boolean voting)
(define-map user-votes
  { proposal-id: uint, voter: principal }
  bool ;; true for yes, false for no
)

;; Basic proposal info
(define-map proposals
  uint ;; proposal-id
  {
    title: (string-ascii 100),
    creator: principal,
    active: bool
  }
)

;; Read-only functions
(define-read-only (get-proposal-votes (proposal-id uint))
  (default-to 
    { total-yes: u0, total-no: u0, total-voters: u0 }
    (map-get? proposal-votes proposal-id)
  )
)

(define-read-only (get-user-vote (proposal-id uint) (voter principal))
  (map-get? user-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? user-votes { proposal-id: proposal-id, voter: voter }))
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-proposal-counter)
  (var-get proposal-counter)
)

;; Private functions
(define-private (is-proposal-active (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (get active proposal)
    false
  )
)

;; Public functions
(define-public (create-proposal (title (string-ascii 100)))
  (let
    (
      (new-id (+ (var-get proposal-counter) u1))
    )
    ;; Create proposal
    (map-set proposals new-id
      {
        title: title,
        creator: tx-sender,
        active: true
      }
    )
    
    ;; Initialize vote counts
    (map-set proposal-votes new-id
      {
        total-yes: u0,
        total-no: u0,
        total-voters: u0
      }
    )
    
    ;; Update counter
    (var-set proposal-counter new-id)
    
    (ok new-id)
  )
)

(define-public (cast-vote (proposal-id uint) (vote-for bool))
  (let
    (
      (current-votes (get-proposal-votes proposal-id))
    )
    ;; Check if proposal exists and is active
    (asserts! (is-proposal-active proposal-id) err-invalid-proposal)
    
    ;; Check if already voted
    (asserts! (not (has-voted proposal-id tx-sender)) err-already-voted)
    
    ;; Record vote
    (map-set user-votes 
      { proposal-id: proposal-id, voter: tx-sender }
      vote-for
    )
    
    ;; Update vote totals
    (map-set proposal-votes proposal-id
      {
        total-yes: (if vote-for 
          (+ (get total-yes current-votes) u1)
          (get total-yes current-votes)
        ),
        total-no: (if vote-for
          (get total-no current-votes)
          (+ (get total-no current-votes) u1)
        ),
        total-voters: (+ (get total-voters current-votes) u1)
      }
    )
    
    (ok true)
  )
)

(define-public (close-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) err-invalid-proposal))
    )
    ;; Only creator or contract owner can close
    (asserts! 
      (or 
        (is-eq tx-sender (get creator proposal))
        (is-eq tx-sender contract-owner)
      ) 
      err-invalid-proposal
    )
    
    ;; Mark as inactive
    (map-set proposals proposal-id
      (merge proposal { active: false })
    )
    
    (ok true)
  )
)