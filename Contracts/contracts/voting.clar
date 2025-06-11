;; voting.clar
;; Handles voting mechanism for StackFund proposals

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-active (err u200))
(define-constant err-already-voted (err u201))
(define-constant err-no-stake (err u202))
(define-constant err-voting-ended (err u203))
(define-constant err-invalid-proposal (err u204))

;; Data Variables
(define-data-var min-stake-required uint u1000000) ;; 1 STX minimum to vote
(define-data-var quorum-threshold uint u10) ;; 10% of votes needed

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

;; Track individual votes
(define-map user-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, amount: uint }
)

;; Track staked STX for voting weight
(define-map user-stakes
  principal
  uint
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

(define-read-only (get-user-stake (user principal))
  (default-to u0 (map-get? user-stakes user))
)

(define-read-only (has-voted (proposal-id uint) (voter principal))
  (is-some (map-get? user-votes { proposal-id: proposal-id, voter: voter }))
)

(define-read-only (calculate-vote-percentage (proposal-id uint))
  (let
    (
      (votes (get-proposal-votes proposal-id))
      (total (+ (get total-yes votes) (get total-no votes)))
    )
    (if (is-eq total u0)
      { yes-percentage: u0, no-percentage: u0 }
      {
        yes-percentage: (/ (* (get total-yes votes) u100) total),
        no-percentage: (/ (* (get total-no votes) u100) total)
      }
    )
  )
)

;; Private functions
(define-private (is-voting-active (proposal-id uint))
  ;; In MVP, we'll check with proposal registry contract
  ;; For now, simplified check
  (and 
    (> proposal-id u0)
    (<= proposal-id u1000) ;; arbitrary upper limit for safety
  )
)

;; Public functions
(define-public (stake-for-voting (amount uint))
  (let
    (
      (current-stake (get-user-stake tx-sender))
    )
    ;; Transfer STX to this contract for staking
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update stake
    (map-set user-stakes tx-sender (+ current-stake amount))
    
    (ok (+ current-stake amount))
  )
)

(define-public (unstake (amount uint))
  (let
    (
      (current-stake (get-user-stake tx-sender))
    )
    ;; Ensure user has enough stake
    (asserts! (>= current-stake amount) err-no-stake)
    
    ;; Transfer STX back to user
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Update stake
    (map-set user-stakes tx-sender (- current-stake amount))
    
    (ok (- current-stake amount))
  )
)

(define-public (cast-vote (proposal-id uint) (vote-for bool))
  (let
    (
      (voter-stake (get-user-stake tx-sender))
      (current-votes (get-proposal-votes proposal-id))
    )
    ;; Check if proposal is active
    (asserts! (is-voting-active proposal-id) err-not-active)
    
    ;; Check if already voted
    (asserts! (not (has-voted proposal-id tx-sender)) err-already-voted)
    
    ;; Check minimum stake
    (asserts! (>= voter-stake (var-get min-stake-required)) err-no-stake)
    
    ;; Record vote
    (map-set user-votes 
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-for, amount: voter-stake }
    )
    
    ;; Update vote totals
    (map-set proposal-votes proposal-id
      {
        total-yes: (if vote-for 
          (+ (get total-yes current-votes) voter-stake)
          (get total-yes current-votes)
        ),
        total-no: (if vote-for
          (get total-no current-votes)
          (+ (get total-no current-votes) voter-stake)
        ),
        total-voters: (+ (get total-voters current-votes) u1)
      }
    )
    
    (ok true)
  )
)

;; Delegate vote function for future use
(define-public (delegate-vote (proposal-id uint) (delegate-to principal))
  (let
    (
      (voter-stake (get-user-stake tx-sender))
    )
    ;; Check if proposal is active
    (asserts! (is-voting-active proposal-id) err-not-active)
    
    ;; Check if already voted
    (asserts! (not (has-voted proposal-id tx-sender)) err-already-voted)
    
    ;; For MVP, just record delegation without executing
    ;; Full implementation would transfer voting power
    (ok true)
  )
)

;; Admin functions
(define-public (update-min-stake (new-min uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-invalid-proposal)
    (var-set min-stake-required new-min)
    (ok true)
  )
)

(define-public (update-quorum (new-quorum uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-invalid-proposal)
    (asserts! (<= new-quorum u100) err-invalid-proposal) ;; Max 100%
    (var-set quorum-threshold new-quorum)
    (ok true)
  )
)