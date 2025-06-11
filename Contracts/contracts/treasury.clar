;; treasury.clar
;; Manages funds custody and releases for approved proposals

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-insufficient-funds (err u301))
(define-constant err-already-claimed (err u302))
(define-constant err-not-approved (err u303))
(define-constant err-invalid-amount (err u304))

;; Data Variables
(define-data-var treasury-balance uint u0)
(define-data-var total-allocated uint u0)
(define-data-var withdrawal-fee uint u10000) ;; 0.01 STX fee

;; Data Maps
;; Track claimed proposals
(define-map proposal-claims
  uint  ;; proposal-id
  {
    claimed: bool,
    claimed-at: uint,
    amount: uint,
    recipient: principal
  }
)

;; Track treasury deposits
(define-map deposits
  uint  ;; deposit-id
  {
    depositor: principal,
    amount: uint,
    block-height: uint
  }
)

(define-data-var deposit-counter uint u0)

;; Read-only functions
(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-available-balance)
  (- (var-get treasury-balance) (var-get total-allocated))
)

(define-read-only (get-total-allocated)
  (var-get total-allocated)
)

(define-read-only (has-claimed (proposal-id uint))
  (default-to false 
    (get claimed (map-get? proposal-claims proposal-id))
  )
)

(define-read-only (get-claim-info (proposal-id uint))
  (map-get? proposal-claims proposal-id)
)

(define-read-only (get-withdrawal-fee)
  (var-get withdrawal-fee)
)

;; Public functions
(define-public (deposit-funds (amount uint))
  (let
    (
      (deposit-id (+ (var-get deposit-counter) u1))
    )
    ;; Validate amount
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Transfer STX to treasury
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Record deposit
    (map-set deposits deposit-id {
      depositor: tx-sender,
      amount: amount,
      block-height: block-height
    })
    
    ;; Update balances
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (var-set deposit-counter deposit-id)
    
    (ok deposit-id)
  )
)

(define-public (claim-funds (proposal-id uint) (recipient principal) (amount uint))
  (let
    (
      (available (get-available-balance))
      (fee (var-get withdrawal-fee))
      (total-needed (+ amount fee))
    )
    ;; For MVP, only owner can release funds (later will check proposal approval)
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Check if already claimed
    (asserts! (not (has-claimed proposal-id)) err-already-claimed)
    
    ;; Check sufficient funds
    (asserts! (>= available total-needed) err-insufficient-funds)
    
    ;; Transfer funds to recipient
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    ;; Transfer fee to owner (for maintenance)
    (try! (as-contract (stx-transfer? fee tx-sender contract-owner)))
    
    ;; Record claim
    (map-set proposal-claims proposal-id {
      claimed: true,
      claimed-at: block-height,
      amount: amount,
      recipient: recipient
    })
    
    ;; Update balance
    (var-set treasury-balance (- (var-get treasury-balance) total-needed))
    
    (ok true)
  )
)

(define-public (allocate-funds (proposal-id uint) (amount uint))
  (let
    (
      (available (get-available-balance))
    )
    ;; Only owner can allocate (later will be automatic after vote)
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Check sufficient funds
    (asserts! (>= available amount) err-insufficient-funds)
    
    ;; Update allocated amount
    (var-set total-allocated (+ (var-get total-allocated) amount))
    
    (ok true)
  )
)

(define-public (release-allocation (proposal-id uint) (amount uint))
  (begin
    ;; Only owner can release allocation
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Ensure we don't release more than allocated
    (asserts! (>= (var-get total-allocated) amount) err-invalid-amount)
    
    ;; Update allocated amount
    (var-set total-allocated (- (var-get total-allocated) amount))
    
    (ok true)
  )
)

;; Emergency functions
(define-public (emergency-withdraw (amount uint) (recipient principal))
  (begin
    ;; Only owner can do emergency withdrawal
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Check sufficient funds
    (asserts! (>= (var-get treasury-balance) amount) err-insufficient-funds)
    
    ;; Transfer funds
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    ;; Update balance
    (var-set treasury-balance (- (var-get treasury-balance) amount))
    
    (ok true)
  )
)

;; Admin functions
(define-public (update-withdrawal-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set withdrawal-fee new-fee)
    (ok true)
  )
)

;; Integration helper function for future use
(define-public (execute-approved-proposal (proposal-id uint) (recipient principal) (amount uint))
  (begin
    ;; This will eventually check with proposal registry for approval
    ;; For now, it just calls claim-funds
    (claim-funds proposal-id recipient amount)
  )
)