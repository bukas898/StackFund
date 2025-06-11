;; treasury.clar: Basic Treasury Management
;; Simple fund custody with basic deposit and withdrawal functionality

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-insufficient-funds (err u301))
(define-constant err-invalid-amount (err u302))

;; Data Variables
(define-data-var treasury-balance uint u0)

;; Data Maps
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

(define-read-only (get-deposit-info (deposit-id uint))
  (map-get? deposits deposit-id)
)

(define-read-only (get-deposit-count)
  (var-get deposit-counter)
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
    
    ;; Update balance and counter
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (var-set deposit-counter deposit-id)
    
    (ok deposit-id)
  )
)

(define-public (withdraw-funds (amount uint) (recipient principal))
  (begin
    ;; Only owner can withdraw funds
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Validate amount
    (asserts! (> amount u0) err-invalid-amount)
    
    ;; Check sufficient funds
    (asserts! (>= (var-get treasury-balance) amount) err-insufficient-funds)
    
    ;; Transfer funds to recipient
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    ;; Update balance
    (var-set treasury-balance (- (var-get treasury-balance) amount))
    
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