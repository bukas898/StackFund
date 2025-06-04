;; proposal-registry.clar
;; Basic proposal creation and storage for StackFund - Stage 1

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-proposal (err u101))
(define-constant err-proposal-not-found (err u102))

;; Data Variables
(define-data-var proposal-counter uint u0)

;; Proposal Status Types
(define-constant status-pending u0)
(define-constant status-approved u1)
(define-constant status-rejected u2)

;; Data Maps
(define-map proposals
  uint
  {
    title: (string-utf8 100),
    description: (string-utf8 200),
    proposer: principal,
    requested-amount: uint,
    status: uint,
    created-at: uint
  }
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-proposal-counter)
  (var-get proposal-counter)
)

;; Private functions
(define-private (is-valid-proposal (title (string-utf8 100)) (description (string-utf8 200)) (amount uint))
  (and 
    (> (len title) u0)
    (> (len description) u0)
    (> amount u0)
  )
)

;; Public functions
(define-public (submit-proposal (title (string-utf8 100)) (description (string-utf8 200)) (requested-amount uint))
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
    )
    ;; Validate proposal
    (asserts! (is-valid-proposal title description requested-amount) err-invalid-proposal)
    
    ;; Create proposal
    (map-set proposals proposal-id {
      title: title,
      description: description,
      proposer: tx-sender,
      requested-amount: requested-amount,
      status: status-pending,
      created-at: block-height
    })
    
    ;; Increment counter
    (var-set proposal-counter proposal-id)
    
    (ok proposal-id)
  )
)

;; Admin functions
(define-public (approve-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
    )
    ;; Only owner can approve
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Update status to approved
    (map-set proposals proposal-id 
      (merge proposal { status: status-approved })
    )
    
    (ok true)
  )
)

(define-public (reject-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
    )
    ;; Only owner can reject
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Update status to rejected
    (map-set proposals proposal-id 
      (merge proposal { status: status-rejected })
    )
    
    (ok true)
  )
)