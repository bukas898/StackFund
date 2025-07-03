;; ===== MINIMAL PROPOSAL REGISTRY =====
;; proposal-registry-minimal.clar
(define-constant contract-owner tx-sender)
(define-constant err-invalid-proposal (err u101))
(define-constant err-proposal-not-found (err u102))
(define-constant err-owner-only (err u100))

(define-data-var proposal-counter uint u0)

(define-map proposals
  uint
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    proposer: principal,
    requested-amount: uint,
    status: uint,
    created-at: uint
  }
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-proposal-counter)
  (var-get proposal-counter)
)

(define-public (submit-proposal 
  (title (string-utf8 100)) 
  (description (string-utf8 500)) 
  (requested-amount uint)
)
  (let
    ((proposal-id (+ (var-get proposal-counter) u1)))
    ;; Simple validation
    (asserts! (> (len title) u0) err-invalid-proposal)
    (asserts! (> (len description) u10) err-invalid-proposal)
    (asserts! (> requested-amount u0) err-invalid-proposal)
    
    ;; Create proposal
    (map-set proposals proposal-id {
      title: title,
      description: description,
      proposer: tx-sender,
      requested-amount: requested-amount,
      status: u0,
      created-at: block-height
    })
    
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)
