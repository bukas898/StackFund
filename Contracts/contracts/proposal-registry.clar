;; proposal-registry.clar
;; Enhanced proposal registry with submission fees and voting periods

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-proposal (err u101))
(define-constant err-proposal-not-found (err u102))
(define-constant err-already-finalized (err u103))
(define-constant err-insufficient-fee (err u104))

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var proposal-submission-fee uint u500000) ;; 0.5 STX in microSTX
(define-data-var voting-period uint u1440) ;; Default 1440 blocks (~10 days)

;; Proposal Status Types
(define-constant status-pending u0)
(define-constant status-active u1)
(define-constant status-approved u2)
(define-constant status-rejected u3)

;; Data Maps
(define-map proposals
  uint
  {
    title: (string-utf8 100),
    description: (string-utf8 300),
    proposer: principal,
    requested-amount: uint,
    status: uint,
    created-at: uint,
    voting-ends-at: uint,
    category: (string-utf8 50)
  }
)

(define-map proposal-metadata
  uint
  {
    tags: (list 5 (string-utf8 20)),
    external-link: (optional (string-utf8 200))
  }
)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-proposal-metadata (proposal-id uint))
  (map-get? proposal-metadata proposal-id)
)

(define-read-only (get-proposal-counter)
  (var-get proposal-counter)
)

(define-read-only (get-submission-fee)
  (var-get proposal-submission-fee)
)

(define-read-only (get-voting-period)
  (var-get voting-period)
)

(define-read-only (is-proposal-active (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (and 
      (is-eq (get status proposal) status-active)
      (< block-height (get voting-ends-at proposal))
    )
    false
  )
)

(define-read-only (is-voting-ended (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal (>= block-height (get voting-ends-at proposal))
    true
  )
)

;; Private functions
(define-private (is-valid-proposal (title (string-utf8 100)) (description (string-utf8 300)) (amount uint))
  (and 
    (> (len title) u0)
    (> (len description) u10) ;; Require more detailed description
    (> amount u0)
    (<= amount u100000000000) ;; Max 100,000 STX
  )
)

;; Public functions
(define-public (submit-proposal 
  (title (string-utf8 100)) 
  (description (string-utf8 300)) 
  (requested-amount uint)
  (category (string-utf8 50))
  (tags (list 5 (string-utf8 20)))
  (external-link (optional (string-utf8 200)))
)
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (fee (var-get proposal-submission-fee))
      (voting-duration (var-get voting-period))
    )
    ;; Validate proposal
    (asserts! (is-valid-proposal title description requested-amount) err-invalid-proposal)
    
    ;; Transfer submission fee
    (try! (stx-transfer? fee tx-sender contract-owner))
    
    ;; Create proposal
    (map-set proposals proposal-id {
      title: title,
      description: description,
      proposer: tx-sender,
      requested-amount: requested-amount,
      status: status-pending,
      created-at: block-height,
      voting-ends-at: (+ block-height voting-duration),
      category: category
    })
    
    ;; Store metadata
    (map-set proposal-metadata proposal-id {
      tags: tags,
      external-link: external-link
    })
    
    ;; Increment counter
    (var-set proposal-counter proposal-id)
    
    (ok proposal-id)
  )
)

;; Admin functions
(define-public (activate-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
    )
    ;; Only owner can activate
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    ;; Must be pending
    (asserts! (is-eq (get status proposal) status-pending) err-already-finalized)
    
    ;; Update status to active
    (map-set proposals proposal-id 
      (merge proposal { status: status-active })
    )
    
    (ok true)
  )
)

(define-public (finalize-proposal (proposal-id uint) (approved bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
    )
    ;; Only owner can finalize
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    ;; Must be active and voting ended
    (asserts! (is-eq (get status proposal) status-active) err-already-finalized)
    (asserts! (is-voting-ended proposal-id) err-invalid-proposal)
    
    ;; Update status
    (map-set proposals proposal-id 
      (merge proposal { 
        status: (if approved status-approved status-rejected) 
      })
    )
    
    (ok true)
  )
)

(define-public (update-submission-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u10000000) err-invalid-proposal) ;; Max 10 STX fee
    (var-set proposal-submission-fee new-fee)
    (ok true)
  )
)

(define-public (update-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (and (>= new-period u144) (<= new-period u4320)) err-invalid-proposal) ;; 1-30 days
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (emergency-pause-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
    )
    ;; Only owner can pause
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    ;; Must be active
    (asserts! (is-eq (get status proposal) status-active) err-already-finalized)
    
    ;; Set voting end to current block (effectively pausing)
    (map-set proposals proposal-id 
      (merge proposal { voting-ends-at: block-height })
    )
    
    (ok true)
  )
)