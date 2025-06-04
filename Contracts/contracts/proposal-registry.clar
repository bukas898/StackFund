;; proposal-registry.clar
;; Proposal registry with execution tracking and integration - Stage 3

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-invalid-proposal (err u101))
(define-constant err-proposal-not-found (err u102))
(define-constant err-already-finalized (err u103))
(define-constant err-insufficient-fee (err u104))
(define-constant err-unauthorized (err u105))
(define-constant err-invalid-status (err u106))

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var proposal-submission-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var voting-period uint u1440) ;; Default 1440 blocks (~10 days)
(define-data-var min-proposal-amount uint u1000000) ;; Minimum 1 STX
(define-data-var max-proposal-amount uint u100000000000) ;; Maximum 100,000 STX

;; Proposal Status Types
(define-constant status-pending u0)
(define-constant status-active u1)
(define-constant status-approved u2)
(define-constant status-rejected u3)
(define-constant status-executed u4)
(define-constant status-cancelled u5)

;; Data Maps
(define-map proposals
  uint
  {
    title: (string-utf8 100),
    description: (string-utf8 500),
    proposer: principal,
    requested-amount: uint,
    status: uint,
    created-at: uint,
    voting-ends-at: uint,
    category: (string-utf8 50),
    priority: uint
  }
)

(define-map proposal-metadata
  uint
  {
    tags: (list 5 (string-utf8 20)),
    external-link: (optional (string-utf8 200)),
    milestones: (list 3 (string-utf8 100)),
    expected-duration: uint
  }
)

(define-map proposal-approvals uint bool)
(define-map proposal-execution-data
  uint
  {
    executed-at: uint,
    executed-by: principal,
    execution-tx: (string-utf8 100)
  }
)

;; Authorized contracts for proposal execution
(define-map authorized-contracts principal bool)

;; Read-only functions
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals proposal-id)
)

(define-read-only (get-proposal-metadata (proposal-id uint))
  (map-get? proposal-metadata proposal-id)
)

(define-read-only (get-proposal-execution-data (proposal-id uint))
  (map-get? proposal-execution-data proposal-id)
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

(define-read-only (get-proposal-limits)
  {
    min-amount: (var-get min-proposal-amount),
    max-amount: (var-get max-proposal-amount)
  }
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

(define-read-only (is-proposal-approved (proposal-id uint))
  (default-to false (map-get? proposal-approvals proposal-id))
)

(define-read-only (is-contract-authorized (contract principal))
  (default-to false (map-get? authorized-contracts contract))
)

(define-read-only (get-proposals-by-status (status uint))
  (filter is-status-match (list 
    u1 u2 u3 u4 u5 u6 u7 u8 u9 u10
    u11 u12 u13 u14 u15 u16 u17 u18 u19 u20
  ))
)

;; Private functions
(define-private (is-valid-proposal (title (string-utf8 100)) (description (string-utf8 500)) (amount uint))
  (and 
    (> (len title) u0)
    (> (len description) u20) ;; Require detailed description
    (>= amount (var-get min-proposal-amount))
    (<= amount (var-get max-proposal-amount))
  )
)

(define-private (is-status-match (proposal-id uint))
  (match (map-get? proposals proposal-id)
    proposal true
    false
  )
)

(define-private (is-authorized-caller)
  (or 
    (is-eq tx-sender contract-owner)
    (is-contract-authorized contract-caller)
  )
)

;; Public functions
(define-public (submit-proposal 
  (title (string-utf8 100)) 
  (description (string-utf8 500)) 
  (requested-amount uint)
  (category (string-utf8 50))
  (priority uint)
  (tags (list 5 (string-utf8 20)))
  (external-link (optional (string-utf8 200)))
  (milestones (list 3 (string-utf8 100)))
  (expected-duration uint)
)
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (fee (var-get proposal-submission-fee))
      (voting-duration (var-get voting-period))
    )
    ;; Validate proposal
    (asserts! (is-valid-proposal title description requested-amount) err-invalid-proposal)
    (asserts! (<= priority u3) err-invalid-proposal) ;; Priority 0-3
    (asserts! (<= expected-duration u8640) err-invalid-proposal) ;; Max 60 days
    
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
      category: category,
      priority: priority
    })
    
    ;; Store metadata
    (map-set proposal-metadata proposal-id {
      tags: tags,
      external-link: external-link,
      milestones: milestones,
      expected-duration: expected-duration
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
    ;; Only authorized can activate
    (asserts! (is-authorized-caller) err-unauthorized)
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
    ;; Only authorized can finalize
    (asserts! (is-authorized-caller) err-unauthorized)
    ;; Must be active and voting ended
    (asserts! (is-eq (get status proposal) status-active) err-already-finalized)
    (asserts! (is-voting-ended proposal-id) err-invalid-proposal)
    
    ;; Update status
    (map-set proposals proposal-id 
      (merge proposal { 
        status: (if approved status-approved status-rejected) 
      })
    )
    
    ;; Record approval for treasury integration
    (if approved
      (map-set proposal-approvals proposal-id true)
      false
    )
    
    (ok true)
  )
)

(define-public (mark-as-executed (proposal-id uint) (execution-tx (string-utf8 100)))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
    )
    ;; Only authorized contracts can mark as executed
    (asserts! (is-authorized-caller) err-unauthorized)
    ;; Must be approved
    (asserts! (is-eq (get status proposal) status-approved) err-invalid-status)
    
    ;; Update status
    (map-set proposals proposal-id 
      (merge proposal { status: status-executed })
    )
    
    ;; Record execution data
    (map-set proposal-execution-data proposal-id {
      executed-at: block-height,
      executed-by: tx-sender,
      execution-tx: execution-tx
    })
    
    (ok true)
  )
)

(define-public (cancel-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals proposal-id) err-proposal-not-found))
    )
    ;; Only proposer or owner can cancel
    (asserts! (or 
      (is-eq tx-sender (get proposer proposal))
      (is-eq tx-sender contract-owner)
    ) err-unauthorized)
    ;; Must be pending or active
    (asserts! (or 
      (is-eq (get status proposal) status-pending)
      (is-eq (get status proposal) status-active)
    ) err-invalid-status)
    
    ;; Update status to cancelled
    (map-set proposals proposal-id 
      (merge proposal { status: status-cancelled })
    )
    
    (ok true)
  )
)

;; Authorization management
(define-public (authorize-contract (contract principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set authorized-contracts contract true)
    (ok true)
  )
)

(define-public (revoke-contract-authorization (contract principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-delete authorized-contracts contract)
    (ok true)
  )
)

;; Configuration updates
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

(define-public (update-proposal-limits (min-amount uint) (max-amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (< min-amount max-amount) err-invalid-proposal)
    (var-set min-proposal-amount min-amount)
    (var-set max-proposal-amount max-amount)
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