(define-fungible-token foodpass-token)

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_CARD_NOT_FOUND (err u103))
(define-constant ERR_CARD_EXPIRED (err u104))
(define-constant ERR_CARD_ALREADY_EXISTS (err u105))
(define-constant ERR_MONTHLY_LIMIT_EXCEEDED (err u106))
(define-constant ERR_INVALID_QR_CODE (err u107))
(define-constant ERR_CARD_INACTIVE (err u108))
(define-constant ERR_INVALID_BENEFICIARY (err u109))
(define-constant ERR_DISTRIBUTION_FAILED (err u110))

(define-constant MONTHLY_RATION_AMOUNT u1000000)
(define-constant BLOCKS_PER_MONTH u4320)
(define-constant MAX_FAMILY_SIZE u10)
(define-constant TOKEN_DECIMALS u6)

(define-data-var contract-owner principal CONTRACT_OWNER)
(define-data-var total-cards-issued uint u0)
(define-data-var monthly-distribution-pool uint u0)
(define-data-var current-distribution-cycle uint u0)

(define-map ration-cards
  { card-id: uint }
  {
    beneficiary: principal,
    family-size: uint,
    monthly-allocation: uint,
    issue-date: uint,
    expiry-date: uint,
    is-active: bool,
    qr-code-hash: (buff 32),
    last-claim-cycle: uint,
    total-claimed: uint
  }
)

(define-map user-cards
  { beneficiary: principal }
  { card-id: uint }
)

(define-map monthly-claims
  { card-id: uint, cycle: uint }
  { amount-claimed: uint, claim-date: uint }
)

(define-map authorized-distributors
  { distributor: principal }
  { is-authorized: bool, registration-date: uint }
)

(define-map qr-code-registry
  { qr-hash: (buff 32) }
  { card-id: uint, created-at: uint }
)

(define-private (get-current-cycle)
  (/ (- stacks-block-height u1) BLOCKS_PER_MONTH)
)

(define-private (is-card-expired (card-id uint))
  (match (map-get? ration-cards { card-id: card-id })
    card-data (>= stacks-block-height (get expiry-date card-data))
    true
  )
)

(define-private (calculate-monthly-allocation (family-size uint))
  (if (<= family-size MAX_FAMILY_SIZE)
    (* MONTHLY_RATION_AMOUNT family-size)
    (* MONTHLY_RATION_AMOUNT MAX_FAMILY_SIZE)
  )
)

(define-private (generate-qr-hash (card-id uint) (beneficiary principal))
  (keccak256 (concat (unwrap-panic (to-consensus-buff? card-id)) (unwrap-panic (to-consensus-buff? beneficiary))))
)

(define-private (is-authorized-distributor (distributor principal))
  (default-to false (get is-authorized (map-get? authorized-distributors { distributor: distributor })))
)

(define-private (validate-beneficiary (beneficiary principal))
  (not (is-eq beneficiary CONTRACT_OWNER))
)

(define-public (issue-ration-card (beneficiary principal) (family-size uint) (validity-months uint))
  (let (
    (card-id (+ (var-get total-cards-issued) u1))
    (current-block stacks-block-height)
    (expiry-block (+ current-block (* validity-months BLOCKS_PER_MONTH)))
    (monthly-allocation (calculate-monthly-allocation family-size))
    (qr-hash (generate-qr-hash card-id beneficiary))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (validate-beneficiary beneficiary) ERR_INVALID_BENEFICIARY)
    (asserts! (and (> family-size u0) (<= family-size MAX_FAMILY_SIZE)) ERR_INVALID_AMOUNT)
    (asserts! (> validity-months u0) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? user-cards { beneficiary: beneficiary })) ERR_CARD_ALREADY_EXISTS)
    
    (map-set ration-cards
      { card-id: card-id }
      {
        beneficiary: beneficiary,
        family-size: family-size,
        monthly-allocation: monthly-allocation,
        issue-date: current-block,
        expiry-date: expiry-block,
        is-active: true,
        qr-code-hash: qr-hash,
        last-claim-cycle: u0,
        total-claimed: u0
      }
    )
    
    (map-set user-cards
      { beneficiary: beneficiary }
      { card-id: card-id }
    )
    
    (map-set qr-code-registry
      { qr-hash: qr-hash }
      { card-id: card-id, created-at: current-block }
    )
    
    (var-set total-cards-issued card-id)
    (ok card-id)
  )
)

(define-public (claim-monthly-ration (card-id uint) (qr-code-hash (buff 32)))
  (let (
    (current-cycle (get-current-cycle))
    (card-data (unwrap! (map-get? ration-cards { card-id: card-id }) ERR_CARD_NOT_FOUND))
    (qr-data (unwrap! (map-get? qr-code-registry { qr-hash: qr-code-hash }) ERR_INVALID_QR_CODE))
  )
    (asserts! (is-eq (get card-id qr-data) card-id) ERR_INVALID_QR_CODE)
    (asserts! (is-eq (get qr-code-hash card-data) qr-code-hash) ERR_INVALID_QR_CODE)
    (asserts! (get is-active card-data) ERR_CARD_INACTIVE)
    (asserts! (not (is-card-expired card-id)) ERR_CARD_EXPIRED)
    (asserts! (not (is-eq (get last-claim-cycle card-data) current-cycle)) ERR_MONTHLY_LIMIT_EXCEEDED)
    
    (let (
      (allocation (get monthly-allocation card-data))
      (beneficiary (get beneficiary card-data))
    )
      (try! (ft-mint? foodpass-token allocation beneficiary))
      
      (map-set ration-cards
        { card-id: card-id }
        (merge card-data {
          last-claim-cycle: current-cycle,
          total-claimed: (+ (get total-claimed card-data) allocation)
        })
      )
      
      (map-set monthly-claims
        { card-id: card-id, cycle: current-cycle }
        { amount-claimed: allocation, claim-date: stacks-block-height }
      )
      
      (ok allocation)
    )
  )
)

(define-public (transfer-rations (recipient principal) (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (ft-transfer? foodpass-token amount tx-sender recipient)
  )
)

(define-public (deactivate-card (card-id uint))
  (let (
    (card-data (unwrap! (map-get? ration-cards { card-id: card-id }) ERR_CARD_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set ration-cards
      { card-id: card-id }
      (merge card-data { is-active: false })
    )
    (ok true)
  )
)

(define-public (reactivate-card (card-id uint))
  (let (
    (card-data (unwrap! (map-get? ration-cards { card-id: card-id }) ERR_CARD_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (not (is-card-expired card-id)) ERR_CARD_EXPIRED)
    (map-set ration-cards
      { card-id: card-id }
      (merge card-data { is-active: true })
    )
    (ok true)
  )
)

(define-public (authorize-distributor (distributor principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set authorized-distributors
      { distributor: distributor }
      { is-authorized: true, registration-date: stacks-block-height }
    )
    (ok true)
  )
)

(define-public (revoke-distributor (distributor principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set authorized-distributors
      { distributor: distributor }
      { is-authorized: false, registration-date: stacks-block-height }
    )
    (ok true)
  )
)

(define-public (distribute-emergency-rations (recipients (list 50 principal)) (amount uint))
  (begin
    (asserts! (is-authorized-distributor tx-sender) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (fold emergency-distribute recipients (ok u0))
  )
)

(define-private (emergency-distribute (recipient principal) (prev-result (response uint uint)))
  (match prev-result
    amount
      (match (ft-mint? foodpass-token amount recipient)
        ok-val (ok amount)
        err-val (err err-val)
      )
    error-val (err error-val)
  )
)

(define-public (update-family-size (card-id uint) (new-family-size uint))
  (let (
    (card-data (unwrap! (map-get? ration-cards { card-id: card-id }) ERR_CARD_NOT_FOUND))
    (new-allocation (calculate-monthly-allocation new-family-size))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (and (> new-family-size u0) (<= new-family-size MAX_FAMILY_SIZE)) ERR_INVALID_AMOUNT)
    (map-set ration-cards
      { card-id: card-id }
      (merge card-data {
        family-size: new-family-size,
        monthly-allocation: new-allocation
      })
    )
    (ok new-allocation)
  )
)

(define-public (extend-card-validity (card-id uint) (additional-months uint))
  (let (
    (card-data (unwrap! (map-get? ration-cards { card-id: card-id }) ERR_CARD_NOT_FOUND))
    (new-expiry (+ (get expiry-date card-data) (* additional-months BLOCKS_PER_MONTH)))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (> additional-months u0) ERR_INVALID_AMOUNT)
    (map-set ration-cards
      { card-id: card-id }
      (merge card-data { expiry-date: new-expiry })
    )
    (ok new-expiry)
  )
)

(define-read-only (get-card-details (card-id uint))
  (map-get? ration-cards { card-id: card-id })
)

(define-read-only (get-user-card (beneficiary principal))
  (map-get? user-cards { beneficiary: beneficiary })
)

(define-read-only (get-monthly-claim (card-id uint) (cycle uint))
  (map-get? monthly-claims { card-id: card-id, cycle: cycle })
)

(define-read-only (get-current-distribution-cycle)
  (get-current-cycle)
)

(define-read-only (get-card-balance (card-id uint))
  (match (map-get? ration-cards { card-id: card-id })
    card-data (ft-get-balance foodpass-token (get beneficiary card-data))
    u0
  )
)

(define-read-only (get-qr-code-info (qr-hash (buff 32)))
  (map-get? qr-code-registry { qr-hash: qr-hash })
)

(define-read-only (is-card-valid (card-id uint))
  (match (map-get? ration-cards { card-id: card-id })
    card-data (and (get is-active card-data) (not (is-card-expired card-id)))
    false
  )
)

(define-read-only (get-total-cards-issued)
  (var-get total-cards-issued)
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

(define-read-only (is-distributor-authorized (distributor principal))
  (is-authorized-distributor distributor)
)

(define-read-only (get-token-balance (user principal))
  (ft-get-balance foodpass-token user)
)
