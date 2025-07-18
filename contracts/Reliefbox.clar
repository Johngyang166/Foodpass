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
(define-constant ERR_VENDOR_NOT_FOUND (err u111))
(define-constant ERR_VENDOR_ALREADY_EXISTS (err u112))
(define-constant ERR_VENDOR_INACTIVE (err u113))
(define-constant ERR_INSUFFICIENT_VENDOR_BALANCE (err u114))
(define-constant ERR_INVALID_VENDOR_CATEGORY (err u115))
(define-constant ERR_VENDOR_LIMIT_EXCEEDED (err u116))
(define-constant ERR_INVALID_COMMISSION_RATE (err u117))
(define-constant ERR_SETTLEMENT_FAILED (err u118))

(define-constant MONTHLY_RATION_AMOUNT u1000000)
(define-constant BLOCKS_PER_MONTH u4320)
(define-constant MAX_FAMILY_SIZE u10)
(define-constant TOKEN_DECIMALS u6)
(define-constant MIN_VENDOR_BALANCE u100000)
(define-constant MAX_COMMISSION_RATE u500)
(define-constant VENDOR_CATEGORY_GROCERY u1)
(define-constant VENDOR_CATEGORY_RESTAURANT u2)
(define-constant VENDOR_CATEGORY_PHARMACY u3)
(define-constant VENDOR_CATEGORY_MARKET u4)

(define-data-var contract-owner principal CONTRACT_OWNER)
(define-data-var total-cards-issued uint u0)
(define-data-var monthly-distribution-pool uint u0)
(define-data-var current-distribution-cycle uint u0)
(define-data-var total-vendors-registered uint u0)
(define-data-var total-vendor-settlements uint u0)

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

(define-map vendor-registry
  { vendor-id: uint }
  {
    vendor-address: principal,
    business-name: (string-ascii 100),
    vendor-category: uint,
    commission-rate: uint,
    registration-date: uint,
    is-active: bool,
    total-transactions: uint,
    total-volume: uint,
    minimum-balance: uint,
    last-settlement-date: uint
  }
)

(define-map vendor-address-lookup
  { vendor-address: principal }
  { vendor-id: uint }
)

(define-map vendor-transactions
  { vendor-id: uint, transaction-id: uint }
  {
    customer: principal,
    amount: uint,
    transaction-date: uint,
    is-settled: bool,
    settlement-date: uint
  }
)

(define-map vendor-balances
  { vendor-id: uint }
  {
    pending-balance: uint,
    settled-balance: uint,
    total-earned: uint,
    last-updated: uint
  }
)

(define-map vendor-settlement-history
  { vendor-id: uint, settlement-id: uint }
  {
    settlement-amount: uint,
    settlement-date: uint,
    transaction-count: uint,
    commission-earned: uint
  }
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

(define-private (is-valid-vendor-category (category uint))
  (and (>= category VENDOR_CATEGORY_GROCERY) (<= category VENDOR_CATEGORY_MARKET))
)

(define-private (get-vendor-by-address (vendor-address principal))
  (match (map-get? vendor-address-lookup { vendor-address: vendor-address })
    address-data (map-get? vendor-registry { vendor-id: (get vendor-id address-data) })
    none
  )
)

(define-private (is-vendor-active (vendor-id uint))
  (match (map-get? vendor-registry { vendor-id: vendor-id })
    vendor-data (get is-active vendor-data)
    false
  )
)

(define-private (calculate-commission (amount uint) (commission-rate uint))
  (/ (* amount commission-rate) u10000)
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

(define-public (register-vendor (business-name (string-ascii 100)) (vendor-category uint) (commission-rate uint))
  (let (
    (vendor-id (+ (var-get total-vendors-registered) u1))
    (current-block stacks-block-height)
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (is-valid-vendor-category vendor-category) ERR_INVALID_VENDOR_CATEGORY)
    (asserts! (<= commission-rate MAX_COMMISSION_RATE) ERR_INVALID_COMMISSION_RATE)
    (asserts! (is-none (map-get? vendor-address-lookup { vendor-address: tx-sender })) ERR_VENDOR_ALREADY_EXISTS)
    
    (map-set vendor-registry
      { vendor-id: vendor-id }
      {
        vendor-address: tx-sender,
        business-name: business-name,
        vendor-category: vendor-category,
        commission-rate: commission-rate,
        registration-date: current-block,
        is-active: true,
        total-transactions: u0,
        total-volume: u0,
        minimum-balance: MIN_VENDOR_BALANCE,
        last-settlement-date: u0
      }
    )
    
    (map-set vendor-address-lookup
      { vendor-address: tx-sender }
      { vendor-id: vendor-id }
    )
    
    (map-set vendor-balances
      { vendor-id: vendor-id }
      {
        pending-balance: u0,
        settled-balance: u0,
        total-earned: u0,
        last-updated: current-block
      }
    )
    
    (var-set total-vendors-registered vendor-id)
    (ok vendor-id)
  )
)

(define-public (process-vendor-payment (vendor-address principal) (amount uint))
  (let (
    (vendor-data (unwrap! (get-vendor-by-address vendor-address) ERR_VENDOR_NOT_FOUND))
    (vendor-lookup (unwrap! (map-get? vendor-address-lookup { vendor-address: vendor-address }) ERR_VENDOR_NOT_FOUND))
    (vendor-id (get vendor-id vendor-lookup))
    (commission (calculate-commission amount (get commission-rate vendor-data)))
    (net-amount (- amount commission))
    (current-balances (default-to 
      { pending-balance: u0, settled-balance: u0, total-earned: u0, last-updated: u0 }
      (map-get? vendor-balances { vendor-id: vendor-id })
    ))
    (transaction-count (+ (get total-transactions vendor-data) u1))
  )
    (asserts! (get is-active vendor-data) ERR_VENDOR_INACTIVE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (ft-transfer? foodpass-token amount tx-sender vendor-address))
    
    (map-set vendor-registry
      { vendor-id: vendor-id }
      (merge vendor-data {
        total-transactions: transaction-count,
        total-volume: (+ (get total-volume vendor-data) amount)
      })
    )
    
    (map-set vendor-transactions
      { vendor-id: vendor-id, transaction-id: transaction-count }
      {
        customer: tx-sender,
        amount: amount,
        transaction-date: stacks-block-height,
        is-settled: false,
        settlement-date: u0
      }
    )
    
    (map-set vendor-balances
      { vendor-id: vendor-id }
      {
        pending-balance: (+ (get pending-balance current-balances) net-amount),
        settled-balance: (get settled-balance current-balances),
        total-earned: (+ (get total-earned current-balances) net-amount),
        last-updated: stacks-block-height
      }
    )
    
    (ok { transaction-id: transaction-count, commission: commission, net-amount: net-amount })
  )
)

(define-public (settle-vendor-payments (vendor-id uint))
  (let (
    (vendor-data (unwrap! (map-get? vendor-registry { vendor-id: vendor-id }) ERR_VENDOR_NOT_FOUND))
    (vendor-balances-data (unwrap! (map-get? vendor-balances { vendor-id: vendor-id }) ERR_VENDOR_NOT_FOUND))
    (pending-amount (get pending-balance vendor-balances-data))
    (settlement-id (+ (var-get total-vendor-settlements) u1))
    (commission-earned (calculate-commission (get total-volume vendor-data) (get commission-rate vendor-data)))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (get is-active vendor-data) ERR_VENDOR_INACTIVE)
    (asserts! (>= pending-amount (get minimum-balance vendor-data)) ERR_INSUFFICIENT_VENDOR_BALANCE)
    
    (try! (ft-mint? foodpass-token pending-amount (get vendor-address vendor-data)))
    
    (map-set vendor-balances
      { vendor-id: vendor-id }
      {
        pending-balance: u0,
        settled-balance: (+ (get settled-balance vendor-balances-data) pending-amount),
        total-earned: (get total-earned vendor-balances-data),
        last-updated: stacks-block-height
      }
    )
    
    (map-set vendor-registry
      { vendor-id: vendor-id }
      (merge vendor-data { last-settlement-date: stacks-block-height })
    )
    
    (map-set vendor-settlement-history
      { vendor-id: vendor-id, settlement-id: settlement-id }
      {
        settlement-amount: pending-amount,
        settlement-date: stacks-block-height,
        transaction-count: (get total-transactions vendor-data),
        commission-earned: commission-earned
      }
    )
    
    (var-set total-vendor-settlements settlement-id)
    (ok { settlement-id: settlement-id, amount: pending-amount })
  )
)

(define-public (update-vendor-status (vendor-id uint) (is-active bool))
  (let (
    (vendor-data (unwrap! (map-get? vendor-registry { vendor-id: vendor-id }) ERR_VENDOR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (map-set vendor-registry
      { vendor-id: vendor-id }
      (merge vendor-data { is-active: is-active })
    )
    (ok is-active)
  )
)

(define-public (update-vendor-commission (vendor-id uint) (new-commission-rate uint))
  (let (
    (vendor-data (unwrap! (map-get? vendor-registry { vendor-id: vendor-id }) ERR_VENDOR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (<= new-commission-rate MAX_COMMISSION_RATE) ERR_INVALID_COMMISSION_RATE)
    (map-set vendor-registry
      { vendor-id: vendor-id }
      (merge vendor-data { commission-rate: new-commission-rate })
    )
    (ok new-commission-rate)
  )
)

(define-public (update-vendor-minimum-balance (vendor-id uint) (new-minimum uint))
  (let (
    (vendor-data (unwrap! (map-get? vendor-registry { vendor-id: vendor-id }) ERR_VENDOR_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_UNAUTHORIZED)
    (asserts! (> new-minimum u0) ERR_INVALID_AMOUNT)
    (map-set vendor-registry
      { vendor-id: vendor-id }
      (merge vendor-data { minimum-balance: new-minimum })
    )
    (ok new-minimum)
  )
)

(define-read-only (get-vendor-details (vendor-id uint))
  (map-get? vendor-registry { vendor-id: vendor-id })
)

(define-read-only (get-vendor-by-address-info (vendor-address principal))
  (get-vendor-by-address vendor-address)
)

(define-read-only (get-vendor-balances (vendor-id uint))
  (map-get? vendor-balances { vendor-id: vendor-id })
)

(define-read-only (get-vendor-transaction (vendor-id uint) (transaction-id uint))
  (map-get? vendor-transactions { vendor-id: vendor-id, transaction-id: transaction-id })
)

(define-read-only (get-vendor-settlement (vendor-id uint) (settlement-id uint))
  (map-get? vendor-settlement-history { vendor-id: vendor-id, settlement-id: settlement-id })
)

(define-read-only (get-total-vendors)
  (var-get total-vendors-registered)
)

(define-read-only (get-total-settlements)
  (var-get total-vendor-settlements)
)

(define-read-only (is-vendor-registered (vendor-address principal))
  (is-some (map-get? vendor-address-lookup { vendor-address: vendor-address }))
)
