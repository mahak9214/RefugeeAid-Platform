;; RefugeeAid Platform Smart Contract
;; Support displaced populations with transparent aid distribution and identity management

;; Define the aid token for distribution
(define-fungible-token aid-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-refugee-not-registered (err u103))
(define-constant err-already-registered (err u104))
(define-constant err-insufficient-funds (err u105))

;; Data variables
(define-data-var total-aid-distributed uint u0)
(define-data-var total-registered-refugees uint u0)
(define-data-var next-distribution-id uint u1)

;; Refugee identity management map
(define-map refugee-registry 
  principal 
  {
    name: (string-ascii 50),
    registration-date: uint,
    total-aid-received: uint,
    is-verified: bool,
    location: (string-ascii 30)
  })

;; Aid distribution tracking map
(define-map aid-distributions
  {refugee: principal, distribution-id: uint}
  {
    amount: uint,
    timestamp: uint,
    aid-type: (string-ascii 30),
    distributor: principal,
    notes: (string-ascii 100)
  })

;; Function 1: Register Refugee Identity
;; Allows displaced individuals to register their identity on the platform
(define-public (register-refugee-identity 
  (refugee-name (string-ascii 50)) 
  (location (string-ascii 30)))
  (let ((refugee-principal tx-sender))
    (begin
      ;; Validate input
      (asserts! (> (len refugee-name) u0) err-invalid-amount)
      (asserts! (> (len location) u0) err-invalid-amount)
      
      ;; Check if refugee is not already registered
      (asserts! (is-none (map-get? refugee-registry refugee-principal)) err-already-registered)
      
      ;; Register the refugee with identity information
      (map-set refugee-registry refugee-principal
        {
          name: refugee-name,
          registration-date: stacks-block-height,
          total-aid-received: u0,
          is-verified: false,
          location: location
        })
      
      ;; Update total registered refugees count
      (var-set total-registered-refugees (+ (var-get total-registered-refugees) u1))
      
      ;; Return success with registration details
      (ok {
        message: "Refugee identity registered successfully",
        refugee: refugee-principal,
        name: refugee-name,
        location: location,
        registration-block:stacks-block-height
      }))))

;; Function 2: Distribute Aid to Refugees
;; Allows authorized distributors to transparently distribute aid to registered refugees
(define-public (distribute-aid-to-refugee 
  (recipient principal) 
  (amount uint) 
  (aid-type (string-ascii 30))
  (notes (string-ascii 100)))
  (let ((distribution-id (var-get next-distribution-id))
        (recipient-data (map-get? refugee-registry recipient)))
    (begin
      ;; Only contract owner can distribute aid (extensible to authorized distributors)
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      
      ;; Validate amount and aid type
      (asserts! (> amount u0) err-invalid-amount)
      (asserts! (> (len aid-type) u0) err-invalid-amount)
      
      ;; Check if recipient is registered refugee
      (asserts! (is-some recipient-data) err-refugee-not-registered)
      
      ;; Mint aid tokens to recipient
      (try! (ft-mint? aid-token amount recipient))
      
      ;; Record the aid distribution for transparency
      (map-set aid-distributions 
        {refugee: recipient, distribution-id: distribution-id}
        {
          amount: amount,
          timestamp: stacks-block-height,
          aid-type: aid-type,
          distributor: tx-sender,
          notes: notes
        })
      
      ;; Update refugee's total aid received
      (match recipient-data
        refugee-info
        (map-set refugee-registry recipient
          (merge refugee-info {total-aid-received: (+ (get total-aid-received refugee-info) amount)}))
        false)
      
      ;; Update global tracking variables
      (var-set total-aid-distributed (+ (var-get total-aid-distributed) amount))
      (var-set next-distribution-id (+ distribution-id u1))
      
      ;; Return distribution confirmation
      (ok {
        distribution-id: distribution-id,
        recipient: recipient,
        amount: amount,
        aid-type: aid-type,
        timestamp: stacks-block-height,
        notes: notes
      }))))

;; Read-only functions for transparency and verification

;; Get refugee information by principal
(define-read-only (get-refugee-info (refugee principal))
  (ok (map-get? refugee-registry refugee)))

;; Get specific aid distribution details
(define-read-only (get-aid-distribution-details (refugee principal) (distribution-id uint))
  (ok (map-get? aid-distributions {refugee: refugee, distribution-id: distribution-id})))

;; Get refugee's current aid token balance
(define-read-only (get-refugee-aid-balance (refugee principal))
  (ok (ft-get-balance aid-token refugee)))

;; Get platform statistics for transparency
(define-read-only (get-platform-statistics)
  (ok {
    total-aid-distributed: (var-get total-aid-distributed),
    total-registered-refugees: (var-get total-registered-refugees),
    next-distribution-id: (var-get next-distribution-id),
    current-block: stacks-block-height
  }))

;; Verify refugee status (can be extended to multiple verification authorities)
(define-public (verify-refugee-status (refugee principal))
  (let ((refugee-data (map-get? refugee-registry refugee)))
    (begin
      ;; Only contract owner can verify (extensible to verification authorities)
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      
      ;; Check if refugee is registered
      (asserts! (is-some refugee-data) err-refugee-not-registered)
      
      ;; Update verification status
      (match refugee-data
        refugee-info
        (map-set refugee-registry refugee
          (merge refugee-info {is-verified: true}))
        false)
      
      (ok {
        message: "Refugee status verified successfully", 
        refugee: refugee,
        verified-at-block: stacks-block-height
      }))))

;; Get total supply of aid tokens distributed
(define-read-only (get-total-aid-supply)
  (ok (ft-get-supply aid-token)))