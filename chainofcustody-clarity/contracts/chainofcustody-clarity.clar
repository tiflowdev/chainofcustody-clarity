;; ChainOfCustody - Living Supply Chain Memory

;; Core features implemented:
;;   - Product registration with provenance DNA
;;   - Ownership transfer with experience accumulation
;;   - Participant reputation (Trust Inheritance)
;;   - Contributory Value Stacking (micro-royalties)
;;   - Temporal Quality Oracles (degradation + dynamic pricing)
;;   - Programmable Compliance verification
;;   - Decentralized Quality Arbitration

;; ===================================================
;; CONSTANTS
;; ===================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED        (err u100))
(define-constant ERR-PRODUCT-NOT-FOUND     (err u101))
(define-constant ERR-PRODUCT-EXISTS        (err u102))
(define-constant ERR-NOT-OWNER             (err u103))
(define-constant ERR-INVALID-QUALITY       (err u104))
(define-constant ERR-DISPUTE-NOT-FOUND     (err u105))
(define-constant ERR-ALREADY-RESOLVED      (err u106))
(define-constant ERR-INVALID-COMPLIANCE    (err u107))
(define-constant ERR-INSUFFICIENT-ROYALTY  (err u108))

;; Quality score bounds (0-100)
(define-constant MIN-QUALITY u0)
(define-constant MAX-QUALITY u100)

;; Royalty basis points (e.g. 200 = 2%)
(define-constant ROYALTY-BASIS-POINTS u200)
(define-constant BASIS-POINTS-DENOM   u10000)

;; ===================================================
;; DATA MAPS & VARS
;; ===================================================

;; Auto-incrementing IDs
(define-data-var next-product-id uint u1)
(define-data-var next-dispute-id uint u1)

;; ----- Product Registry -----
(define-map products
  { product-id: uint }
  {
    name:              (string-ascii 64),
    category:          (string-ascii 32),  ;; food | pharma | luxury | general
    owner:             principal,
    origin:            principal,          ;; original producer (immutable)
    quality-score:     uint,               ;; 0-100, updated by oracles
    base-price:        uint,               ;; in micro-STX
    current-price:     uint,               ;; dynamically adjusted
    experience-count:  uint,               ;; number of custody transfers
    compliant:         bool,               ;; programmable compliance flag
    active:            bool
  }
)

;; ----- Custody / Experience Log (Living Supply Chain Memory) -----
(define-map custody-log
  { product-id: uint, experience-index: uint }
  {
    custodian:         principal,
    quality-at-entry:  uint,
    quality-at-exit:   uint,
    temperature-ok:    bool,             ;; IoT sensor proxy
    handling-score:    uint,             ;; 0-100, reported by next custodian
    timestamp:         uint,             ;; block height as timestamp
    notes:             (string-ascii 128)
  }
)

;; ----- Participant Profiles (Reputation / Trust Inheritance) -----
(define-map participants
  { participant: principal }
  {
    reputation-score:  uint,   ;; accumulated across all products handled
    total-handled:     uint,
    royalties-earned:  uint,   ;; in micro-STX
    royalties-paid:    uint
  }
)

;; ----- Supply Chain Genetics: successful routing templates -----
(define-map routing-templates
  { template-id: uint }
  {
    creator:           principal,
    category:          (string-ascii 32),
    avg-quality-gain:  uint,
    use-count:         uint,
    active:            bool
  }
)
(define-data-var next-template-id uint u1)

;; ----- Quality Arbitration Disputes -----
(define-map disputes
  { dispute-id: uint }
  {
    product-id:        uint,
    claimant:          principal,
    respondent:        principal,
    description:       (string-ascii 256),
    ruling:            (optional (string-ascii 128)),
    resolved:          bool,
    timestamp:         uint
  }
)

;; ===================================================
;; PRIVATE HELPERS
;; ===================================================

(define-private (get-or-init-participant (p principal))
  (default-to
    { reputation-score: u50, total-handled: u0,
      royalties-earned: u0, royalties-paid: u0 }
    (map-get? participants { participant: p })
  )
)

(define-private (clamp-quality (q uint))
  (if (> q MAX-QUALITY) MAX-QUALITY
      (if (< q MIN-QUALITY) MIN-QUALITY q))
)

;; Dynamic price adjustment: price decays with quality drop
;; new-price = base-price * quality-score / 100
(define-private (calc-dynamic-price (base uint) (quality uint))
  (/ (* base quality) u100)
)

;; Micro-royalty = transfer-value * ROYALTY-BASIS-POINTS / BASIS-POINTS-DENOM
(define-private (calc-royalty (value uint))
  (/ (* value ROYALTY-BASIS-POINTS) BASIS-POINTS-DENOM)
)

;; ===================================================
;; PUBLIC FUNCTIONS
;; ===================================================

;; ----- Register a new product (Product Consciousness birth) -----
(define-public (register-product
    (name          (string-ascii 64))
    (category      (string-ascii 32))
    (base-price    uint)
    (initial-quality uint))
  (let (
    (pid       (var-get next-product-id))
    (quality   (clamp-quality initial-quality))
    (dyn-price (calc-dynamic-price base-price quality))
  )
    (asserts! (is-none (map-get? products { product-id: pid })) ERR-PRODUCT-EXISTS)
    (map-set products { product-id: pid }
      {
        name:             name,
        category:         category,
        owner:            tx-sender,
        origin:           tx-sender,
        quality-score:    quality,
        base-price:       base-price,
        current-price:    dyn-price,
        experience-count: u0,
        compliant:        true,
        active:           true
      }
    )
    ;; Init participant if new
    (map-set participants { participant: tx-sender }
      (merge (get-or-init-participant tx-sender)
             { total-handled: (+ (get total-handled (get-or-init-participant tx-sender)) u1) })
    )
    (var-set next-product-id (+ pid u1))
    (ok pid)
  )
)

;; ----- Transfer custody (Experience Accumulation Protocol) -----
;; Caller must be current owner; pays royalty to origin on each transfer.
(define-public (transfer-custody
    (product-id       uint)
    (new-owner        principal)
    (quality-at-exit  uint)
    (temperature-ok   bool)
    (handling-score   uint)
    (notes            (string-ascii 128)))
  (let (
    (product   (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
    (exp-idx   (get experience-count product))
    (q-exit    (clamp-quality quality-at-exit))
    (h-score   (clamp-quality handling-score))
    (royalty   (calc-royalty (get current-price product)))
    (origin    (get origin product))
    (old-owner tx-sender)
    (old-rep   (get-or-init-participant old-owner))
    (new-rep   (get-or-init-participant new-owner))
    (origin-p  (get-or-init-participant origin))
    (new-price (calc-dynamic-price (get base-price product) q-exit))
  )
    (asserts! (is-eq tx-sender (get owner product)) ERR-NOT-OWNER)
    (asserts! (get active product) ERR-NOT-AUTHORIZED)

    ;; Pay royalty from old owner to origin (STX transfer)
    (if (and (> royalty u0) (not (is-eq old-owner origin)))
      (try! (stx-transfer? royalty old-owner origin))
      true
    )

    ;; Log the custody experience
    (map-set custody-log { product-id: product-id, experience-index: exp-idx }
      {
        custodian:        old-owner,
        quality-at-entry: (get quality-score product),
        quality-at-exit:  q-exit,
        temperature-ok:   temperature-ok,
        handling-score:   h-score,
        timestamp:        block-height,
        notes:            notes
      }
    )

    ;; Update product state (Temporal Quality Oracle adjustment)
    (map-set products { product-id: product-id }
      (merge product {
        owner:            new-owner,
        quality-score:    q-exit,
        current-price:    new-price,
        experience-count: (+ exp-idx u1)
      })
    )

    ;; Update old owner reputation
    (map-set participants { participant: old-owner }
      (merge old-rep {
        reputation-score: (/ (+ (* (get reputation-score old-rep) u4) h-score) u5),
        royalties-paid:   (+ (get royalties-paid old-rep) royalty)
      })
    )

    ;; Credit royalty to origin
    (map-set participants { participant: origin }
      (merge origin-p {
        royalties-earned: (+ (get royalties-earned origin-p) royalty)
      })
    )

    ;; Update new owner participation count
    (map-set participants { participant: new-owner }
      (merge new-rep {
        total-handled: (+ (get total-handled new-rep) u1)
      })
    )

    (ok true)
  )
)

;; ----- Oracle: update quality score and dynamic price -----
;; Only contract owner acts as trusted oracle in this MVP.
(define-public (oracle-update-quality
    (product-id uint)
    (new-quality uint))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
    (q       (clamp-quality new-quality))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set products { product-id: product-id }
      (merge product {
        quality-score: q,
        current-price: (calc-dynamic-price (get base-price product) q)
      })
    )
    (ok true)
  )
)

;; ----- Programmable Compliance: set compliance flag -----
(define-public (set-compliance
    (product-id uint)
    (compliant  bool))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set products { product-id: product-id }
      (merge product { compliant: compliant })
    )
    (ok true)
  )
)

;; ----- Supply Chain Genetics: register a routing template -----
(define-public (register-template
    (category        (string-ascii 32))
    (avg-quality-gain uint))
  (let (
    (tid (var-get next-template-id))
  )
    (map-set routing-templates { template-id: tid }
      {
        creator:          tx-sender,
        category:         category,
        avg-quality-gain: avg-quality-gain,
        use-count:        u0,
        active:           true
      }
    )
    (var-set next-template-id (+ tid u1))
    (ok tid)
  )
)

;; ----- Apply a routing template (increments use-count) -----
(define-public (apply-template (template-id uint))
  (let (
    (tmpl (unwrap! (map-get? routing-templates { template-id: template-id })
                   ERR-PRODUCT-NOT-FOUND))
  )
    (asserts! (get active tmpl) ERR-NOT-AUTHORIZED)
    (map-set routing-templates { template-id: template-id }
      (merge tmpl { use-count: (+ (get use-count tmpl) u1) })
    )
    (ok true)
  )
)

;; ----- Decentralized Quality Arbitration: open a dispute -----
(define-public (open-dispute
    (product-id  uint)
    (respondent  principal)
    (description (string-ascii 256)))
  (let (
    (did (var-get next-dispute-id))
  )
    (asserts! (is-some (map-get? products { product-id: product-id })) ERR-PRODUCT-NOT-FOUND)
    (map-set disputes { dispute-id: did }
      {
        product-id:  product-id,
        claimant:    tx-sender,
        respondent:  respondent,
        description: description,
        ruling:      none,
        resolved:    false,
        timestamp:   block-height
      }
    )
    (var-set next-dispute-id (+ did u1))
    (ok did)
  )
)

;; ----- Resolve a dispute (arbitrator = contract owner for MVP) -----
(define-public (resolve-dispute
    (dispute-id uint)
    (ruling     (string-ascii 128)))
  (let (
    (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-DISPUTE-NOT-FOUND))
  )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (not (get resolved dispute)) ERR-ALREADY-RESOLVED)
    (map-set disputes { dispute-id: dispute-id }
      (merge dispute { ruling: (some ruling), resolved: true })
    )
    (ok true)
  )
)

;; ----- Deactivate a product (end of life) -----
(define-public (deactivate-product (product-id uint))
  (let (
    (product (unwrap! (map-get? products { product-id: product-id }) ERR-PRODUCT-NOT-FOUND))
  )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER)
                  (is-eq tx-sender (get owner product))) ERR-NOT-AUTHORIZED)
    (map-set products { product-id: product-id }
      (merge product { active: false })
    )
    (ok true)
  )
)

;; ===================================================
;; READ-ONLY FUNCTIONS
;; ===================================================

(define-read-only (get-product (product-id uint))
  (map-get? products { product-id: product-id })
)

(define-read-only (get-custody-entry (product-id uint) (experience-index uint))
  (map-get? custody-log { product-id: product-id, experience-index: experience-index })
)

(define-read-only (get-participant (p principal))
  (get-or-init-participant p)
)

(define-read-only (get-template (template-id uint))
  (map-get? routing-templates { template-id: template-id })
)

(define-read-only (get-dispute (dispute-id uint))
  (map-get? disputes { dispute-id: dispute-id })
)

(define-read-only (get-next-product-id)
  (var-get next-product-id)
)

(define-read-only (get-next-dispute-id)
  (var-get next-dispute-id)
)

(define-read-only (is-compliant (product-id uint))
  (match (map-get? products { product-id: product-id })
    product (ok (get compliant product))
    ERR-PRODUCT-NOT-FOUND
  )
)
