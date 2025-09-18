;; luminous-schema-engine

;; =========================================
;; System Configuration and Global State Variables
;; =========================================

;; Registry administrator principal
(define-constant REGISTRY-OWNER tx-sender)

;; Protocol error response constants
(define-constant ERR-INVALID-ASSET-SIZE (err u304))
(define-constant ERR-ACCESS-FORBIDDEN (err u305))
(define-constant ERR-INVALID-RECIPIENT (err u306))
(define-constant ERR-OWNER-ONLY-FUNCTION (err u307))
(define-constant ERR-ACCESS-DENIED (err u308))
(define-constant ERR-ASSET-NOT-FOUND (err u301))
(define-constant ERR-ASSET-EXISTS (err u302))
(define-constant ERR-INVALID-ASSET-TITLE (err u303))

;; Global asset sequence counter
(define-data-var total-registered-assets uint u0)

;; Asset access control mapping structure
(define-map asset-access-registry
  { asset-id: uint, accessor: principal }
  { authorized: bool }
)

;; Primary asset metadata storage
(define-map quantum-asset-database
  { asset-id: uint }
  {
    title: (string-ascii 64),
    creator: principal,
    size: uint,
    created-at: uint,
    description: (string-ascii 128),
    tags: (list 10 (string-ascii 32))
  }
)

;; =========================================
;; Internal Validation and Helper Functions
;; =========================================

;; Verifies asset existence in quantum database
(define-private (asset-exists-in-registry? (asset-id uint))
  (is-some (map-get? quantum-asset-database { asset-id: asset-id }))
)

;; Validates creator ownership privileges
(define-private (validate-creator-rights? (asset-id uint) (creator principal))
  (match (map-get? quantum-asset-database { asset-id: asset-id })
    asset-record (is-eq (get creator asset-record) creator)
    false
  )
)

;; Extracts asset size attribute
(define-private (extract-asset-size (asset-id uint))
  (default-to u0 
    (get size 
      (map-get? quantum-asset-database { asset-id: asset-id })
    )
  )
)

;; Validates tag list integrity
(define-private (validate-tag-collection? (tags (list 10 (string-ascii 32))))
  (and
    (> (len tags) u0)
    (<= (len tags) u10)
    (is-eq (len (filter validate-single-tag? tags)) (len tags))
  )
)

;; Validates string attribute constraints
(define-private (validate-string-attribute (text-input (string-ascii 64)) (min-len uint) (max-len uint))
  (and 
    (>= (len text-input) min-len)
    (<= (len text-input) max-len)
  )
)

;; Increments asset counter and returns previous value
(define-private (increment-asset-counter)
  (let ((previous-count (var-get total-registered-assets)))
    (var-set total-registered-assets (+ previous-count u1))
    (ok previous-count)
  )
)

;; Validates individual tag format
(define-private (validate-single-tag? (tag (string-ascii 32)))
  (and 
    (> (len tag) u0)
    (< (len tag) u33)
  )
)

;; =========================================
;; Public Interface Functions
;; =========================================

;; Creates new asset entry in quantum registry
(define-public (create-quantum-asset (title (string-ascii 64)) (size uint) (description (string-ascii 128)) (tags (list 10 (string-ascii 32))))
  (let
    (
      (next-asset-id (+ (var-get total-registered-assets) u1))
    )
    ;; Comprehensive input validation
    (asserts! (and (> (len title) u0) (< (len title) u65)) ERR-INVALID-ASSET-TITLE)
    (asserts! (and (> size u0) (< size u1000000000)) ERR-INVALID-ASSET-SIZE)
    (asserts! (and (> (len description) u0) (< (len description) u129)) ERR-INVALID-ASSET-TITLE)
    (asserts! (validate-tag-collection? tags) ERR-INVALID-ASSET-TITLE)

    ;; Insert new asset record
    (map-insert quantum-asset-database
      { asset-id: next-asset-id }
      {
        title: title,
        creator: tx-sender,
        size: size,
        created-at: block-height,
        description: description,
        tags: tags
      }
    )

    ;; Establish creator access permissions
    (map-insert asset-access-registry
      { asset-id: next-asset-id, accessor: tx-sender }
      { authorized: true }
    )

    ;; Update global asset counter
    (var-set total-registered-assets next-asset-id)
    (ok next-asset-id)
  )
)

;; Fetches asset description from registry
(define-public (fetch-asset-description (asset-id uint))
  (let
    (
      (asset-record (unwrap! (map-get? quantum-asset-database { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
    )
    (ok (get description asset-record))
  )
)

;; Confirms accessor permissions for specific asset
(define-public (confirm-access-privileges (asset-id uint) (accessor principal))
  (let
    (
      (permission-record (map-get? asset-access-registry { asset-id: asset-id, accessor: accessor }))
    )
    (ok (is-some permission-record))
  )
)

;; Calculates total tag count for asset
(define-public (calculate-tag-quantity (asset-id uint))
  (let
    (
      (asset-record (unwrap! (map-get? quantum-asset-database { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
    )
    (ok (len (get tags asset-record)))
  )
)

;; Validates title format compliance
(define-public (validate-title-format (title (string-ascii 64)))
  (ok (and (> (len title) u0) (<= (len title) u64)))
)

;; Transfers asset ownership to new creator
(define-public (transfer-asset-ownership (asset-id uint) (new-creator principal))
  (let
    (
      (asset-record (unwrap! (map-get? quantum-asset-database { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
    )
    (asserts! (asset-exists-in-registry? asset-id) ERR-ASSET-NOT-FOUND)
    (asserts! (is-eq (get creator asset-record) tx-sender) ERR-ACCESS-FORBIDDEN)

    ;; Execute ownership transfer
    (map-set quantum-asset-database
      { asset-id: asset-id }
      (merge asset-record { creator: new-creator })
    )
    (ok true)
  )
)

;; Updates comprehensive asset metadata
(define-public (update-asset-metadata (asset-id uint) (new-title (string-ascii 64)) (new-size uint) (new-description (string-ascii 128)) (new-tags (list 10 (string-ascii 32))))
  (let
    (
      (asset-record (unwrap! (map-get? quantum-asset-database { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
    )
    ;; Comprehensive validation suite
    (asserts! (asset-exists-in-registry? asset-id) ERR-ASSET-NOT-FOUND)
    (asserts! (is-eq (get creator asset-record) tx-sender) ERR-ACCESS-FORBIDDEN)
    (asserts! (and (> (len new-title) u0) (< (len new-title) u65)) ERR-INVALID-ASSET-TITLE)
    (asserts! (and (> new-size u0) (< new-size u1000000000)) ERR-INVALID-ASSET-SIZE)
    (asserts! (and (> (len new-description) u0) (< (len new-description) u129)) ERR-INVALID-ASSET-TITLE)
    (asserts! (validate-tag-collection? new-tags) ERR-INVALID-ASSET-TITLE)

    ;; Execute metadata update operation
    (map-set quantum-asset-database
      { asset-id: asset-id }
      (merge asset-record { 
        title: new-title, 
        size: new-size, 
        description: new-description, 
        tags: new-tags 
      })
    )
    (ok true)
  )
)

;; Permanently removes asset from quantum registry
(define-public (remove-quantum-asset (asset-id uint))
  (let
    (
      (asset-record (unwrap! (map-get? quantum-asset-database { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
    )
    (asserts! (asset-exists-in-registry? asset-id) ERR-ASSET-NOT-FOUND)
    (asserts! (is-eq (get creator asset-record) tx-sender) ERR-ACCESS-FORBIDDEN)

    ;; Execute permanent asset deletion
    (map-delete quantum-asset-database { asset-id: asset-id })
    (ok true)
  )
)

