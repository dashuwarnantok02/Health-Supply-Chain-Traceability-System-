;; Health Supply Chain Traceability System

(define-data-var admin principal tx-sender)

(define-map products
    { product-id: (string-ascii 36) }
    {
        name: (string-ascii 64),
        manufacturer: principal,
        manufacturing-date: uint,
        expiry-date: uint,
        batch-number: (string-ascii 36),
        current-custodian: principal,
        is-active: bool,
    }
)

(define-map product-history
    {
        product-id: (string-ascii 36),
        sequence: uint,
    }
    {
        timestamp: uint,
        custodian: principal,
        location: (string-ascii 64),
        temperature: int,
        action: (string-ascii 20),
    }
)

(define-map custodians
    { custodian-id: principal }
    {
        name: (string-ascii 64),
        role: (string-ascii 20),
        is-verified: bool,
    }
)

(define-map locations
    { location-id: (string-ascii 36) }
    {
        name: (string-ascii 64),
        address: (string-ascii 128),
        location-type: (string-ascii 20),
    }
)

(define-data-var history-counter uint u0)

(define-read-only (get-admin)
    (var-get admin)
)

(define-public (set-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) (err u403))
        (ok (var-set admin new-admin))
    )
)

(define-public (register-custodian
        (name (string-ascii 64))
        (role (string-ascii 20))
    )
    (begin
        (asserts!
            (or (is-eq tx-sender (var-get admin)) (is-none (map-get? custodians { custodian-id: tx-sender })))
            (err u401)
        )
        (map-set custodians { custodian-id: tx-sender } {
            name: name,
            role: role,
            is-verified: (is-eq tx-sender (var-get admin)),
        })
        (ok true)
    )
)

(define-public (verify-custodian (custodian principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) (err u403))
        (asserts! (is-some (map-get? custodians { custodian-id: custodian }))
            (err u404)
        )
        (map-set custodians { custodian-id: custodian }
            (merge
                (unwrap-panic (map-get? custodians { custodian-id: custodian })) { is-verified: true }
            ))
        (ok true)
    )
)

(define-public (register-location
        (location-id (string-ascii 36))
        (name (string-ascii 64))
        (address (string-ascii 128))
        (location-type (string-ascii 20))
    )
    (begin
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (map-set locations { location-id: location-id } {
            name: name,
            address: address,
            location-type: location-type,
        })
        (ok true)
    )
)

(define-public (register-product
        (product-id (string-ascii 36))
        (name (string-ascii 64))
        (manufacturing-date uint)
        (expiry-date uint)
        (batch-number (string-ascii 36))
    )
    (begin
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (is-none (map-get? products { product-id: product-id }))
            (err u409)
        )
        (map-set products { product-id: product-id } {
            name: name,
            manufacturer: tx-sender,
            manufacturing-date: manufacturing-date,
            expiry-date: expiry-date,
            batch-number: batch-number,
            current-custodian: tx-sender,
            is-active: true,
        })
        (try! (record-product-event product-id tx-sender "MANUFACTURED" 0))
        (ok true)
    )
)

(define-public (transfer-product
        (product-id (string-ascii 36))
        (new-custodian principal)
        (location (string-ascii 64))
        (temperature int)
    )
    (let ((product (unwrap! (map-get? products { product-id: product-id }) (err u404))))
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (is-custodian-verified new-custodian) (err u401))
        (asserts! (is-eq (get current-custodian product) tx-sender) (err u403))
        (asserts! (get is-active product) (err u410))
        (map-set products { product-id: product-id }
            (merge product { current-custodian: new-custodian })
        )
        (try! (record-product-event product-id new-custodian location temperature))
        (ok true)
    )
)

(define-public (record-product-event
        (product-id (string-ascii 36))
        (custodian principal)
        (location (string-ascii 64))
        (temperature int)
    )
    (let (
            (product (unwrap! (map-get? products { product-id: product-id }) (err u404)))
            (counter (var-get history-counter))
        )
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (get is-active product) (err u410))
        (map-set product-history {
            product-id: product-id,
            sequence: counter,
        } {
            timestamp: burn-block-height,
            custodian: custodian,
            location: location,
            temperature: temperature,
            action: "TRANSFER",
        })
        (var-set history-counter (+ counter u1))
        (ok true)
    )
)

(define-public (record-product-event-with-action
        (product-id (string-ascii 36))
        (custodian principal)
        (location (string-ascii 64))
        (temperature int)
        (action (string-ascii 20))
    )
    (let (
            (product (unwrap! (map-get? products { product-id: product-id }) (err u404)))
            (counter (var-get history-counter))
        )
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (get is-active product) (err u410))
        (map-set product-history {
            product-id: product-id,
            sequence: counter,
        } {
            timestamp: burn-block-height,
            custodian: custodian,
            location: location,
            temperature: temperature,
            action: action,
        })
        (var-set history-counter (+ counter u1))
        (ok true)
    )
)

(define-public (deactivate-product (product-id (string-ascii 36)))
    (let ((product (unwrap! (map-get? products { product-id: product-id }) (err u404))))
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts!
            (or (is-eq tx-sender (var-get admin)) (is-eq (get current-custodian product) tx-sender))
            (err u403)
        )
        (map-set products { product-id: product-id }
            (merge product { is-active: false })
        )
        (ok true)
    )
)

(define-read-only (get-product (product-id (string-ascii 36)))
    (map-get? products { product-id: product-id })
)

(define-read-only (get-product-history
        (product-id (string-ascii 36))
        (sequence uint)
    )
    (map-get? product-history {
        product-id: product-id,
        sequence: sequence,
    })
)

(define-read-only (get-custodian (custodian principal))
    (map-get? custodians { custodian-id: custodian })
)

(define-read-only (get-location (location-id (string-ascii 36)))
    (map-get? locations { location-id: location-id })
)

(define-read-only (is-custodian-verified (custodian principal))
    (match (map-get? custodians { custodian-id: custodian })
        custodian-data (get is-verified custodian-data)
        false
    )
)
