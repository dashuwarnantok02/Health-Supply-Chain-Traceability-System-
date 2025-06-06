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
(define-map quality-inspectors
    { inspector-id: principal }
    {
        name: (string-ascii 64),
        certification: (string-ascii 64),
        is-active: bool,
    }
)

(define-map quality-assessments
    {
        product-id: (string-ascii 36),
        assessment-id: uint,
    }
    {
        inspector: principal,
        timestamp: uint,
        quality-score: uint,
        compliance-status: (string-ascii 20),
        notes: (string-ascii 256),
        temperature-compliant: bool,
        packaging-intact: bool,
        documentation-complete: bool,
    }
)

(define-data-var assessment-counter uint u0)

(define-public (register-quality-inspector
        (inspector principal)
        (name (string-ascii 64))
        (certification (string-ascii 64))
    )
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) (err u403))
        (map-set quality-inspectors { inspector-id: inspector } {
            name: name,
            certification: certification,
            is-active: true,
        })
        (ok true)
    )
)

(define-public (record-quality-assessment
        (product-id (string-ascii 36))
        (quality-score uint)
        (compliance-status (string-ascii 20))
        (notes (string-ascii 256))
        (temperature-compliant bool)
        (packaging-intact bool)
        (documentation-complete bool)
    )
    (let ((assessment-id (var-get assessment-counter)))
        (asserts! (is-quality-inspector tx-sender) (err u401))
        (asserts! (is-some (map-get? products { product-id: product-id }))
            (err u404)
        )
        (asserts! (<= quality-score u100) (err u400))
        (map-set quality-assessments {
            product-id: product-id,
            assessment-id: assessment-id,
        } {
            inspector: tx-sender,
            timestamp: burn-block-height,
            quality-score: quality-score,
            compliance-status: compliance-status,
            notes: notes,
            temperature-compliant: temperature-compliant,
            packaging-intact: packaging-intact,
            documentation-complete: documentation-complete,
        })
        (var-set assessment-counter (+ assessment-id u1))
        (ok assessment-id)
    )
)

(define-public (deactivate-quality-inspector (inspector principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) (err u403))
        (asserts!
            (is-some (map-get? quality-inspectors { inspector-id: inspector }))
            (err u404)
        )
        (map-set quality-inspectors { inspector-id: inspector }
            (merge
                (unwrap-panic (map-get? quality-inspectors { inspector-id: inspector })) { is-active: false }
            ))
        (ok true)
    )
)

(define-read-only (get-quality-assessment
        (product-id (string-ascii 36))
        (assessment-id uint)
    )
    (map-get? quality-assessments {
        product-id: product-id,
        assessment-id: assessment-id,
    })
)

(define-read-only (get-quality-inspector (inspector principal))
    (map-get? quality-inspectors { inspector-id: inspector })
)

(define-read-only (is-quality-inspector (inspector principal))
    (match (map-get? quality-inspectors { inspector-id: inspector })
        inspector-data (get is-active inspector-data)
        false
    )
)

(define-read-only (get-product-compliance-status (product-id (string-ascii 36)))
    (let ((product (map-get? products { product-id: product-id })))
        (match product
            product-data (some {
                product-id: product-id,
                is-active: (get is-active product-data),
                current-custodian: (get current-custodian product-data),
                expiry-date: (get expiry-date product-data),
                is-expired: (> burn-block-height (get expiry-date product-data)),
            })
            none
        )
    )
)
(define-map alert-subscriptions
    { subscriber: principal }
    {
        temperature-alerts: bool,
        expiry-alerts: bool,
        compliance-alerts: bool,
        transfer-alerts: bool,
    }
)

(define-map system-alerts
    { alert-id: uint }
    {
        product-id: (string-ascii 36),
        alert-type: (string-ascii 20),
        severity: (string-ascii 10),
        message: (string-ascii 256),
        timestamp: uint,
        is-resolved: bool,
        created-by: principal,
    }
)

(define-map alert-recipients
    {
        alert-id: uint,
        recipient: principal,
    }
    {
        is-read: bool,
        read-timestamp: uint,
    }
)

(define-data-var alert-counter uint u0)
(define-data-var temperature-threshold-min int 2)
(define-data-var temperature-threshold-max int 8)
(define-data-var expiry-warning-blocks uint u1440)

(define-public (subscribe-to-alerts
        (temperature-alerts bool)
        (expiry-alerts bool)
        (compliance-alerts bool)
        (transfer-alerts bool)
    )
    (begin
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (map-set alert-subscriptions { subscriber: tx-sender } {
            temperature-alerts: temperature-alerts,
            expiry-alerts: expiry-alerts,
            compliance-alerts: compliance-alerts,
            transfer-alerts: transfer-alerts,
        })
        (ok true)
    )
)

(define-public (create-alert
        (product-id (string-ascii 36))
        (alert-type (string-ascii 20))
        (severity (string-ascii 10))
        (message (string-ascii 256))
    )
    (let ((alert-id (var-get alert-counter)))
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (is-some (map-get? products { product-id: product-id }))
            (err u404)
        )
        (map-set system-alerts { alert-id: alert-id } {
            product-id: product-id,
            alert-type: alert-type,
            severity: severity,
            message: message,
            timestamp: burn-block-height,
            is-resolved: false,
            created-by: tx-sender,
        })
        (var-set alert-counter (+ alert-id u1))
        (unwrap! (distribute-alert alert-id alert-type) (err u500))
        (ok alert-id)
    )
)

(define-public (check-temperature-violation
        (product-id (string-ascii 36))
        (temperature int)
    )
    (begin
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (is-some (map-get? products { product-id: product-id }))
            (err u404)
        )
        (if (or
                (< temperature (var-get temperature-threshold-min))
                (> temperature (var-get temperature-threshold-max))
            )
            (create-alert product-id "TEMPERATURE" "HIGH"
                "Temperature violation detected"
            )
            (ok u0)
        )
    )
)

(define-public (check-expiry-warning (product-id (string-ascii 36)))
    (let ((product (unwrap! (map-get? products { product-id: product-id }) (err u404))))
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (if (<= (- (get expiry-date product) burn-block-height)
                (var-get expiry-warning-blocks)
            )
            (create-alert product-id "EXPIRY" "MEDIUM"
                "Product approaching expiry date"
            )
            (ok u0)
        )
    )
)

(define-public (resolve-alert (alert-id uint))
    (let ((alert (unwrap! (map-get? system-alerts { alert-id: alert-id }) (err u404))))
        (asserts!
            (or
                (is-eq tx-sender (var-get admin))
                (is-eq tx-sender (get created-by alert))
            )
            (err u403)
        )
        (map-set system-alerts { alert-id: alert-id }
            (merge alert { is-resolved: true })
        )
        (ok true)
    )
)

(define-public (mark-alert-read (alert-id uint))
    (begin
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (is-some (map-get? system-alerts { alert-id: alert-id }))
            (err u404)
        )
        (map-set alert-recipients {
            alert-id: alert-id,
            recipient: tx-sender,
        } {
            is-read: true,
            read-timestamp: burn-block-height,
        })
        (ok true)
    )
)

(define-public (set-temperature-thresholds
        (min-temp int)
        (max-temp int)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) (err u403))
        (asserts! (< min-temp max-temp) (err u400))
        (var-set temperature-threshold-min min-temp)
        (var-set temperature-threshold-max max-temp)
        (ok true)
    )
)

(define-private (distribute-alert
        (alert-id uint)
        (alert-type (string-ascii 20))
    )
    (begin
        (ok true)
    )
)

(define-read-only (get-alert (alert-id uint))
    (map-get? system-alerts { alert-id: alert-id })
)

(define-read-only (get-alert-subscription (subscriber principal))
    (map-get? alert-subscriptions { subscriber: subscriber })
)

(define-read-only (get-alert-status
        (alert-id uint)
        (recipient principal)
    )
    (map-get? alert-recipients {
        alert-id: alert-id,
        recipient: recipient,
    })
)

(define-read-only (get-temperature-thresholds)
    {
        min: (var-get temperature-threshold-min),
        max: (var-get temperature-threshold-max),
        expiry-warning-blocks: (var-get expiry-warning-blocks),
    }
)

(define-read-only (get-unresolved-alerts-count)
    (var-get alert-counter)
)
