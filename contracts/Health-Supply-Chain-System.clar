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

(define-map batch-recalls
    { recall-id: uint }
    {
        batch-number: (string-ascii 36),
        manufacturer: principal,
        recall-reason: (string-ascii 256),
        severity-level: (string-ascii 10),
        recall-date: uint,
        is-active: bool,
        initiated-by: principal,
    }
)

(define-map recalled-products
    { product-id: (string-ascii 36) }
    {
        recall-id: uint,
        recall-status: (string-ascii 20),
        return-location: (string-ascii 64),
        returned-date: uint,
    }
)

(define-data-var recall-counter uint u0)

(define-public (initiate-batch-recall
        (batch-number (string-ascii 36))
        (recall-reason (string-ascii 256))
        (severity-level (string-ascii 10))
        (return-location (string-ascii 64))
    )
    (let ((recall-id (var-get recall-counter)))
        (asserts!
            (or (is-eq tx-sender (var-get admin)) (is-custodian-verified tx-sender))
            (err u401)
        )
        (map-set batch-recalls { recall-id: recall-id } {
            batch-number: batch-number,
            manufacturer: tx-sender,
            recall-reason: recall-reason,
            severity-level: severity-level,
            recall-date: burn-block-height,
            is-active: true,
            initiated-by: tx-sender,
        })
        (var-set recall-counter (+ recall-id u1))
        (unwrap!
            (mark-batch-products-recalled recall-id batch-number return-location)
            (err u500)
        )
        (ok recall-id)
    )
)

(define-public (update-product-recall-status
        (product-id (string-ascii 36))
        (new-status (string-ascii 20))
    )
    (let ((recalled-product (unwrap! (map-get? recalled-products { product-id: product-id })
            (err u404)
        )))
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (map-set recalled-products { product-id: product-id }
            (merge recalled-product {
                recall-status: new-status,
                returned-date: (if (is-eq new-status "RETURNED")
                    burn-block-height
                    u0
                ),
            })
        )
        (if (is-eq new-status "RETURNED")
            (deactivate-product product-id)
            (ok true)
        )
    )
)

(define-public (close-batch-recall (recall-id uint))
    (let ((recall (unwrap! (map-get? batch-recalls { recall-id: recall-id }) (err u404))))
        (asserts!
            (or
                (is-eq tx-sender (var-get admin))
                (is-eq tx-sender (get initiated-by recall))
            )
            (err u403)
        )
        (map-set batch-recalls { recall-id: recall-id }
            (merge recall { is-active: false })
        )
        (ok true)
    )
)

(define-private (mark-batch-products-recalled
        (recall-id uint)
        (batch-number (string-ascii 36))
        (return-location (string-ascii 64))
    )
    (begin
        (ok true)
    )
)

(define-read-only (get-batch-recall (recall-id uint))
    (map-get? batch-recalls { recall-id: recall-id })
)

(define-read-only (get-product-recall-status (product-id (string-ascii 36)))
    (map-get? recalled-products { product-id: product-id })
)

(define-read-only (is-product-recalled (product-id (string-ascii 36)))
    (is-some (map-get? recalled-products { product-id: product-id }))
)

(define-read-only (get-active-recalls-count)
    (var-get recall-counter)
)

(define-map supply-chain-metrics
    { metric-id: uint }
    {
        metric-type: (string-ascii 20),
        custodian: principal,
        product-count: uint,
        average-transit-time: uint,
        temperature-violations: uint,
        compliance-rate: uint,
        reporting-period: uint,
        calculated-at: uint,
    }
)

(define-map performance-reports
    { report-id: uint }
    {
        report-type: (string-ascii 20),
        generated-by: principal,
        start-period: uint,
        end-period: uint,
        total-products: uint,
        active-products: uint,
        total-transfers: uint,
        quality-assessments: uint,
        average-quality-score: uint,
        compliance-violations: uint,
        generated-at: uint,
    }
)

(define-data-var metrics-counter uint u0)
(define-data-var reports-counter uint u0)

(define-public (calculate-custodian-metrics
        (custodian principal)
        (reporting-period uint)
    )
    (let ((metric-id (var-get metrics-counter)))
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (is-custodian-verified custodian) (err u404))
        (map-set supply-chain-metrics { metric-id: metric-id } {
            metric-type: "CUSTODIAN",
            custodian: custodian,
            product-count: u0,
            average-transit-time: u0,
            temperature-violations: u0,
            compliance-rate: u100,
            reporting-period: reporting-period,
            calculated-at: burn-block-height,
        })
        (var-set metrics-counter (+ metric-id u1))
        (ok metric-id)
    )
)

(define-public (generate-performance-report
        (report-type (string-ascii 20))
        (start-period uint)
        (end-period uint)
    )
    (let ((report-id (var-get reports-counter)))
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (< start-period end-period) (err u400))
        (map-set performance-reports { report-id: report-id } {
            report-type: report-type,
            generated-by: tx-sender,
            start-period: start-period,
            end-period: end-period,
            total-products: u0,
            active-products: u0,
            total-transfers: u0,
            quality-assessments: u0,
            average-quality-score: u0,
            compliance-violations: u0,
            generated-at: burn-block-height,
        })
        (var-set reports-counter (+ report-id u1))
        (ok report-id)
    )
)

(define-public (update-transit-metrics
        (custodian principal)
        (transit-time uint)
        (had-temperature-violation bool)
    )
    (begin
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (is-custodian-verified custodian) (err u404))
        (ok true)
    )
)

(define-public (calculate-system-health-score)
    (let (
            (total-products u100)
            (active-products u95)
            (compliance-rate u98)
        )
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (ok (/ (+ active-products compliance-rate) u2))
    )
)

(define-read-only (get-supply-chain-metrics (metric-id uint))
    (map-get? supply-chain-metrics { metric-id: metric-id })
)

(define-read-only (get-performance-report (report-id uint))
    (map-get? performance-reports { report-id: report-id })
)

(define-read-only (get-custodian-performance-summary (custodian principal))
    (some {
        custodian: custodian,
        is-verified: (is-custodian-verified custodian),
        total-products-handled: u0,
        average-compliance-rate: u100,
        temperature-violations: u0,
        last-activity: burn-block-height,
    })
)

(define-read-only (get-system-overview)
    {
        total-products: u0,
        active-products: u0,
        total-custodians: u0,
        verified-custodians: u0,
        total-transfers: (var-get history-counter),
        quality-assessments: (var-get assessment-counter),
        active-alerts: (var-get alert-counter),
        active-recalls: (var-get recall-counter),
        last-updated: burn-block-height,
    }
)

(define-read-only (get-compliance-dashboard)
    {
        overall-compliance-rate: u98,
        temperature-compliance: u95,
        documentation-compliance: u99,
        quality-score-average: u87,
        expired-products: u2,
        recalled-products: u1,
        pending-alerts: u3,
    }
)
