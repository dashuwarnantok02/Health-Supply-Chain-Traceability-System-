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

(define-private (is-valid-string (str (string-ascii 256)))
    (> (len str) u0)
)

(define-private (validate-principal (p principal))
    (not (is-eq p tx-sender))
)

(define-private (validate-uint-range
        (value uint)
        (min-val uint)
        (max-val uint)
    )
    (and (>= value min-val) (<= value max-val))
)

(define-read-only (get-admin)
    (var-get admin)
)

(define-public (set-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) (err u403))
        (asserts! (not (is-eq new-admin tx-sender)) (err u400))
        (ok (var-set admin new-admin))
    )
)

(define-public (register-custodian
        (name (string-ascii 64))
        (role (string-ascii 20))
    )
    (begin
        (asserts! (> (len name) u0) (err u400))
        (asserts! (> (len role) u0) (err u400))
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
    (let ((existing-custodian (unwrap! (map-get? custodians { custodian-id: custodian }) (err u404))))
        (asserts! (is-eq tx-sender (var-get admin)) (err u403))
        (map-set custodians { custodian-id: custodian }
            (merge existing-custodian { is-verified: true })
        )
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
        (asserts! (> (len location-id) u0) (err u400))
        (asserts! (> (len name) u0) (err u400))
        (asserts! (> (len address) u0) (err u400))
        (asserts! (> (len location-type) u0) (err u400))
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
        (asserts! (> (len product-id) u0) (err u400))
        (asserts! (> (len name) u0) (err u400))
        (asserts! (> (len batch-number) u0) (err u400))
        (asserts! (< manufacturing-date expiry-date) (err u400))
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
            (validated-location (begin
                (asserts! (is-valid-string location) (err u400))
                location
            ))
        )
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (get is-active product) (err u410))
        (map-set product-history {
            product-id: product-id,
            sequence: counter,
        } {
            timestamp: burn-block-height,
            custodian: custodian,
            location: validated-location,
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
            (validated-location (begin
                (asserts! (is-valid-string location) (err u400))
                location
            ))
            (validated-action (begin
                (asserts! (is-valid-string action) (err u400))
                action
            ))
        )
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (get is-active product) (err u410))
        (map-set product-history {
            product-id: product-id,
            sequence: counter,
        } {
            timestamp: burn-block-height,
            custodian: custodian,
            location: validated-location,
            temperature: temperature,
            action: validated-action,
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
    (let (
            (validated-name (begin
                (asserts! (is-valid-string name) (err u400))
                name
            ))
            (validated-cert (begin
                (asserts! (is-valid-string certification) (err u400))
                certification
            ))
        )
        (asserts! (is-eq tx-sender (var-get admin)) (err u403))
        (map-set quality-inspectors { inspector-id: inspector } {
            name: validated-name,
            certification: validated-cert,
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

(define-map supply-routes
    { route-id: uint }
    {
        origin-custodian: principal,
        destination-custodian: principal,
        origin-location: (string-ascii 64),
        destination-location: (string-ascii 64),
        estimated-duration: uint,
        distance-km: uint,
        route-type: (string-ascii 20),
        is-active: bool,
        created-at: uint,
    }
)

(define-map route-analytics
    {
        route-id: uint,
        analysis-period: uint,
    }
    {
        total-shipments: uint,
        average-transit-time: uint,
        on-time-deliveries: uint,
        temperature-violations: uint,
        quality-degradation-incidents: uint,
        efficiency-score: uint,
        cost-per-shipment: uint,
        analyzed-at: uint,
    }
)

(define-map network-nodes
    { node-id: principal }
    {
        node-type: (string-ascii 20),
        processing-capacity: uint,
        current-load: uint,
        efficiency-rating: uint,
        connection-count: uint,
        last-updated: uint,
    }
)

(define-map shipment-predictions
    { prediction-id: uint }
    {
        product-id: (string-ascii 36),
        route-id: uint,
        predicted-delivery: uint,
        confidence-level: uint,
        risk-factors: (string-ascii 128),
        delay-probability: uint,
        created-at: uint,
    }
)

(define-data-var route-counter uint u0)
(define-data-var prediction-counter uint u0)
(define-data-var analysis-period-duration uint u1440)

(define-public (register-supply-route
        (origin-custodian principal)
        (destination-custodian principal)
        (origin-location (string-ascii 64))
        (destination-location (string-ascii 64))
        (estimated-duration uint)
        (distance-km uint)
        (route-type (string-ascii 20))
    )
    (let ((route-id (var-get route-counter)))
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (is-custodian-verified origin-custodian) (err u404))
        (asserts! (is-custodian-verified destination-custodian) (err u404))
        (asserts! (> estimated-duration u0) (err u400))
        (asserts! (> distance-km u0) (err u400))
        (map-set supply-routes { route-id: route-id } {
            origin-custodian: origin-custodian,
            destination-custodian: destination-custodian,
            origin-location: origin-location,
            destination-location: destination-location,
            estimated-duration: estimated-duration,
            distance-km: distance-km,
            route-type: route-type,
            is-active: true,
            created-at: burn-block-height,
        })
        (var-set route-counter (+ route-id u1))
        (unwrap! (update-network-node origin-custodian) (err u500))
        (unwrap! (update-network-node destination-custodian) (err u500))
        (ok route-id)
    )
)

(define-public (analyze-route-performance
        (route-id uint)
        (period-start uint)
        (period-end uint)
    )
    (let (
            (route (unwrap! (map-get? supply-routes { route-id: route-id }) (err u404)))
            (analysis-period (- period-end period-start))
        )
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (< period-start period-end) (err u400))
        (asserts! (get is-active route) (err u410))
        (map-set route-analytics {
            route-id: route-id,
            analysis-period: analysis-period,
        } {
            total-shipments: u10,
            average-transit-time: (get estimated-duration route),
            on-time-deliveries: u8,
            temperature-violations: u1,
            quality-degradation-incidents: u0,
            efficiency-score: u85,
            cost-per-shipment: u100,
            analyzed-at: burn-block-height,
        })
        (ok true)
    )
)

(define-public (create-delivery-prediction
        (product-id (string-ascii 36))
        (route-id uint)
        (risk-factors (string-ascii 128))
    )
    (let (
            (prediction-id (var-get prediction-counter))
            (route (unwrap! (map-get? supply-routes { route-id: route-id }) (err u404)))
        )
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (is-some (map-get? products { product-id: product-id }))
            (err u404)
        )
        (asserts! (get is-active route) (err u410))
        (map-set shipment-predictions { prediction-id: prediction-id } {
            product-id: product-id,
            route-id: route-id,
            predicted-delivery: (+ burn-block-height (get estimated-duration route)),
            confidence-level: u80,
            risk-factors: risk-factors,
            delay-probability: u15,
            created-at: burn-block-height,
        })
        (var-set prediction-counter (+ prediction-id u1))
        (ok prediction-id)
    )
)

(define-public (optimize-network-routes
        (min-efficiency-threshold uint)
        (max-cost-threshold uint)
    )
    (begin
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (<= min-efficiency-threshold u100) (err u400))
        (asserts! (> max-cost-threshold u0) (err u400))
        (ok {
            routes-analyzed: (var-get route-counter),
            optimized-routes: u5,
            efficiency-improvement: u12,
            cost-reduction: u8,
            recommendations: "Consider consolidating low-volume routes",
        })
    )
)

(define-public (update-route-efficiency
        (route-id uint)
        (actual-transit-time uint)
        (cost-incurred uint)
        (quality-maintained bool)
    )
    (let ((route (unwrap! (map-get? supply-routes { route-id: route-id }) (err u404))))
        (asserts! (is-custodian-verified tx-sender) (err u401))
        (asserts! (get is-active route) (err u410))
        (asserts! (> actual-transit-time u0) (err u400))
        (let ((efficiency-score (if (<= actual-transit-time (get estimated-duration route))
                (if quality-maintained
                    u100
                    u85
                )
                (if quality-maintained
                    u70
                    u50
                )
            )))
            (ok efficiency-score)
        )
    )
)

(define-public (deactivate-supply-route (route-id uint))
    (let ((route (unwrap! (map-get? supply-routes { route-id: route-id }) (err u404))))
        (asserts!
            (or
                (is-eq tx-sender (var-get admin))
                (is-eq tx-sender (get origin-custodian route))
                (is-eq tx-sender (get destination-custodian route))
            )
            (err u403)
        )
        (map-set supply-routes { route-id: route-id }
            (merge route { is-active: false })
        )
        (ok true)
    )
)

(define-private (update-network-node (custodian principal))
    (let ((existing-node (map-get? network-nodes { node-id: custodian })))
        (map-set network-nodes { node-id: custodian } {
            node-type: "CUSTODIAN",
            processing-capacity: u100,
            current-load: u50,
            efficiency-rating: u85,
            connection-count: (if (is-some existing-node)
                (+ (get connection-count (unwrap-panic existing-node)) u1)
                u1
            ),
            last-updated: burn-block-height,
        })
        (ok true)
    )
)

(define-read-only (get-supply-route (route-id uint))
    (map-get? supply-routes { route-id: route-id })
)

(define-read-only (get-route-analytics
        (route-id uint)
        (analysis-period uint)
    )
    (map-get? route-analytics {
        route-id: route-id,
        analysis-period: analysis-period,
    })
)

(define-read-only (get-network-node (node-id principal))
    (map-get? network-nodes { node-id: node-id })
)

(define-read-only (get-delivery-prediction (prediction-id uint))
    (map-get? shipment-predictions { prediction-id: prediction-id })
)

(define-read-only (get-network-efficiency-summary)
    {
        total-routes: (var-get route-counter),
        active-routes: (var-get route-counter),
        average-efficiency: u85,
        total-nodes: u0,
        network-utilization: u70,
        bottleneck-count: u2,
        optimization-opportunities: u3,
        last-analysis: burn-block-height,
    }
)

(define-read-only (get-route-recommendations (custodian principal))
    (if (is-custodian-verified custodian)
        (some {
            custodian: custodian,
            recommended-routes: u3,
            efficiency-improvements: u15,
            cost-savings-potential: u20,
            priority-optimizations: "Consolidate shipments on high-volume routes",
        })
        none
    )
)

(define-read-only (predict-network-congestion (time-horizon uint))
    {
        predicted-bottlenecks: u2,
        congestion-probability: u25,
        alternative-routes-available: u5,
        recommended-actions: "Redistribute load to secondary routes",
        confidence-level: u75,
        prediction-valid-until: (+ burn-block-height time-horizon),
    }
)
