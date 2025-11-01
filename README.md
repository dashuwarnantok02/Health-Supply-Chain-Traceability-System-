# 🏥 Health Supply Chain Traceability System

## 📋 Overview

This smart contract provides a secure and transparent way to track medical supplies, vaccines, and equipment throughout the supply chain. Built on Stacks blockchain using Clarity, it helps prevent counterfeiting, theft, and ensures proper handling of sensitive medical products.

## ✨ Features

- 🔒 Secure product registration with unique identifiers
- 👥 Verified custodian management system
- 📦 Complete chain-of-custody tracking
- 🌡️ Environmental condition monitoring (temperature)
- 📍 Location tracking throughout the supply chain
- 📜 Immutable history of all product movements and events

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Basic knowledge of Clarity and Stacks blockchain

### Installation

1. Create a new Clarinet project:
   ```bash
   clarinet new health-supply-chain
   ```

2. Replace the default contract with the Health-Supply-Chain-System.clar file
3. Deploy and test the contract:
   ```bash
   clarinet console
   ```

## 📖 Usage Guide

### Admin Functions

```clarity
;; Set a new admin
(contract-call? .health-supply-chain-system set-admin 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)

;; Verify a custodian
(contract-call? .health-supply-chain-system verify-custodian 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

### Custodian Management

```clarity
;; Register as a custodian
(contract-call? .health-supply-chain-system register-custodian "General Hospital Pharmacy" "PHARMACY")

;; Register a location
(contract-call? .health-supply-chain-system register-location "loc123" "Main Warehouse" "123 Medical Blvd, City" "WAREHOUSE")
```

### Product Lifecycle

```clarity
;; Register a new product
(contract-call? .health-supply-chain-system register-product "prod123" "COVID-19 Vaccine" u1612137600 u1643673600 "LOT-2021-02")

;; Transfer a product to a new custodian
(contract-call? .health-supply-chain-system transfer-product "prod123" 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 "Hospital Pharmacy" i2)

;; Record a custom event for a product
(contract-call? .health-supply-chain-system record-product-event-with-action "prod123" 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7 "Cold Storage" i-5 "STORED")

;; Deactivate a product (end of life)
(contract-call? .health-supply-chain-system deactivate-product "prod123")
```

### Querying Information

```clarity
;; Get product details
(contract-call? .health-supply-chain-system get-product "prod123")

;; Get product history entry
(contract-call? .health-supply-chain-system get-product-history "prod123" u0)

;; Check if a custodian is verified
(contract-call? .health-supply-chain-system is-custodian-verified 'SP2J6ZY48GV1EZ5V2V5RB9MP66SW86PYKKNRV9EJ7)
```

## 🔐 Security Considerations

- Only verified custodians can register and transfer products
- Products can only be transferred by their current custodian
- Admin privileges are required for certain sensitive operations
- All transactions are permanently recorded on the blockchain

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

